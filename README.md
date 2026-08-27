# apps
Shiny applications

## Two instances of the same app (public + preview)

`scores` and `species` each run **twice** on the server, from this one codebase:

| instance | served at | may render |
|---|---|---|
| public | `app.marinesensitivity.org/v7/scores/` | `access: public` releases only |
| preview | `preview.marinesensitivity.org/v8/scores/` | everything, incl. `restricted` pre-releases |

**The version is the URL path on both hosts** (since 2026-08-27). Caddy strips the prefix and hands
it to the app as `X-MS-Version` — a header the server sets, so a client cannot forge it and no
`?ver=` need duplicate it in a shared link. The old spelling still resolves: `/scores/?ver=v7` 301s
to `/v7/scores/`, as do the retired per-version instances (`/mapgl_v1`, `/mapsp`, …). A bare
`/scores/` resolves the promoted release and then canonicalises its own URL.

The preview instance is a second Shiny Server block whose 3-line wrapper sets `MS_PREVIEW=1`
(`server/rstudio/shiny_apps_preview/`), and `msens::atlas_allow_access()` reads that env var — the
policy is a property of the PROCESS, never of a request header, because shiny-server opens its own
websocket to the R worker and no proxy header survives it. `ui(req)` resolves the version once and
embeds `msens::ver_token_sign()`, so the session renders the version its page was served for even
though `url_search` is client-supplied.

Reviewer access is per version; see `server/cloudflare/README.md`.

## Mobile layout (2026-08-27)

Both apps are bslib `page_sidebar()` pages, and two bslib defaults made them unusable on a
phone: the page is **not fillable on mobile** (`fillable_mobile = FALSE` adds
`.bslib-flow-mobile`, which turns every fill item into `flex: 0 0 auto`, so the map card kept its
intrinsic ~0 height), and `sidebar(open = NULL)` resolves to `mobile = "always"` — the sidebar
stacked *below* the (empty) map with no toggle. Both apps now pass `fillable_mobile = TRUE` and
`open = list(desktop = "open", mobile = "closed")` (species already had `open = FALSE`), so under
bslib's own 575.98px breakpoint the sidebar becomes its toggle row + a full-width overlay on the
map, and the map takes the viewport. Nothing is hand-rolled — it is bslib's mobile mode, plus CSS
in each `ui_impl` under `@media (max-width: 575.98px)`:

- header (`.ms-header`): title + About + dark toggle on one row, the product nav on a second;
- spacing: `--bslib-spacer` (page padding) **and** `--bslib-mb-spacer` (the 1.5rem gap
  `.bslib-gap-spacing` puts between the main column's children — a different variable, measured)
  to 0.5rem, card bodies to 0.5rem;
- scores: `tab_title("Plot of Scores", "Plot")` renders both a long and a short label and CSS
  picks one, so the tab strip stays one row (`value` is untouched — the tour keys on it);
- species: the pickers drop their labels, the layer bar's pills collapse behind a
  "N layers ▾" toggle (`.layer-bar.expanded`, a client-side class flip; a re-render collapses it
  again), and "Zoom to layer" moves under the geocoder box, which goes full-width on narrow maps.

Verified locally at 400px and 360px in Chrome (both apps, sidebar overlay, Plot/Table tabs, layer
toggle) and unchanged at desktop width. Local dev note: the app prefers
`~/_big/msens/derived/{ver}/serve.duckdb` when present, and that file is the SERVER's view DB
(views over `/share/data/big/...`), so a laptop run fails on it — run under a HOME whose
`_big/msens/derived/{ver}` omits `serve.duckdb` (symlink the rest) to fall back to `sdm.duckdb`.

## Cold-start performance (measured 2026-08-27, v8)

A page load on an idle app was ~15 s of time-to-first-byte; the whole client side is 79 requests
and ~150 KB, so it is all server-side worker startup. `tools/profile_cold_start.R` breaks it down:

| phase | scores | species |
|---|---|---|
| attaching packages | 8.6 s | 8.3 s |
| other globals | 1.0 s | — |
| `build_bundle(v8)` | 3.2 s | 8.8 s |
| `build_bundle(v7)` after it | 1.0 s | — |

**Packages dominate and cannot be trimmed away**: `library(msens)` takes 8.1 s loaded first but
only 0.38 s loaded last — its weight is the shared sf/terra/mapgl/duckdb stack the apps attach
regardless. Moving msens Imports to Suggests would buy ~0.4 s.

**The fix in place is to not pay it**: `app_idle_timeout 3600`
(`server/rstudio/shiny-server.conf`) keeps a worker alive an hour past its last session, so only
the first visit after an idle hour is cold (~0.7 s otherwise).

**Caching a bundle per version was evaluated and deferred.** It would work — a bundle is
serialisable apart from the DuckDB connection (and species' one lazy `tbl`) — and reading beats
rebuilding: scores 3.27 s → 0.31 s (56 MB uncompressed), species 8.80 s → 3.03 s (11 MB gzip).
But it saves 3–6 s of a start that is still ~10 s because of packages, it only helps the rare
cold visit now that workers stay warm, and it introduces a cache that must be invalidated on the
release's data, the app code AND the msens version — a surface that can silently serve stale
metadata under a version label, which is the exact failure class this project works hardest to
avoid. Revisit if cold starts become common again (e.g. many concurrent versions evicting
workers), and start with species' 8.8 s / 222 MB bundle, which is the outlier worth understanding
before caching it.
