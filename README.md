# apps
Shiny applications

## Two instances of the same app (public + preview)

`scores` and `species` each run **twice** on the server, from this one codebase:

| instance | served at | version comes from | may render |
|---|---|---|---|
| public | `app.marinesensitivity.org/scores/` | `?ver=` | `access: public` releases only |
| preview | `preview.marinesensitivity.org/v8/scores/` | the URL **path**, handed over by Caddy as `X-MS-Version` | everything, incl. `restricted` pre-releases |

The preview instance is a second Shiny Server block whose 3-line wrapper sets `MS_PREVIEW=1`
(`server/rstudio/shiny_apps_preview/`), and `msens::atlas_allow_access()` reads that env var — the
policy is a property of the PROCESS, never of a request header, because shiny-server opens its own
websocket to the R worker and no proxy header survives it. `ui(req)` resolves the version once and
embeds `msens::ver_token_sign()`, so the session renders the version its page was served for even
though `url_search` is client-supplied.

Reviewer access is per version; see `server/cloudflare/README.md`.

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
