# Usage analytics

How usage is tracked across the MarineSensitivity products, what is collected, and the
one-time setup for the usage-log Sheet.

## Two channels, and why

| | GA4 (gtag) | Usage-log Sheet |
|---|---|---|
| **Holds** | page views, tab / layer / palette selections, downloads, outbound clicks | the same events **plus** full-cardinality detail: species names, typed search strings, report parameters, errors |
| **Why** | free, aggregate, cross-product journeys in one property | GA4 buckets high-cardinality dimensions into `(other)` past its daily limit — with ~16k taxa, species-level detail would be unusable there |
| **Read by** | GA4 UI, property **413466008** | anyone with the Sheet — filterable and chartable without SQL |

Both are emitted by the same call, `window.msTrack(event, params)`, installed by
`msens::ga_js()`. **Neither costs the Shiny server a network request** — see below.

## One measurement ID, not one per product

`G-9HW6L751XG` is used by every product. gtag scopes the `_ga` cookie to the registrable
domain, so a single stream already spans `marinesensitivity.org` and
`app.marinesensitivity.org`; per-product IDs would fragment sessions and make journeys
(homepage → docs → app) unrecoverable. Products are separated in reporting by
`content_group`:

| content_group | Product |
|---|---|
| `home` | marinesensitivity.org (`MarineSensitivity.github.io`) |
| `docs` | marinesensitivity.org/docs |
| `workflows` | marinesensitivity.org/workflows |
| `msens` | marinesensitivity.org/msens (pkgdown) |
| `apps-landing` | the app index page |
| `scores` / `species` | the two Shiny apps |

## Why the app never blocks on logging

The obvious design — POST each event from R — costs a synchronous HTTP round trip on the
reactive thread. An Apps Script `/exec` endpoint takes ~300 ms–2 s and 302-redirects, so
every species selection would visibly stall.

Instead **the browser sends both legs**:

- UI interactions are tracked client-side and never reach R at all.
- Server-side facts (the scientific name behind a picker value, a report's parameters, a
  row count) go out via `msens::ms_track(session, ...)`, which pushes one small message
  over the websocket the session already has open. No HTTP, no background worker, no
  flush queue in R. Errors are swallowed, so instrumentation cannot take down an app.
- The browser batches: flush at 10 queued events, every 15 s, and on page-hide
  (`navigator.sendBeacon` survives unload). That keeps the Apps Script execution quota
  flat regardless of how fast users click.

Two non-obvious details, both load-bearing:

- The beacon body is `text/plain`, which keeps it a CORS **simple** request. Apps Script
  `/exec` does not answer an `OPTIONS` preflight, so an `application/json` body would be
  silently dropped.
- Empty R parameter lists serialise as `[]`, not `{}`; the client normalises them.

## Events

**scores** — `select_tab`, `select_subregion`, `select_unit`, `select_layer`,
`select_palette`, `open_about`, `start_tour`, `open_table_info`, `report_add_area`,
`download_species_csv` (with `n_rows`, `area`, and the layer context),
`report_submit` / `report_result` (with `rpt_ver`, `format`, `n_areas`, duration, outcome).

`report_result` matters disproportionately: the report file itself is fetched from
`file.marinesensitivity.org`, a different host serving `Content-Disposition: attachment`
with no JS — the app is the **only** place a generated report can be counted.

**species** — `search_species` (the typed query), `select_species` (resolved
`scientific_name`, `common_name`, `sp_cat`, `taxon_id`, `n_datasets`, `redlist_code`),
`select_layer`, `select_representation`, `select_outlines`, `toggle_us_only`,
`toggle_obis`, `zoom_to_layer`, `deeplink_mdl_key` (with its resolution outcome, so dead
`?mdl_key=` links surface as a countable `not_found`).

### Did a search find anything?

`search_species` deliberately logs **only the query text**. Selectize's result count is
not usable here: with `server = TRUE`, a zero-match query leaves the previously loaded
options in place, while a query that *does* match makes the server replace the option set
— both end up with `currentResults.total == options.length`, so hits and misses are
indistinguishable (verified in-browser: `Balaenoptera`, which has many real matches, was
indistinguishable from a nonsense string).

Recover it at analysis time instead, more reliably, either way:

- join the logged query against the taxon list, or
- treat a `search_species` with no following `select_species` in the same session as an
  unsuccessful search.

## One-time setup for the Sheet

1. **Create a Google Sheet.** Make the first row exactly `msens::ms_log_header()`:

   ```
   timestamp | app | app_version | client_id | session_id | event | params | page | referrer | user_agent
   ```

2. **Add the Apps Script.** Extensions → Apps Script, paste [`Code.gs`](Code.gs), then
   Deploy → New deployment → type **Web app**, execute as **Me**, who has access
   **Anyone**. Copy the `/exec` URL.

   `Code.gs` is generated from `msens::ms_apps_script()` — regenerate rather than
   hand-editing, so the columns cannot drift from `ms_log_header()`:

   ```r
   Rscript -e 'writeLines(msens::ms_apps_script(), "analytics/Code.gs")'
   ```

3. **Point the apps at it.** Set `MSENS_LOG_URL` to the `/exec` URL in the server `.env`
   (next to `PASSWORD`); `docker-compose.yml` passes it into the rstudio/shiny container.
   Then redeploy the apps. **Unset ⇒ the Sheet leg is a silent no-op** and only GA4
   receives events, which is what local development should do.

   The endpoint URL is the only secret involved — the apps hold no credential — so keep it
   in `.env` (untracked), never in app source.

4. **Confirm.** Open an app, select a species, wait ~15 s (or switch tabs, which forces a
   flush), and a row should appear in the Sheet.

### Retention

The Sheet grows one row per interaction and Google Sheets caps at 10M cells (~1M rows
here). Archive to a new tab/file periodically, or switch this channel to GA4's free
BigQuery export if the volume ever justifies it.
