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
