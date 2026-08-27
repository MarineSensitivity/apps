# TODO:
# - [ ] on click, add marker (rast) or highlight (pa)
# - [use new features mapgl 0.3](https://walker-data.com/scores/news/index.html#mapgl-03)
#   - [ ] on hover, show rast value using `enable_shiny_hover()`
#   - [ ] legend pretty, more compact with `legend_style()`
# - [ ] on light theme, change basemap to light too
# - [ ] Species table: rename columns and add explanation with info popups
# - [ ] Disconnect db connections when Shiny closes
# - [ ] later manage multiple species per model; assume 1 taxa to 1 species for now
# - [ ] Generate metrics raster
# - [ ] Add Download button for cell (tif), pa (gpkg), er (gpkg) with README.md, csv lookup table as zip (ea.)

# packages ----
librarian::shelf(
  bsicons,
  bslib,
  etiennebacher / conductor,
  DBI,
  dbplyr,
  digest,
  dplyr,
  duckdb,
  DT,
  fs,
  future,
  ggiraph,
  ggplot2,
  glue,
  here,
  httr2,
  MarineSensitivity/msens,
  yogevherz / plotme,
  plotly,
  promises,
  purrr,
  RColorBrewer,
  readr,
  scales,
  sf,
  shiny,
  stringr,
  terra,
  tibble,
  tidyr,
  viridisLite,
  quiet = T
)
options(readr.show_col_types = F)

# async backend for background `/report` POSTs so long renders don't
# block the Shiny session. Workers are shared across sessions.
future::plan(future::multisession, workers = 2)

# profile performance of app:
#   profvis::profvis(shiny::runApp(here::here("scores")))

# variables ----
verbose <- interactive()

# version ----
# Default only: the version actually rendered is resolved per request from
# ?ver= (see ver_of / bundle at the bottom). Kept as the last-resort fallback
# for when the published registry cannot be reached at all.
ver_fallback <- "v8"
ver <- ver_fallback

# APP_VERSION — the deployed commit, stamped on every logged event so a Sheet
# row ties back to the exact code that produced it (falls back to `ver`).
#
# `-c safe.directory=*` is required, not optional: shiny-server runs the app as
# `shiny` while the deployed clone is owned by the user who pulled it, and git
# refuses to read a repo it considers "dubious ownership" — which would silently
# drop every logged app_version to the fallback.
APP_VERSION <- local({
  sha <- tryCatch(
    suppressWarnings(system2(
      "git", c("-c", "safe.directory=*", "-C", shQuote(here()),
               "rev-parse", "--short", "HEAD"),
      stdout = TRUE, stderr = FALSE))[1],
    error = function(e) NA_character_)
  if (!is.null(sha) && !is.na(sha) && nzchar(sha)) sha else ver
})

is_server <- Sys.info()[["sysname"]] == "Linux"
dir_private <- ifelse(
  is_server,
  "/share/private",
  "~/My Drive/private"
)
dir_data <- ifelse(
  is_server,
  "/share/data",
  "~/My Drive/projects/msens/data"
)
# mapgl declares every optional bundle in its widget definition, so the PAGE
# carries them whether or not the app calls them -- and it is the UI's
# maplibreOutput() that emits them, not the server-side widget, so filtering the
# rendered object achieves nothing. Measured cold: 3.7 MB of JavaScript across 39
# files, of which html2canvas (194 KB) and the globe minimap (149 KB) are never
# touched -- `add_globe_minimap` and `screenshot` appear zero times here.
#
# This was once reverted on a false alarm: I judged the map "never initialises"
# from a screenshot taken before it had painted (it needs ~60 s under browser
# automation) and a log grep that cannot see tile requests. The browser's own
# resource timing showed titiler and basemap tiles arriving with zero failures
# the whole time. Re-landed after watching the map actually draw.
#
# Conservative by construction: an unknown name simply does not match, and every
# bundle the app uses stays. If a feature is added that needs one of these, drop
# it from this list rather than working around it.
UNUSED_MAP_DEPS <- c("html2canvas", "mapbox-gl-globe-minimap")
map_output <- function(...) {
  x <- maplibreOutput(...)
  d <- htmltools::findDependencies(x)
  htmltools::attachDependencies(
    x, Filter(function(dep) !(dep$name %in% UNUSED_MAP_DEPS), d))
}

# ---- per-version bundle -----------------------------------------------------
#
# Everything below depends on WHICH release is being rendered. It used to run
# once at startup against a hardcoded `ver`, which is exactly why the app had to
# be forked per release. It is now a function of `ver`, memoised per version, so
# one process can serve v1 and v8 side by side.
#
# The UI and server are re-enclosed in the bundle environment (see the bottom of
# this file) rather than having every reference rewritten: `con_sdm`, `d_lyrs`,
# `layer_tiles`, `pra_full_sf` and the rest keep their names and resolve to the
# requested version, falling through to the globals above for anything shared.
.bundles <- new.env(parent = emptyenv())

build_bundle <- function(ver) {

  dir_v <- glue("{dir_data}/derived/{ver}")
  dir_big <- ifelse(
    is_server,
    glue("/share/data/big/{ver}"),
    glue("~/_big/msens/derived/{ver}")
  )
  is_prod <- Sys.getenv("MSENS_ENV") == "prod"
  pmtiles_base_url <- ifelse(
    is_prod,
    "/pmtiles",
    "https://file.marinesensitivity.org/pmtiles")

  mapbox_tkn_txt <- glue("{dir_private}/mapbox_token_bdbest.txt")
  # PER GRID, not a constant: usa05 (v1-v7) and global05 (v8) index cell ids
  # differently, so reading the global lookup for a v7 click returns the wrong
  # cell -- or none, which surfaced as "[subset] invalid name(s)" from extract().
  # The grid registry knows which raster belongs to which release.
  cell_tif <- glue("{dir_data}/derived/{msens::grid_registry()$cellid_tif[msens::grid_registry()$grid_id == msens::grid_for_ver(ver)][1]}")
  # server reads the S3-backed view DB (serve.duckdb, views over marine-atlas Parquet — shared
  # with titiler-v8, no multi-GB rsync); local dev uses the full sdm.duckdb
  sdm_db <- { s <- glue("{dir_big}/serve.duckdb"); if (file.exists(s)) s else glue("{dir_big}/sdm.duckdb") }
  # Per-version LOCAL files are gone: everything the app needs now comes from S3
  # (Parquet via serve.duckdb, COGs and PMTiles via the manifest). `er_gpkg` was
  # only ever listed as required and never read; `metrics_tif` existed to derive
  # subregion bboxes that are computed here from zone_cell + cell_lonlat; the
  # subregion<->programarea mapping is a DB query. Caches are keyed BY VERSION so
  # switching ?ver= cannot serve another release's geometry.
  dir_cache <- here(glue("scores/cache/{ver}")); dir.create(dir_cache, showWarnings = FALSE, recursive = TRUE)
  lyrs_csv <- glue("{dir_v}/layers_{ver}.csv")            # superseded by manifest metrics (see d_lyrs)
  pra_gpkg <- glue("{dir_v}/ply_programareas_2026_{ver}.gpkg")
  taxonomy_csv <- here(
    "scores/data/taxonomic_hierarchy_worms_2025-10-30.csv")
  tbl_pra_pm <- "ply_programareas_2026"

  # Only what every published version genuinely has. A file that exists for one
  # release and not another belongs behind a capability check, not in a hard stop
  # that refuses to start the app at all.
  v_required <- c(
    mapbox_tkn_txt,
    cell_tif,
    sdm_db,
    taxonomy_csv
  )
  v_missing <- v_required[!file.exists(v_required)]
  if (length(v_missing) > 0) {
    stop(glue(
      "Required files missing:\n  {paste(v_missing, collapse = '\n  ')}"
    ))
  }

  # spp_global_csv <- glue("{dir_data}/derived/spp_global_cache.csv")

  if (verbose) {
    message(glue("Verbose: TRUE"))
  }

  # mapbox token ----
  Sys.setenv(MAPBOX_PUBLIC_TOKEN = readLines(mapbox_tkn_txt))
  librarian::shelf(
    mapgl
  )

  # database ----
  con_sdm <- dbConnect(duckdb(), dbdir = sdm_db, read_only = T)

  # Resolve zone table names from the DATABASE, never from a version-derived
  # guess. v1/v2 name their zone tables WITHOUT the version suffix
  # (`ply_subregions_2026`, `ply_programareas_2026`) while v3+ carry it
  # (`ply_subregions_2026_v3`). So `glue("ply_subregions_2026_{ver}")` matched
  # NOTHING on v2 — every `filter(tbl == tbl_sr)` returned zero rows, the
  # subregion<->programarea cache was written empty, and because the cache is
  # only computed when the file is absent it stayed empty forever. The visible
  # symptom was a Program Areas choropleth with no fill and an `InfM/-InfM`
  # legend, which is `range(numeric(0))`.
  #
  # Prefer the suffixed name when it exists (v3+), else the highest-sorting
  # candidate for that field, which picks the 2026 vintage over the 2025 one
  # where a release carries both. Returns NA when the release has no such zone.
  zone_tbl_for <- function(fld_name, prefer = NULL) {
    cand <- tryCatch(
      dbGetQuery(con_sdm, glue(
        "SELECT DISTINCT tbl FROM zone WHERE fld = '{fld_name}' ORDER BY tbl"))$tbl,
      error = function(e) character())
    if (!length(cand)) return(NA_character_)
    if (!is.null(prefer) && prefer %in% cand) return(prefer)
    tail(sort(cand), 1)
  }
  tbl_er  <- zone_tbl_for("ecoregion_key",   "ply_ecoregions_2025")
  tbl_sr  <- zone_tbl_for("subregion_key",   glue("ply_subregions_2026_{ver}"))
  tbl_pra <- zone_tbl_for("programarea_key", glue("ply_programareas_2026_{ver}"))
  tbl_pa  <- zone_tbl_for("planarea_key",    glue("ply_planareas_2025_{ver}"))
  message(glue("zone tables resolved: ecoregion={tbl_er} subregion={tbl_sr} ",
               "programarea={tbl_pra} planarea={tbl_pa}"))

  # ...and the MEASUREMENT column with them. v1-v7 call it `value` everywhere
  # (`zone`, `zone_metric`, `cell_metric`, `model_cell`); v8 renamed it `val`, away
  # from DuckDB's reserved word. Crucially the two FORMS of a v8 release disagree
  # too: the served `serve.duckdb` carries `val` AND a `value` alias, while the
  # source `sdm.duckdb` it was built from carries only `val`.
  #
  # So neither spelling is safe to hardcode, and this file had hardcoded BOTH --
  # `cell_sql()` said `cm.val`, `get_rast()` said `value` -- meaning one of them was
  # always wrong for the release in hand. It stayed hidden because production reads
  # served DBs, where the alias covers `value`, and the `val` path is a fallback the
  # score COGs normally displace. Against a v8 source DB the app died during bundle
  # construction with `cannot coerce type 'closure'`: a bare `value` with no such
  # column resolves to a FUNCTION in scope, so the error names neither the table nor
  # the column.
  #
  # msens::sdm_val_col() resolves it per (connection, table) and is unit-tested on
  # both schemas. Below, the `*_tbl()` adapters rename the resolved column back to
  # `value` at the point of read, so every query downstream keeps one spelling; raw
  # SQL interpolates the resolved name instead.
  val_zone <- msens::sdm_val_col(con_sdm, "zone")
  val_zm   <- msens::sdm_val_col(con_sdm, "zone_metric")
  val_cm   <- msens::sdm_val_col(con_sdm, "cell_metric")
  message(glue("value columns resolved: zone={val_zone} zone_metric={val_zm} ",
               "cell_metric={val_cm}"))
  # Three shapes to survive, not two: `value` only (v1-v7), `val` only (v8 source), and
  # BOTH (v8 served, where the release writes a `value` alias beside `val`). A bare
  # rename() handles the first two and dies on the third with `Names must be unique` --
  # and the third is production. So drop the redundant alias before renaming, and skip
  # the rename entirely where the column is already canonically named.
  vals_tbl <- function(tbl_name, vcol) {
    t <- tbl(con_sdm, tbl_name)
    if (identical(vcol, "value")) return(t)
    t |> select(-any_of("value")) |> rename(value = !!sym(vcol))
  }
  zone_vals            <- function() vals_tbl("zone",        val_zone)
  zone_metric_vals     <- function() vals_tbl("zone_metric", val_zm)
  cell_metric_vals     <- function() vals_tbl("cell_metric", val_cm)
  # dbListTables(con_sdm)
  # duckdb_shutdown(duckdb()); rm(con_sdm)

  # tile server ----
  # browser-facing titilecache (Varnish) URL; used verbatim in the tile URL
  # template sent to mapbox-gl, and for cell_stats() calls from R.
  tile_base_url <- "https://titiler-v8.marinesensitivity.org"
  # cache-bust tag tied to the sdm.duckdb mtime: if the DB is rebuilt, every
  # cell_tile_url() and cell_stats() URL changes, invalidating the Varnish +
  # browser cache automatically. distinct from the dataset version (`v6`).
  db_mtime <- format(file.info(sdm_db)$mtime, "%Y%m%dT%H%M%SZ", tz = "UTC")

  # score COGs from the version manifest ----
  # Every layer this app draws is PRECOMPUTED, so asking titiler to run a SQL
  # SELECT per tile bought nothing while costing a bespoke factory, a SQL
  # validator guarding an injection surface, and a 1M-row cap the US grid already
  # sits just under. The manifest carries a COG href and a build-time rescale per
  # (metric, subregion) — the latter is what removes the /statistics round-trip on
  # every layer switch.
  #
  # Falls back to the SQL path whenever the manifest, the capability, or a
  # particular surface is missing, so an app pointed at a release that predates the
  # COGs still draws rather than blanking.
  manifest <- tryCatch(
    msens::atlas_manifest(ver),
    error = function(e) { message("manifest unavailable (", conditionMessage(e),
                                  ") - falling back to SQL tiles"); NULL })
  cog_tbl <- if (!is.null(manifest) && isTRUE(manifest$capabilities$score_cogs)) {
    cols <- c("metric_key", "description", "subregion_key", "cog",
              "rescale_min", "rescale_max", "colormap")
    mt   <- manifest$metrics[, intersect(cols, names(manifest$metrics)), drop = FALSE]
    # overlays (the "cells outside Program Areas" mask) are rasters the app draws
    # but not quantities it scores, so the manifest keeps them separate; here they
    # join the same lookup, keyed like a metric and carrying no rescale
    ov <- manifest$overlays
    if (!is.null(ov) && nrow(ov)) {
      ov <- data.frame(metric_key = ov$overlay_key, description = NA_character_,
                     subregion_key = ov$subregion_key,
                       cog = ov$cog, rescale_min = NA_real_, rescale_max = NA_real_,
                       colormap = ov$colormap, stringsAsFactors = FALSE)
      mt <- rbind(mt[, names(ov)], ov)
    }
    mt
  } else NULL
  message("score COGs: ", if (is.null(cog_tbl)) "none (SQL fallback)" else nrow(cog_tbl))

  # zone outlines from the manifest ----
  #
  # The zone PMTiles used to be two unversioned filenames on the file host
  # (`ply_programareas_2026`, `ply_ecoregions_2025`) inherited from the archived v7
  # notebook -- nothing built them reproducibly, and every release drew whatever
  # those files happened to contain. They are now published per VINTAGE
  # (`zones/{zone_set_key}/zones.pmtiles`) and each release's manifest names the
  # vintage it actually used, so v3 and v8 can draw different Program Areas.
  #
  # The layer id inside the published tiles is the zone TYPE (`programarea`), not
  # the old table name, so URL and source_layer must move together -- pointing the
  # new URL at the old layer name yields a silently empty overlay.
  zone_tbl <- if (!is.null(manifest)) manifest$zones else NULL

  # Returns NULL when the manifest is PRESENT but names no such zone type: that is
  # a positive statement that the release does not have it, not a gap to paper
  # over. v1 predates Program Areas entirely (`capabilities.programareas = FALSE`),
  # so falling back there would draw the 2026 Program Areas over a 2025 Planning
  # Area release -- an outline that looks entirely plausible and is simply wrong.
  # The unversioned fallback is reserved for a missing manifest, where drawing the
  # historical default still beats a blank map.
  ztile <- function(zone_type, fallback_tbl) {
    if (!is.null(zone_tbl) && "pmtiles" %in% names(zone_tbl)) {
      i <- which(zone_tbl$fld == paste0(zone_type, "_key") & !is.na(zone_tbl$pmtiles))
      return(if (length(i)) list(url = zone_tbl$pmtiles[i[1]], source_layer = zone_type)
             else NULL)
    }
    list(url          = glue("{pmtiles_base_url}/{fallback_tbl}.pmtiles"),
         source_layer = fallback_tbl)
  }
  message("zone tiles: ",
          if (is.null(zone_tbl) || !"pmtiles" %in% names(zone_tbl))
            "none (unversioned fallback)" else sum(!is.na(zone_tbl$pmtiles)))

  # resolved once: every later add_fill_layer / add_line_layer that reuses the
  # source add_pmline created must name the SAME layer inside those tiles, or it
  # renders nothing at all (no error -- just an empty overlay)
  # %||% keeps these character even when the release has no such zone: the layers
  # that use them sit behind the matching capability gate and are unreachable then
  pra_src_layer <- ztile("programarea", tbl_pra_pm)$source_layer %||% tbl_pra_pm
  er_src_layer  <- ztile("ecoregion",  tbl_er)$source_layer      %||% tbl_er



  cog_of <- function(metric_key, subregion_key = "FULL") {
    if (is.null(cog_tbl)) return(NULL)
    i <- which(cog_tbl$metric_key == metric_key & cog_tbl$subregion_key == subregion_key)
    if (!length(i) || is.na(cog_tbl$cog[i[1]])) return(NULL)
    list(url      = cog_tbl$cog[i[1]],
         rescale  = c(cog_tbl$rescale_min[i[1]], cog_tbl$rescale_max[i[1]]),
         colormap = cog_tbl$colormap[i[1]])
  }

  # one place that answers "how do I draw this layer?", COG-first
  #
  # THE STUDY AREA IS A CAMERA, NOT A FILTER (apps#13). This used to ask for the
  # per-subregion COG, which is clipped -- and on v4-v7 the `USA` subregion is
  # EXACTLY the Program-Area union (349,139 of 662,075 scored cells), so the
  # default view silently hid 47% of the scores. They were computed, they are
  # published, and the FULL COG has them: sampling the v7 composite FULL COG at
  # (-64.925, 18.275) in the US Virgin Islands returns 93, while the USA COG
  # does not even cover that longitude.
  #
  # So the raster is always the FULL surface, on every release; `subregion_key`
  # now only moves the camera (sr_view). Every release v1-v8 publishes a
  # FULL COG, so the per-subregion fallback below is defensive, not a path we
  # expect to take.
  #
  # Consequence worth knowing: FULL's rescale spans the whole study area rather
  # than the chosen subregion, so colours are comparable BETWEEN study areas at
  # the cost of some contrast within a small one. That is the honest trade for
  # "show all the data".
  layer_tiles <- function(metric_key, subregion_key = "FULL", palette = "spectral_r") {
    cg <- cog_of(metric_key, "FULL")
    if (is.null(cg)) cg <- cog_of(metric_key, subregion_key)
    if (!is.null(cg))
      return(list(rescale = cg$rescale,
                  url = msens::cog_tile_url(cg$url, colormap = palette,
                                            rescale = cg$rescale, base = tile_base_url)))
    sql <- cell_sql(metric_key)
    st  <- msens::cell_stats(sql, mtime = db_mtime, base = tile_base_url)
    rs  <- c(st$min, st$max)
    list(rescale = rs,
         url = msens::cell_tile_url(sql, colormap = palette, rescale = rs,
                                    mtime = db_mtime, base = tile_base_url))
  }

  # build the (cell_id, value) SELECT for a given metric + subregion; passed to
  # msens::cell_tile_url() / cell_stats(). strict allowlist on the identifiers
  # to keep the string string-interpolation-safe before it ever hits DuckDB.
  # `subregion_key` is accepted and DELIBERATELY IGNORED: the study area moves the
  # camera and never filters the surface (apps#13). It used to add a zone_cell
  # join that dropped every cell outside the chosen subregion -- the SQL-tile
  # twin of the clipped-COG masking above. Kept in the signature so existing
  # call sites read unchanged.
  cell_sql <- function(metric_key, subregion_key = "FULL") {
    stopifnot(
      is.character(metric_key),   length(metric_key) == 1,
      grepl("^[A-Za-z0-9_.-]+$", metric_key))
    stopifnot(
      is.character(subregion_key), length(subregion_key) == 1,
      grepl("^[A-Za-z0-9_]+$",   subregion_key))
    # `cm.{val_cm}` / `z.{val_zone}`, not a hardcoded `val`: this SELECT is handed to
    # titiler, so on v1-v7 (where the column is `value`) it produced a tile request that
    # could only fail server-side, where the error is a blank layer rather than a message.
    glue(
      "SELECT cm.cell_id, cm.{val_cm} AS value ",
      "FROM cell_metric cm ",
      "JOIN metric m ON cm.metric_seq = m.metric_seq ",
      "WHERE m.metric_key = '{metric_key}'")
  }

  # helper functions ----
  get_rast <- function(m_key, subregion_key = "FULL") {
    # m_key         = "score_extriskspcat_primprod_ecoregionrescaled_equalweights"
    # m_key = "extrisk_mammal"; subregion_key = "FULL" (full study area)

    d_metric <- tbl(con_sdm, "metric") |> # get metric.metric_seq
      filter(metric_key == !!m_key) |> #   by input$sel_lyr
      select(metric_seq) |>
      inner_join(
        # get cell_metric.value
        cell_metric_vals() |>
          select(metric_seq, cell_id, value),
        by = join_by(metric_seq)
      ) |>
      select(cell_id, value)

    d <- if (subregion_key == "FULL") {
      # full study area: all cells with this metric
      d_metric |> collect()
    } else {
      # limit to subregion zone
      d_metric |>
        inner_join(
          zone_vals() |>
            filter(
              tbl == !!tbl_sr,
              fld == "subregion_key",
              value == !!subregion_key
            ) |>
            select(zone_seq) |>
            inner_join(
              tbl(con_sdm, "zone_cell") |>
                select(zone_seq, cell_id),
              by = join_by(zone_seq)
            ) |>
            select(cell_id),
          by = join_by(cell_id)
        ) |>
        collect()
    }

    r <- init(r_cell_open()[[1]], NA) # plot(r)
    r[d$cell_id] <- d$value

    r <- trim(r) # plot(r)

    r
  }

  get_lyr_name <- function(lyr) {
    # get layer name from d_lyrs
    lyr_name <- d_lyrs |>
      filter(lyr == !!lyr) |>
      pull(layer)
    if (length(lyr_name) == 0) {
      stop(glue("Layer '{lyr}' not found in d_lyrs."))
    }
    lyr_name
  }

  # plot_flower, cells_in_polygon, scores_for_cells, species_for_cells:
  # now provided by the msens package (loaded via librarian::shelf above).
  # Polygon drawing moved from the Map tab to the Report tab; the old
  # paint_drawn_polygon helper was removed along with it.

  # data prep ----

# Program-area polygons for the Report tab (the user's added areas go on to the
  # scoring API, so this needs real geometry, not just something drawable). Read
  # the vintage's GeoParquet published beside its PMTiles -- geometry depends on
  # the VINTAGE, not the release, so one file serves every version that uses it.
  # The per-version gpkg is the fallback, and existed only for v3-v8.
  # Program-area polygons for the Report tab (the user's added areas go on to the
# scoring API, so this needs real geometry, not just something drawable).
#
# LAZY on purpose: the published FlatGeobuf is ~7 MB and only the Report tab ever
# needs it, so loading it at startup would put a 7 MB download in front of every
# visitor who just wants the map. Memoised after the first call.
#
# Read from the VINTAGE's published geometry, not a per-version gpkg -- geometry
# depends on the vintage, so one file serves every release that uses it. Local
# gpkg stays as the fallback (and is faster when present).
.zone_geom_cache <- new.env(parent = emptyenv())

# Polygon geometry for ANY zone type of this release, from the published
# per-vintage FlatGeobuf (`zones/{zone_set_key}/zones.fgb`), falling back to a
# local gpkg for program areas where one is checked in. Returns NULL when the
# release has no such unit, which callers must handle: reading the gpkg path
# directly is what made v1 and v2 -- which ship no program-area file -- die at
# startup with "The file doesn't seem to exist", i.e. HTTP 500 for the entire
# release rather than a map without labels.
zone_geom <- function(type) {
  if (is.null(type) || is.na(type)) return(NULL)
  if (!is.null(.zone_geom_cache[[type]])) return(.zone_geom_cache[[type]])
  kcol <- paste0(type, "_key"); ncol <- paste0(type, "_name")
  g <- NULL
  # local gpkg, where this release ships one for that unit
  loc <- switch(type,
    programarea = pra_gpkg,
    planarea    = glue("{dir_v}/ply_planareas_2025_{ver}.gpkg"),
    ecoregion   = glue("{dir_v}/ply_ecoregions_2025.gpkg"),
    NULL)
  if (!is.null(loc) && file.exists(loc))
    g <- tryCatch(read_sf(loc) |> select(any_of(c(kcol, ncol))),
                  error = function(e) NULL)
  # published FlatGeobuf for the vintage the manifest names -- the general path,
  # and the only one that works for a unit with no local file
  if (is.null(g) || !nrow(g)) {
    zk <- if (!is.null(zone_tbl) && "zone_set_key" %in% names(zone_tbl)) {
      i <- which(zone_tbl$fld == paste0(type, "_key"))
      if (length(i)) zone_tbl$zone_set_key[i[1]] else NA_character_
    } else NA_character_
    if (!is.na(zk)) {
      src <- glue("/vsicurl/{msens::atlas_base_url()}/zones/{zk}/zones.fgb")
      g <- tryCatch(read_sf(src), error = function(e) {
        message("zone fgb unavailable for ", type, " (", conditionMessage(e), ")"); NULL })
      if (!is.null(g)) g <- g |> select(any_of(c(kcol, ncol)))
    }
  }
  if (is.null(g) || !nrow(g)) {
    message("no ", type, " geometry for ", ver)
    g <- NULL
  } else if (!ncol %in% names(g)) {
    # a name column is optional; label/tooltip fall back to the key
    g[[ncol]] <- g[[kcol]]
  }
  .zone_geom_cache[[type]] <- g
  g
}

# kept for the Report tab, which asks specifically for the programme's areas
pra_geom <- function() zone_geom("programarea")

  # ---- zone units: every spatial unit THIS release actually scores ----------
  #
  # The app used to hardcode one polygon unit, Program Areas. That is wrong in
  # both directions: v1 has none (it scores 36 Planning Areas and 12 Ecoregions),
  # and v8 scores Ecoregions and Subregions alongside Program Areas but offered
  # neither. Worse, "Planning areas" was briefly offered on the strength of the
  # release capability alone while no render path existed, so choosing it silently
  # did nothing.
  #
  # So the unit list is DERIVED: a unit appears iff this release has zone rows for
  # it, PMTiles to draw it, and at least one composite score attached to it. All
  # three are required — geometry without scores paints an empty choropleth, and
  # scores without geometry cannot be drawn at all. Zone types will keep changing,
  # so nothing below names a specific one.
  zone_units <- local({
    z <- zone_tbl
    # requires the manifest's per-vintage tile URLs; the unversioned fallback
    # cannot say WHICH units a release has, so no unit list is offered from it
    if (is.null(z) || !nrow(z) || !"pmtiles" %in% names(z)) return(NULL)
    # Count the zones that actually carry a composite score, NOT the zones the
    # manifest declares. v7 defines 5 subregions and scores exactly one of them
    # (the study-area rollup), so declaring it a choropleth unit would paint 1 of
    # 5 and leave the rest grey. A unit needs >= 2 scored zones to be a map.
    # The `value` alias exists on every SERVED release, but not on a v8 source DB --
    # and the tryCatch below would have turned that into "no scored units at all",
    # i.e. a silently unusable app rather than an error. Resolve the name instead.
    scored <- tryCatch(
      dbGetQuery(con_sdm, glue("
        SELECT DISTINCT z.{val_zone} AS zkey, z.fld
          FROM zone z JOIN zone_metric zm USING (zone_seq)
          JOIN metric m USING (metric_seq)
         WHERE m.metric_key LIKE 'score!_%' ESCAPE '!'")),
      error = function(e) data.frame(fld = character(), zkey = character()))
    scored <- split(as.character(scored$zkey), scored$fld)   # fld -> scored keys
    lab <- c(programarea = "Program areas", planarea  = "Planning areas",
             ecoregion   = "Ecoregions",    subregion = "Subregions")
    out <- lapply(seq_len(nrow(z)), function(i) {
      fld  <- z$fld[i]
      type <- sub("_key$", "", fld)
      ks   <- scored[[fld]]
      if (is.null(ks) || length(ks) < 2 || is.na(z$pmtiles[i])) return(NULL)
      r <- data.frame(
        fld = fld, type = type,
        label = unname(if (type %in% names(lab)) lab[[type]] else
                       paste0(toupper(substring(type, 1, 1)), substring(type, 2), "s")),
        url = z$pmtiles[i], source_layer = type,
        zone_set_key = if ("zone_set_key" %in% names(z)) z$zone_set_key[i] else NA_character_,
        n = length(ks), stringsAsFactors = FALSE)
      r$keys <- list(ks)
      r
    })
    out <- do.call(rbind, Filter(Negate(is.null), out))
    if (is.null(out) || !nrow(out)) return(NULL)
    # Program Areas first where present -- the current programme's reporting unit
    # -- then finest first, so the default is the most detailed view available.
    out <- out[order(out$type != "programarea", -out$n), , drop = FALSE]
    rownames(out) <- NULL
    out
  })
  # A zone must be DRAWABLE as well as scored. v8 scores 5 subregions but the
  # published geometry has 4: `USA` is the whole-study-area rollup, a total rather
  # than a mappable subregion, and v7's lone scored subregion is the same thing
  # under the name `FULL`. Intersecting scored keys with the geometry's keys
  # excludes them WITHOUT hardcoding either name -- which matters, because these
  # rollup keys have already changed once and the zone sets will change again.
  zone_units <- local({
    if (is.null(zone_units)) return(NULL)
    keep <- lapply(seq_len(nrow(zone_units)), function(i) {
      type <- zone_units$type[i]
      g <- zone_geom(type)
      if (is.null(g) || !nrow(g)) return(NULL)
      kcol <- paste0(type, "_key")
      drawable <- intersect(zone_units$keys[[i]], as.character(g[[kcol]]))
      if (length(drawable) < 2) return(NULL)
      r <- zone_units[i, , drop = FALSE]
      r$n <- length(drawable)
      r$keys <- list(drawable)
      r
    })
    keep <- do.call(rbind, Filter(Negate(is.null), keep))
    if (is.null(keep) || !nrow(keep)) return(NULL)
    rownames(keep) <- NULL
    keep
  })
  message("scored zone units: ",
          if (is.null(zone_units)) "none" else
            paste(sprintf("%s(%d)", zone_units$type, zone_units$n), collapse = " "))

  # The release's PRIMARY reporting unit: what the map outlines as context in
  # raster-cell mode, and the default polygon unit. Program Areas where they
  # exist, otherwise the finest scored unit (v1 -> Planning Areas).
  primary_unit <- if (is.null(zone_units)) NULL else zone_units$type[1]

  # Which outline layers exist, and therefore what a later layer may sit BEFORE.
  #
  # MapLibre rejects an add whose before_id names a layer that does not exist,
  # and the failure CASCADES: v1 has no Program Areas, so `pra_ln` was never
  # added, the ecoregion layer asking for before_id="pra_ln" failed, and the score
  # raster asking for before_id="er_ln" failed after it -- the map came up with
  # nothing but labels. These must therefore be derived from the units actually
  # created, which is why they live here and not beside the tile helpers: the
  # outline ids are now `{type}_ln`, so a stale "pra_ln" would resurrect that bug.
  primary_ln <- if (is.null(primary_unit)) NULL else paste0(primary_unit, "_ln")
  # a standalone ecoregion outline only exists when ecoregion is NOT itself a
  # scored unit (otherwise it already has a `{type}_ln` of its own)
  has_er_ln  <- !is.null(ztile("ecoregion", tbl_er)) &&
                !("ecoregion" %in% (zone_units$type %||% character()))
  before_er  <- primary_ln
  before_r   <- if (has_er_ln) "er_ln" else primary_ln
  zone_unit_row <- function(type) {
    if (is.null(zone_units) || is.null(type)) return(NULL)
    i <- which(zone_units$type == type)
    if (length(i)) zone_units[i[1], ] else NULL
  }
  # the layers-control entries for the zone units: every outline, but a label entry
  # only for a unit that HAS a label layer (msens::zone_style says which) -- an
  # entry naming a layer that was never added is a dead switch
  zone_ctrl_layers <- function() {
    if (is.null(zone_units)) return(list())
    has_lbl <- vapply(zone_units$type, function(t) !is.null(msens::zone_label_args(t)), logical(1))
    c(setNames(as.list(paste0(zone_units$type, "_ln")),
               paste(zone_units$label, "outlines")),
      setNames(as.list(paste0(zone_units$type[has_lbl], "_lbl")),
               paste(zone_units$label[has_lbl], "labels")))
  }

  # * zone label points, cached per unit ----
  # Cached per VERSION and per TYPE: this is geometry, and a release without a
  # given unit was previously drawing another release's labels over an empty map.
  zone_pts <- local({
    cache <- new.env(parent = emptyenv())
    function(type) {
      if (is.null(type) || is.na(type)) return(NULL)
      if (!is.null(cache[[type]])) return(cache[[type]])
      kcol <- paste0(type, "_key"); ncol <- paste0(type, "_name")
      f <- glue("{dir_cache}/{type}_label_pts.csv")
      d <- if (file.exists(f)) tryCatch(read_csv(f, show_col_types = FALSE),
                                        error = function(e) NULL) else NULL
      if (is.null(d)) {
        g <- zone_geom(type)
        d <- if (is.null(g) || !nrow(g)) {
          setNames(tibble(character(), character(), numeric(), numeric()),
                   c(kcol, ncol, "lng", "lat"))
        } else {
          # st_coordinates() on the SF OBJECT, not on a named geometry column:
          # the column is `geom` from a GeoPackage and `geometry` from
          # FlatGeobuf, so naming it broke outright when the source changed --
          # and that took the whole app down, not just the labels.
          suppressWarnings({
            pts <- g |> st_shift_longitude() |> st_point_on_surface()
            crd <- st_coordinates(pts)
            out <- pts |> st_drop_geometry()
            out$lng <- crd[, 1]; out$lat <- crd[, 2]
            out[, c(kcol, ncol, "lng", "lat")]
          })
        }
        tryCatch(write_csv(d, f), error = function(e)
          message("could not cache ", type, " labels: ", conditionMessage(e)))
      }
      out <- st_as_sf(d, coords = c("lng", "lat"), crs = 4326)
      cache[[type]] <- out
      out
    }
  })

  # the primary unit's labels, used by the base map
  pra_pts <- zone_pts(primary_unit %||% "programarea")

  # * sr_choices ----
  # sr_choices <- c(
  #   "All USA" = "USA",
  #   "Mainland USA" = "L48",
  #   "Alaska" = "AK",
  #   "Mainland USA & Alaska" = "AKL48")
  # TODO: version subregions
  # TODO: add other subregions:
  # - `HI`  : Hawaii
  # - `HIPI`: Hawaii & Pacific Island Territories
  # - `HIPI`: Pacific Island Territories
  # - `PAC` : Pacific Islands & Mainland USA
  # - `GOA` : Gulf of America
  # - `ATL` : Mainland Atlantic
  # - `ATL` : Atlantic & Gulf of America, incl. Puerto Rico

  # * check cached ----
  # Was a second hard stop over the same per-version files as v_required (and it
  # named a notebook, calc_scores.qmd, that v8 replaced). Only cell_tif is still a
  # real prerequisite; the rest are resolved from the manifest or the view DB.
  if (!file_exists(cell_tif))
    stop(glue("cell id raster missing: {cell_tif}"))

  # NOT a cached SpatRaster.
  #
  # A terra SpatRaster is an EXTERNAL POINTER into C++ memory. The per-version
  # bundle is memoised across Shiny sessions, so a cached raster gets reused by a
  # later session whose pointer is stale -- and terra::extract() on a stale
  # pointer SEGFAULTS the R process. That is what "clicking the map disconnects
  # the app" was: no R error in the log, no traceback, just a dead process,
  # because a segfault is not an exception.
  #
  # Opening it costs ~10 ms (measured), so open per use and cache only the path.
  # deliberately NOT stored: every caller opens its own handle
  r_cell_open <- function() rast(cell_tif)

  # * lyrs ----
  # Layer picker metadata comes from the MANIFEST, so a release needs no local
  # layers_{ver}.csv (v1/v2 have none, which is one reason they could not render).
  # The csv is kept as a fallback for a release whose manifest predates the fields.
  d_lyrs <- local({
    cols <- c("lyr_order", "category", "label")
    if (!is.null(cog_tbl) && all(cols %in% names(cog_tbl))) {
      d <- cog_tbl[!is.na(cog_tbl$lyr_order), c("metric_key", cols)]
      d <- d[!duplicated(d$metric_key), ]
      d <- d[order(d$lyr_order), ]
      if (nrow(d))
        return(tibble(order = d$lyr_order, category = d$category,
                      layer = d$label, lyr = d$metric_key))
    }
    if (file.exists(lyrs_csv)) return(read_csv(lyrs_csv, show_col_types = FALSE))

    # No layers csv (v1/v2): derive friendly names from the manifest rather than
    # showing raw database column names like `extrisk_all_ecoregion_rescaled`.
    # `description` is already published per metric ("Extinction risk for all"),
    # so use it, and put the OVERALL score first -- v2 defaulted to extrisk_all
    # purely because it sorts first alphabetically.
    if (is.null(cog_tbl))
      return(tibble(order = integer(), category = character(),
                    layer = character(), lyr = character()))
    d  <- cog_tbl[!duplicated(cog_tbl$metric_key), ]
    ds <- if ("description" %in% names(d)) d$description else rep(NA_character_, nrow(d))
    is_score <- grepl("^score", d$metric_key)
    is_resc  <- grepl("_ecoregion_rescaled$", d$metric_key)
    lab <- ifelse(is.na(ds) | !nzchar(ds), d$metric_key, ds)
    lab[is_score] <- "score"                  # the overall index, as older apps named it
    tibble(lyr      = d$metric_key,
           layer    = lab,
           category = ifelse(is_score, "Overall",
                      ifelse(is_resc, "Species, rescaled by Ecoregion", "Species")),
           rank     = ifelse(is_score, 0L, ifelse(is_resc, 2L, 1L))) |>
      arrange(rank, layer) |>
      mutate(order = row_number()) |>
      select(order, category, layer, lyr)
  })
  message("layer picker: ", nrow(d_lyrs), " layers (",
          if (!is.null(cog_tbl) && "lyr_order" %in% names(cog_tbl)) "manifest" else "csv/derived", ")")

  # ** test lyrs (eval = F for performance) ----
  if (F) {
    source(here("../workflows/libs/db.R")) # con
    # dbDisconnect(con, shutdown = T)

    # confirm all layers available for both planareas and cell metrics
    # lyrs_pa <- dbListFields(con, "ply_planareas_2025")
    lyrs_pra <- dbListFields(con, tbl_pra)
    lyrs_cell <- tbl(con_sdm, "metric") |>
      semi_join(
        cell_metric_vals() |>
          distinct(metric_seq),
        by = "metric_seq"
      ) |>
      pull(metric_key)
    # stopifnot(all(d_lyrs$lyr %in% lyrs_pa))
    stopifnot(all(d_lyrs$lyr %in% lyrs_pra))
    stopifnot(all(d_lyrs$lyr %in% lyrs_cell))
  }

  lyr_choices <- d_lyrs |>
    group_by(order, category) |>
    summarise(
      layer = list(setNames(lyr, layer)),
      .groups = "drop"
    ) |>
    arrange(order, layer) |>
    select(-order) |>
    deframe()

  lyr_default <- d_lyrs$lyr[1]

  # palette choices: default + color-blind friendly alternatives (via cblindplot CVD mappings)
  # deuteranopia -> viridis, protanopia -> cividis, tritanopia -> magma
  palette_choices <- c(
    "Spectral (default)"        = "spectral_r",
    "Viridis (deuteranopia)"    = "viridis",
    "Cividis (protanopia)"      = "cividis",
    "Magma (tritanopia)"        = "magma"
  )

  get_pal_colors <- function(pal_key, n = 11) {
    switch(pal_key,
      spectral_r = rev(RColorBrewer::brewer.pal(n, "Spectral")),
      viridis    = viridisLite::viridis(n),
      cividis    = viridisLite::cividis(n),
      magma      = viridisLite::magma(n),
      rev(RColorBrewer::brewer.pal(n, "Spectral"))
    )
  }

  # * planareas by subregion ---

  # if (!file.exists(sr_pa_csv)) {
  #   # calculate subregion - planarea cells
  #   message(glue("Calculating subregion - planarea cells..."))
  #
  #   # subregion cells
  #   tbl_sr_cell <- tbl(con_sdm, "zone") |>
  #     filter(fld == "subregion_key") |>
  #     select(sr_key = value, zone_seq) |>
  #     inner_join(
  #       tbl(con_sdm, "zone_cell") |>
  #         select(zone_seq, cell_id),
  #       by = join_by(zone_seq)
  #     ) |>
  #     select(sr_key, cell_id)
  #
  #   # planarea cells
  #   tbl_pa_cell <- tbl(con_sdm, "zone") |>
  #     filter(fld == "planarea_key") |>
  #     select(pa_key = value, zone_seq) |>
  #     inner_join(
  #       tbl(con_sdm, "zone_cell") |>
  #         select(zone_seq, cell_id),
  #       by = join_by(zone_seq)
  #     ) |>
  #     select(pa_key, cell_id)
  #
  #   # planareas per subregion
  #   d_sr_pa <- tbl_sr_cell |>
  #     inner_join(
  #       tbl_pa_cell,
  #       by = join_by(cell_id)
  #     ) |>
  #     group_by(sr_key, pa_key) |>
  #     summarise(n_cells = n(), .groups = "drop") |>
  #     arrange(sr_key, pa_key) |>
  #     select(
  #       subregion_key = sr_key,
  #       planarea_key = pa_key
  #     ) |>
  #     collect()
  #
  #   # write to csv
  #   write_csv(d_sr_pa, sr_pa_csv)
  # } else {
  #   d_sr_pa <- read_csv(sr_pa_csv)
  # }
  # sr <- read_sf(sr_gpkg)

  # (the subregion -> programarea mapping that used to live here is gone with
  # the Program-Area-derived study areas; see below)

  # * study areas — one canonical set, every release ----
  #
  # These are CAMERA PRESETS and nothing else (apps#13, apps#14). Everything
  # that used to live here -- a subregion->programarea mapping, a per-version
  # cached bbox csv, a FULL row synthesised from their union, a picker built by
  # intersecting "has a surface" with "has an extent" -- existed to answer
  # "which subregions did THIS release publish?", and that question stopped
  # mattering the moment the study area no longer filtered the map.
  #
  # msens::study_areas() is derived from the ecoregion `region_key` rollup,
  # which is shared across v1-v8, so every release now offers the same five and
  # a release can point at water it never scored: v7 has Atlantic scores and,
  # until this, no Atlantic preset, because the old subregions were dissolved
  # from a 2026 program that has no Atlantic areas.
  #
  # Centres and zooms are computed on the unit sphere -- East Bering Sea and the
  # Pacific Island Territories both cross the antimeridian, where a bounding box
  # reports a 67 deg span as 360 (apps#9).
  d_sa <- msens::study_areas()

  sr_view <- function(sr_key) {
    r <- d_sa[d_sa$key == sr_key, ]
    if (!nrow(r)) r <- d_sa[d_sa$key == "FULL", ]
    list(center = c(r$lon[1], r$lat[1]), zoom = r$zoom[1])
  }
  sr_choices <- stats::setNames(d_sa$key, d_sa$label)
  message("study areas: ", paste(names(sr_choices), collapse = ", "))

  # The zone that means "everything this release scored". The study-area presets
  # are geography and are the same everywhere; the ZONES are per release and
  # differ (v7 publishes FULL/USA/AK/GA/PA, v8 publishes USA/AK/AT/GA/PA), so
  # anything reading a published aggregate -- the flower plot, the species table
  # -- resolves it here rather than assuming the camera's key names a zone. It
  # would not: selecting Atlantic on v7 names a zone v7 does not have.
  zone_all_key <- local({
    have <- tryCatch(zone_vals() |> filter(fld == "subregion_key") |> pull(value),
                     error = function(e) character())
    for (k in c("FULL", "USA")) if (k %in% have) return(k)
    if (length(have)) have[1] else "USA"
  })
  message("whole-study-area zone: ", zone_all_key)

  # Report version list: every published release, newest first, labelled with
  # status so a pre-release or retired one is not mistaken for the promoted one.
  rpt_ver_choices <- local({
    v <- tryCatch(msens::atlas_versions(), error = function(e) NULL)
    if (is.null(v) || !nrow(v)) return(setNames(ver, ver))
    lab <- ifelse(v$status == "released", v$ver,
                  sprintf("%s (%s)", v$ver, v$status))
    setNames(v$ver, lab)
  })

  # pre-compute initial tile state for build_initial_map() so startup doesn't
  # block on a network round-trip to /msens/statistics every time the map
  # re-renders (sphere toggle, etc.). Varnish caches this anyway; warming
  # it once at boot keeps the critical path synchronous-but-fast.
  initial_sql      <- cell_sql(lyr_default, sr_choices[[1]])
  initial_lyr      <- layer_tiles(lyr_default, sr_choices[[1]])
  initial_rescale  <- initial_lyr$rescale
  initial_tile_url <- initial_lyr$url
  initial_view     <- sr_view(sr_choices[[1]])

  # "Cells outside Program Areas" overlay: a binary mask served by the same
  # msens TiTiler factory but with the `color=` param (single-color mask
  # render, bypassing colormap/rescale). Replaces the old r_outside_pra
  # terra raster + msens::add_cells(..., colors = c("#222222","#222222")).
  outside_pra_sql <- paste0(
    "SELECT c.cell_id, 1.0 AS value ",
    "FROM (SELECT DISTINCT cell_id FROM cell_metric) c ",
    "WHERE c.cell_id NOT IN (",
    "SELECT zc.cell_id FROM zone_cell zc ",
    "JOIN zone z ON zc.zone_seq = z.zone_seq ",
    "WHERE z.fld = 'programarea_key'",
    ")")
  outside_pra_tile_url <- local({
    cg <- cog_of("_outside_pra", "FULL")
    if (!is.null(cg))
      msens::cog_tile_url(cg$url, color = "#222222", base = tile_base_url)
    else
      msens::cell_tile_url(outside_pra_sql, color = "#222222",
                           mtime = db_mtime, base = tile_base_url)
  })

  # NOTE: the previous r_outside_pra terra raster (cached to
  # scores/cache/r_cells_outside_pra.tif) is gone — the same "cells with
  # metric values but outside any Program Area" mask is now rendered by
  # the msens TiTiler factory via `outside_pra_tile_url` (defined above,
  # SQL: cell_metric cell_ids NOT IN any zone where fld='programarea_key').

  # * default subregion flower-plot data (cached) ----
  # Pre-compute the flower-plot tibble for each subregion zone (USA, AK, GA,
  # PA) once at startup so the default Plot of Scores tab loads instantly.
  # zone_metric for subregions was added by the cell_metrics_to_zone_metrics
  # chunk in calc_scores.qmd; if it's missing for some reason this still
  # falls back to on-the-fly aggregation across cell_metric x zone_cell.
  flower_default_csv <- here("scores/cache/flower_default_subregions.csv")
  if (!file_exists(flower_default_csv)) {
    if (verbose) message("Building flower_default_subregions cache...")
    d_flower_default <- zone_vals() |>
      filter(tbl == !!tbl_sr, fld == "subregion_key") |>
      select(zone_seq, subregion_key = value) |>
      inner_join(zone_metric_vals(), by = "zone_seq") |>
      inner_join(
        tbl(con_sdm, "metric") |>
          filter(str_detect(metric_key, ".*_ecoregion_rescaled$")) |>
          select(metric_seq, metric_key),
        by = "metric_seq") |>
      select(subregion_key, metric_key, score = value) |>
      collect() |>
      mutate(
        component = metric_key |>
          str_replace("extrisk_", "") |>
          str_replace("_ecoregion_rescaled", "") |>
          str_replace("_", " "),
        even = 1) |>
      filter(component != "all")
    if (nrow(d_flower_default) == 0) {
      warning("No subregion zone_metric rows; default flower plot will be empty. ",
              "Re-run the cell_metrics_to_zone_metrics chunk in calc_scores.qmd.")
    }
    write_csv(d_flower_default, flower_default_csv)
  }
  d_flower_default <- read_csv(flower_default_csv)

  # * d_taxonomy ----
  d_taxonomy <- read_csv(taxonomy_csv, guess_max = Inf)

  # ui ----
  light <- bs_theme()
  # dark <- bs_theme(bg = "black", fg = "white", primary = "purple")
  dark <- bs_theme()
  # ui is a FUNCTION of the request, not a static object, for one reason: the
  # client IP. shiny-server does not proxy the websocket upgrade — it opens a
  # fresh localhost connection to the R worker — so the server session sees
  # REMOTE_ADDR 127.0.0.1 and no X-Forwarded-For (Caddy sets it correctly; it is
  # lost at the shiny-server hop). This page request is the only one that still
  # carries the real address, so it is captured here and baked into the snippet.

  environment()
}

# memoised: building a bundle opens a DuckDB connection, reads the manifest and
# derives the subregion extents (~1 s), so a second visitor asking for the same
# version must not pay it again
bundle <- function(v) {
  v <- as.character(v)[1]
  if (is.null(.bundles[[v]])) .bundles[[v]] <- build_bundle(v)
  .bundles[[v]]
}

# ?ver= from a request/session, resolved against the published registry. An
# unknown or absent value falls back to the promoted release rather than
# erroring, and the UI reports which it settled on.
#
# `allow_access` is the pre-release review gate. This process may resolve only
# the `access` values msens::atlas_allow_access() returns: "public" on the
# public Shiny Server instance, everything on the PREVIEW instance -- a second
# Shiny Server block whose wrapper app.R sets MS_PREVIEW=1 and is reachable only
# through the signed-in preview.marinesensitivity.org vhost. The policy is an
# env var of the PROCESS, never a request header: shiny-server opens its own
# websocket to this worker, so no proxy header reaches session$request, and a
# client-supplied url_search could otherwise steer the public instance to a
# restricted release. A restricted request falls back to the promoted release
# here; the ?ver= observer in server_impl says why.
ver_of <- function(qs) {
  v <- tryCatch({
    q <- shiny::parseQueryString(qs %||% "")
    msens::atlas_resolve_ver(q$ver, allow_access = msens::atlas_allow_access())
  }, error = function(e) NULL)
  if (is.null(v)) tryCatch(msens::atlas_resolve_ver(NULL, allow_access = msens::atlas_allow_access()),
                           error = function(e) ver_fallback) else v
}

# The version a SESSION renders: the token ui(req) embedded for the page, never
# the client-reported URL. session$clientData$url_search / url_pathname are
# whatever the browser's JavaScript sends over the websocket -- forgeable -- and
# on the signed-in preview host the version is the URL PATH (/v9/scores/, gated
# per version by Cloudflare Access; Caddy rewrites it to ?ver=v9 for the page
# GET), so a reviewer allowed on v9 must not be able to steer this process to
# v10. The token is HMAC-signed by the server; a client can drop or edit it but
# cannot mint one. Its version is re-resolved through the instance policy, so a
# token minted elsewhere never widens what this process shows. A missing or
# stale token (a page older than 24 h, or one straddling a process restart
# without MS_TOKEN_SECRET) falls back to the promoted release -- reload the page.
ver_of_session <- function(input, session) {
  vt <- msens::ver_token_verify(isolate(input$ms_ver_token))
  if (is.null(vt)) return(ver_of(NULL))
  tryCatch(msens::atlas_resolve_ver(vt, allow_access = msens::atlas_allow_access()),
           error = function(e) ver_of(NULL))
}

# The version of the PAGE being served. On the preview host it is the URL PATH
# (/v9/scores/), which Caddy strips before shiny-server sees it and hands over as
# X-MS-Version -- a header the server SETS from the path, so a client can neither
# forge it nor need to carry a redundant ?ver= in every shared link. The public
# host has no prefix and keeps ?ver=. Either way the value goes through ver_of(),
# so the instance's access policy still decides.
ver_of_req <- function(req) {
  v <- tryCatch(req[["HTTP_X_MS_VERSION"]], error = function(e) NULL)
  if (!is.null(v) && nzchar(v)) ver_of(paste0("ver=", v)) else ver_of(req$QUERY_STRING)
}

# preview instance chrome: a badge naming the signed-in reviewer. The identity
# comes from Caddy's X-MS-User header, set from the VERIFIED Cloudflare Access
# JWT and only ever present on the page GET (the one request whose headers
# reach ui(req)); the public vhost strips it. Display only -- policy is
# MS_PREVIEW, above.
preview_badge <- function(req, ver) {
  # only for a release that IS restricted: on the preview host a public release
  # is just the app on another hostname, and labelling it PREVIEW says something
  # untrue about the data
  if (!msens::atlas_is_preview() || !identical(ver_access(ver), "restricted")) return(NULL)
  who <- tryCatch(req[["HTTP_X_MS_USER"]], error = function(e) NULL)
  span(class = "badge bg-warning text-dark ms-2",
       title = "restricted pre-release under review \u2014 sign-in required",
       "PREVIEW", if (!is.null(who) && nzchar(who)) glue(" \u00b7 {who}"))
}

# cross-product nav (apps#11) — the scores table links OUT to a species map and
# nothing led back, so a shared deep link was a one-way trip and the browser Back
# button the only exit. Destinations come from msens::product_urls(), the one
# definition the docs nav reads too, so the two cannot drift.
#
# Access is read from the INSTANCE, not from the registry: this app already knows
# which one it is running as, and on the preview instance a reviewer must stay on
# the signed-in host (where the version is the URL PATH) rather than be handed a
# public ?ver= link to a release the public host will not serve them. It also
# keeps this off the network, and this renders on every page request.
# Is the VERSION on screen restricted? Not "is this the preview instance" -- the
# preview instance also serves PUBLIC releases (its catch-all Access application
# covers any path without a version-specific one), and treating those as
# restricted got two things wrong at once on /v7/scores/: it badged a public
# release as PREVIEW, and it pointed that release's docs at the preview clone,
# which holds only restricted books -- so the welcome modal's figure 404'd.
# Access is a property of the release, and the registry is where it lives.
ver_access <- function(ver) tryCatch(msens::atlas_ver_access(ver), error = function(e) "public")

# the same source as product_nav(), for links OUTSIDE the nav (the welcome modal)
product_url <- function(ver, key) msens::product_urls(ver, access = ver_access(ver))[[key]]

# a tab title with a short form for phones: the four titles wrapped the card's
# tab strip onto two rows at 400px. CSS swaps which span shows; `value` (what the
# server keys on) is untouched.
tab_title <- function(long, short) span(
  span(class = "tab-long",  long),
  span(class = "tab-short", short))

product_nav <- function(ver, current) {
  u   <- msens::product_urls(ver, access = ver_access(ver))
  lnk <- function(key, label, ...) if (identical(key, current))
    span(class = "nav-here", title = "you are here", label) else
    tags$a(class = "nav-link-ms", href = u[[key]], title = u[[key]], label, ...)
  div(
    class = "header-nav",
    lnk("scores",  "Scores"),  span(class = "nav-sep", "\u00b7"),
    lnk("species", "Species"), span(class = "nav-sep", "\u00b7"),
    lnk("docs",    "Docs", target = "_blank"), span(class = "nav-sep", "\u00b7"),
    lnk("home",    "Home"))
}

ui_impl <- function(req) page_sidebar(
  # mobile: keep the page fillable so the map takes the viewport (the default,
  # FALSE, is why the map had zero height on a phone -- see the mobile CSS below)
  fillable_mobile = TRUE,
  tags$head(
    tags$link(rel = "icon", type = "image/x-icon", href = "favicon.ico"),
    # curl-checkable sentinels: WHICH release this page renders and WHETHER this
    # is the preview instance. The release checks (CHECK_PREVIEW in
    # workflows/release_marine-atlas.qmd) assert on these, so proving that the
    # public host never serves a restricted version needs no browser.
    tags$meta(name = "ms-ver",     content = ver),
    tags$meta(name = "ms-preview", content = if (msens::atlas_is_preview()) "1" else "0"),
    # usage tracking: GA4 (aggregate) + a batched beacon to the usage-log Sheet
    # (detail). Both legs are driven from the browser, so no reactive ever
    # performs network I/O — see msens::ga_js(). The Sheet leg is a silent no-op
    # unless MSENS_LOG_URL is set, so local dev writes nothing. Reviewer sessions
    # on the preview instance are tagged apart so they never mix into public counts.
    msens::ga_head(if (msens::atlas_is_preview()) "scores-preview" else "scores",
                   app_version = APP_VERSION,
                   ip = msens::ms_client_ip(req)),
    tags$style(HTML("
      .maplibregl-popup-content{color:black;}
      .bslib-full-screen .girafe_container_std {
        height: calc(100vh - 120px) !important;
        width: 100% !important;
      }
      .bslib-full-screen .card-body {
        height: calc(100vh - 120px) !important;
        display: flex;
        flex-direction: column;
      }
      .bslib-full-screen #plot_flower {
        height: 100% !important;
        flex: 1;
      }
      #plot_flower {
        height: 300px;
      }
      .header-right { margin-left: auto; display: flex; align-items: center; gap: 12px; }
      /* cross-product nav (apps#11) */
      .header-nav { display: flex; align-items: center; gap: 6px; margin-left: 18px; font-size: 0.9em; }
      .header-nav a.nav-link-ms { color: inherit; text-decoration: underline; text-underline-offset: 3px; }
      .header-nav a.nav-link-ms:hover { text-decoration-thickness: 2px; }
      .header-nav .nav-here { font-weight: 700; opacity: 0.8; }
      .header-nav .nav-sep  { opacity: 0.35; }
      .header-right .action-button { background: none; border: none; color: inherit; cursor: pointer; text-decoration: underline; font-size: 0.9em; padding: 0; }
      .modal-footer { flex-wrap: wrap; justify-content: center; }
      .modal-footer .form-group { width: 100%; margin-bottom: 0.5rem; }
      .map-container { position: relative; width: 100%; flex: 1 1 auto; min-height: 0; }
      .map-container > .html-widget { height: 100% !important; }
      .map-loading-overlay {
        position: absolute; inset: 0; z-index: 10;
        display: flex; flex-direction: column;
        align-items: center; justify-content: center;
        background: rgba(25, 25, 25, 0.85);
        color: #9ca3af; font-size: 0.9em; gap: 12px;
        transition: opacity 0.4s ease;
      }
      .map-loading-overlay.hidden { opacity: 0; pointer-events: none; }
      .map-loading-spinner {
        width: 36px; height: 36px;
        border: 3px solid rgba(255,255,255,0.12);
        border-top-color: #6ea8fe;
        border-radius: 50%;
        animation: msens-spin 0.8s linear infinite;
      }
      @keyframes msens-spin { to { transform: rotate(360deg); } }
      .tab-short { display: none; }
      /* ---- mobile (bslib's own sidebar breakpoint) -----------------------------
         The map used to be invisible on a phone: page_sidebar() is NOT fillable on
         mobile by default (.bslib-flow-mobile makes every fill item flex:0 0 auto,
         so the map card kept its intrinsic ~0 height) and sidebar(open = NULL)
         resolves to mobile = 'always' (stacked below main, no toggle). Both are now
         set in ui_impl (fillable_mobile = TRUE; open = list(mobile = 'closed')), so
         bslib draws its toggle row and overlays the sidebar on the map. What is left
         for CSS: fit the header and tab strip in ~400px and trim the page spacing. */
      /* sidebar hint: a pill to the right of bslib's collapse chevron while the sidebar is
         CLOSED, so a first-time visitor knows what the chevron opens. Pure CSS -- a ::after on
         the toggle, which the browser hit-tests as part of the button, so tapping the pill
         opens the sidebar too. No markup and no extra row: it lives in the toggle's own line
         (the mobile toggle row, or beside the collapsed sidebar's edge on desktop). */
      .bslib-page-sidebar > .bslib-sidebar-layout.sidebar-collapsed > .collapse-toggle::after {
        content: 'Map options';
        position: absolute; left: 100%; top: 50%; transform: translateY(-50%);
        margin-left: 6px; padding: 1px 9px; border-radius: 999px;
        font-size: 0.75rem; line-height: 1.4; white-space: nowrap;
        border: 1px solid rgba(128,128,128,0.6); background: rgba(128,128,128,0.15);
        color: inherit; opacity: 0.9; cursor: pointer;
      }
      .bslib-page-sidebar > .bslib-sidebar-layout.transitioning > .collapse-toggle::after { display: none; }
      /* desktop: a collapsed sidebar leaves only a 48px gutter, and a pill to the right of the
         chevron overlapped the first control (measured), so there it hangs BELOW the chevron as a
         vertical tab in that gutter. Phones keep it beside the chevron in the empty toggle row. */
      @media (min-width: 576px) {
        .bslib-page-sidebar > .bslib-sidebar-layout.sidebar-collapsed > .collapse-toggle::after {
          left: 50%; top: 100%; margin: 6px 0 0 0; transform: translateX(-50%);
          writing-mode: vertical-rl; padding: 9px 1px;
        }
      }
      .ms-header { display: flex; align-items: center; width: 100%; }
      @media (max-width: 575.98px) {
        /* --bslib-spacer is the page padding; --bslib-mb-spacer is the 1.5rem gap
           .bslib-gap-spacing puts between the main column's children (measured) */
        .bslib-page-sidebar { --bslib-spacer: 0.5rem; --bslib-mb-spacer: 0.5rem; }
        .bslib-sidebar-layout { --bslib-sidebar-padding: 0.5rem; }
        .bslib-card > .card-body { padding: 0.5rem; }
        .bslib-page-sidebar > .navbar { --bs-navbar-padding-y: 0.3rem; }
        .ms-header { flex-wrap: wrap; row-gap: 2px; }
        .ms-header .ms-title { flex: 1 1 auto; }
        .ms-header .header-right { flex: 0 0 auto; margin-left: auto; padding-left: 8px; }
        .ms-header .header-nav { order: 3; flex-basis: 100%; margin-left: 0; }
        .tab-long  { display: none; }
        .tab-short { display: inline; }
        .nav-tabs .nav-link { padding: 0.35rem 0.6rem; }
      }
    ")),
    tags$script(HTML("
      $(document).on('shiny:connected', function() {
        var params = new URLSearchParams(window.location.search);
        if (params.get('splash') === 'false') {
          Shiny.setInputValue('show_splash_pref', 'false');
        } else {
          var show = localStorage.getItem('msens_mapgl_show_splash');
          Shiny.setInputValue('show_splash_pref', show === null ? 'true' : show);
        }
      });
      Shiny.addCustomMessageHandler('saveSplashPref', function(val) {
        localStorage.setItem('msens_mapgl_show_splash', val);
      });

      // hide a map loading overlay with a fade-out transition
      Shiny.addCustomMessageHandler('hideMapOverlay', function(id) {
        var el = document.getElementById(id);
        if (el) el.classList.add('hidden');
      });

      // open a rendered-report URL in a new browser tab.
      //
      // Modern browsers block window.open() calls that arent in the
      // same tick as a user gesture. The /report round-trip takes
      // tens of seconds, so by the time the promise resolves the
      // user-activation window is long gone and the popup is blocked
      // on the first click. Workaround: when the Generate report
      // button is clicked, open an about:blank tab synchronously
      // (while we still have user activation) and stash the window
      // reference. When the /report response finally arrives, point
      // that stashed window at the real URL -- no popup check, no
      // second-click-to-open dance.
      //
      // Multiple concurrent reports: a queue + map keyed by reqId
      // ensures each response targets the correct placeholder tab.
      window._msens_report_pending = [];
      window._msens_report_wins = {};
      $(document).on('click', '#btn_rpt_submit', function() {
        try {
          var w = window.open('', '_blank');
          if (w && w.document) {
            w.document.title = 'Generating report\u2026';
            w.document.body.innerText =
              'Generating report \u2014 this tab will close when the ' +
              'report is finished and begins downloading (usually a couple of minutes). You ' +
              'can keep using the app in the meantime.';
          }
          window._msens_report_pending.push(w);
        } catch (e) {
          window._msens_report_pending.push(null);
        }
      });
      Shiny.addCustomMessageHandler('setReportReqId', function(reqId) {
        var w = window._msens_report_pending.shift() || null;
        if (w) window._msens_report_wins[reqId] = w;
      });
      Shiny.addCustomMessageHandler('openUrl', function(msg) {
        var url   = msg.url;
        var reqId = msg.reqId;
        var w     = window._msens_report_wins[reqId];
        delete window._msens_report_wins[reqId];
        if (w && !w.closed) {
          try { w.location.href = url; } catch (e) {}
          // close placeholder tab after download starts (server sends
          // Content-Disposition: attachment so the tab won't navigate)
          setTimeout(function() { if (w && !w.closed) w.close(); }, 2000);
        } else {
          // fallback: hidden anchor click triggers download via
          // Content-Disposition header from the file server
          var a = document.createElement('a');
          a.href = url;
          a.style.display = 'none';
          document.body.appendChild(a);
          a.click();
          setTimeout(function() { document.body.removeChild(a); }, 100);
        }
      });

      // program area tooltip lookup (updated from server)
      var praTooltips = {};
      var praPopup = null;
      var praHandlersAdded = {};   // layer id -> handlers bound
      // Tooltips for whichever zone unit is being drawn. The layer id and the
      // key/name property names arrive WITH the data: they were hardcoded to
      // 'pra_lyr' + programarea_*, so any other unit hovered silently to nothing.
      Shiny.addCustomMessageHandler('setPraTooltips', function(data) {
        praTooltips = data.tips || data;
        var layer = data.layer || 'programarea_fill';
        var kProp = data.keyProp || 'programarea_key';
        var nProp = data.nameProp || 'programarea_name';
        var widget = HTMLWidgets.find('#map');
        if (!widget) return;
        var map = widget.getMap();
        if (!map) return;
        if (!praPopup) praPopup = new maplibregl.Popup({
          closeButton: false, closeOnClick: false
        });
        // bind once PER LAYER: switching units adds a new layer id, and the old
        // handler must keep working if the user switches back
        praHandlersAdded = praHandlersAdded || {};
        if (praHandlersAdded[layer]) return;
        praHandlersAdded[layer] = true;
        map.on('mousemove', layer, function(e) {
          if (!e.features || !e.features.length) return;
          var key = e.features[0].properties[kProp];
          var tip = praTooltips[key] || e.features[0].properties[nProp] || key;
          map.getCanvas().style.cursor = 'pointer';
          praPopup.setLngLat(e.lngLat).setHTML(tip).addTo(map);
        });
        map.on('mouseleave', layer, function() {
          map.getCanvas().style.cursor = '';
          praPopup.remove();
        });
      });
    "))
  ),
  useConductor(),
  # the version this PAGE was resolved for, signed (msens::ver_token_sign): the
  # server function renders the version in this token, not whatever the client's
  # JavaScript reports as url_search/url_pathname -- see ver_of_session() below.
  # A hidden bound text input, so its value arrives in Shiny's init message.
  div(style = "display:none",
      tags$input(id = "ms_ver_token", type = "text", value = msens::ver_token_sign(ver))),
  title = div(
    class = "ms-header",
    span(class = "ms-title",
         "BOEM Marine Sensitivity ",
         actionLink("show_versions", glue("({ver})"),
                    title = "data version - click to switch"),
         preview_badge(req, ver)),
    product_nav(ver, "scores"),
    div(
      class = "header-right",
      actionLink("btn_about", "About"),
      input_dark_mode(id = "tgl_dark", mode = "dark")
    )
  ),
  # Without this, bslib derives the browser/bookmark title by flattening the
  # `title` argument -- and since ours is a div containing the dark-mode toggle,
  # the tab read "BOEM Marine Sensitivity (v8) About bslib-component-js 0.12.0
  # components/dist components.min.js web-components.min.js module bslib".
  # Carries the version, so a bookmark says which release it points at.
  window_title = glue("BOEM Marine Sensitivity ({ver})"),
  sidebar = sidebar(
    # on a phone the sidebar collapses to bslib's toggle row and opens as an
    # overlay on the map; the default (mobile = 'always') stacked it below a
    # zero-height map with no way to close it
    open = list(desktop = "open", mobile = "closed"),
    tags$div(
      id = "tour_subregion",
      selectInput(
        "sel_subregion",
        "Study area",
        choices = sr_choices
      )
    ),
    tags$div(
      id = "tour_unit",
      selectInput(
        "sel_unit",
        "Spatial units",
        # Every spatial unit this release actually scores AND can draw, from
        # `zone_units`. Nothing is hardcoded: v1 offers Planning Areas and
        # Ecoregions (it has no Program Areas at all), v8 adds Ecoregions and
        # Subregions beside them, and a zone type nobody has invented yet will
        # appear on its own once a release scores it.
        choices = c(
          setNames("cell", "Raster cells (0.05°)"),
          if (!is.null(zone_units)) setNames(zone_units$type, zone_units$label))
      )
    ),
    # A release scoring a DIFFERENT primary unit than the current programme's
    # should say so, so a reader does not assume Program Areas are merely missing.
    if (!is.null(primary_unit) && primary_unit != "programarea")
      tags$div(
        style = "font-size:0.85em; opacity:0.75; margin:-0.5em 0 1em 0;",
        sprintf("%s predates the BOEM Program Areas — it reports on %s.",
                ver, tolower(zone_units$label[1]))),
    tags$div(
      id = "tour_lyr",
      selectInput(
        "sel_lyr",
        "Layer",
        choices = lyr_choices,
        selected = lyr_default
      )
    ),
    selectInput(
      "sel_palette",
      "Color palette",
      choices  = palette_choices,
      selected = "spectral_r"
    ),
    input_switch(
      "tgl_sphere",
      "Sphere",
      T
    ),
  ),

  navset_card_tab(
    id          = "main_tabs",
    full_screen = TRUE,
    nav_panel(
      title = "Map",
      value = "Map",
      div(class = "map-container",
        div(id = "map-overlay", class = "map-loading-overlay",
          div(class = "map-loading-spinner"),
          span("Loading map\u2026")),
        map_output("map"))
    ),
    nav_panel(
      title = tab_title("Plot of Scores", "Plot"),
      value = "Plot of Scores",
      card(
        full_screen = T,
        card_header(textOutput("flower_panel_title", inline = TRUE)),
        card_body(
          girafeOutput("plot_flower", height = "100%")
        )
      )
    ),
    nav_panel(
      title = tab_title("Table of Species", "Table"),
      value = "Table of Species",
      card(
        card_header(
          span(
            textOutput("spp_tbl_hdr", inline = T),
            actionButton(
              "btn_tbl_info",
              "",
              icon = icon("circle-info"),
              class = "btn-sm"
            )
          ),
          class = "d-flex justify-content-between align-items-center",
          downloadButton("download_tbl", "Download CSV", class = "btn-sm")
        ),
        card_body(
          navset_card_tab(
            nav_panel(
              "Table",
              DTOutput("spp_tbl")
            ),
            nav_panel(
              "Composition",
              "NOTE: The 'bird' component has yet to be added to this visualization.",
              plotlyOutput("spp_comp")
            )
          )
        )
      )
    ),
    # Report tab ----
    # Build a list of labeled areas (drawn polygons and/or selected
    # Program Areas) and submit to the parameterized Quarto report
    # endpoint. Drawing lives here, not on the Map tab.
    nav_panel(
      title = "Report",
      value = "Report",
      layout_sidebar(
        sidebar = sidebar(
          width = 360,
          open  = list(desktop = "open", mobile = "closed"),
          textInput(
            "rpt_title",
            "Report title",
            value = "BOEM Marine Sensitivity Report"),
          selectInput(
            "rpt_ver",
            "Data version",
            # From the published registry, not a hand-kept list. The hardcoded
            # one offered "v4c", which has never existed, while omitting v1, v2
            # and v4 -- so a user could request a report for a version that is
            # not there and could not request three that are. Defaults to the
            # version currently on screen.
            choices  = rpt_ver_choices,
            selected = ver),
          radioButtons(
            "rpt_format",
            "Output format",
            choices  = c("HTML" = "html",
                         "Word (.docx)" = "docx",
                         "PDF" = "pdf"),
            selected = "html",
            inline   = TRUE),
          hr(),
          tags$h5("Add area"),
          tags$p(class = "text-muted small",
            "Draw a polygon on the map OR click a Program Area ",
            "(set Spatial units = Program areas in the sidebar), then:"),
          textInput("rpt_area_label", "Label for next area",
                    value = "Area 1"),
          div(class = "d-flex gap-2 mb-2",
            actionButton("btn_add_drawn", "Add drawn polygon",
                         icon  = icon("plus"),
                         class = "btn-sm btn-outline-primary"),
            actionButton("btn_add_pra", "Add selected Program Area",
                         icon  = icon("plus"),
                         class = "btn-sm btn-outline-primary")),
          hr(),
          tags$h5("Areas"),
          uiOutput("rpt_areas_ui"),
          hr(),
          actionButton("btn_rpt_submit", "Generate report",
                       class = "btn-primary w-100",
                       icon  = icon("file-export"))
        ),
        div(class = "map-container", style = "height: 700px;",
          div(id = "map-rpt-overlay", class = "map-loading-overlay",
            div(class = "map-loading-spinner"),
            span("Loading map\u2026")),
          map_output("map_rpt", height = "700px"))
      )
    )
  )
)

# server ----
server_impl <- function(input, output, session) {

  # version picker ----
  # One app renders any published release, so the header says which one is on
  # screen and offers the rest. Markup comes from msens::version_picker_html()
  # off the same versions.json the pipeline and docs read, so the three cannot
  # disagree about what exists.
  observeEvent(input$show_versions, {
    showModal(modalDialog(
      title = "Data version", easyClose = TRUE, size = "l",
      p("This app renders one published release of the Marine Sensitivity Toolkit."),
      tryCatch(
        # every row goes where THAT release lives: a public one to the public
        # host with ?ver=, a restricted one to the signed-in preview host by
        # path. Linking by instance instead sent a reviewer on the preview host
        # to /v7/scores/ -- which works only for an admin, since the per-version
        # Access applications do not cover a public release.
        msens::version_picker_html(
          ver, href = function(v) msens::product_urls(v, access = ver_access(v))[["scores"]]),
        error = function(e)
          p(class = "text-muted", "Version list unavailable: ", conditionMessage(e)))))
  })

  # ?ver= — the version is a URL parameter, not a fork of this app ----
  # Historically each MST release shipped as a FROZEN COPY of this app symlinked
  # at /scores_v{n}, so every improvement stranded in the newest fork. The
  # version is now data: `latest.txt` and `versions.json` say what exists, and
  # each release's manifest.json says how to draw it.
  #
  # This app can currently only RENDER the release it was built against, because
  # a version's tables and COGs have to be published before they can be shown --
  # the v1-v7 backfill is what unlocks the rest. So the contract is established
  # here (accept, validate, echo back into the URL) and an explicit request for a
  # version that is not yet served says so plainly, rather than silently drawing
  # the wrong one under the right label.
  observeEvent(session$clientData$url_search, once = TRUE, {
    q   <- parseQueryString(session$clientData$url_search)
    req <- q$ver
    if (is.null(req) || !nzchar(req)) return(invisible())

    resolved <- tryCatch(
      msens::atlas_resolve_ver(req, allow_access = msens::atlas_allow_access()),
      msens_restricted = function(e) e, error = function(e) NULL)
    if (inherits(resolved, "msens_restricted")) {
      # a pre-release under review: say so, and point at the door (the version
      # itself was already refused by ver_of(), so `ver` is the promoted release)
      pv <- tryCatch(msens::preview_app_url("scores", req), error = function(e) msens::atlas_preview_url())
      showModal(modalDialog(
        title = glue("Version {htmltools::htmlEscape(req)} is under review"), easyClose = TRUE,
        p(HTML(glue("<code>?ver={htmltools::htmlEscape(req)}</code> is a pre-release restricted ",
                    "to reviewers. Showing <b>{ver}</b>."))),
        p("Reviewers sign in at ", a(href = pv, target = "_blank", pv), ".")))
    } else if (is.null(resolved)) {
      showModal(modalDialog(
        title = "Unknown data version", easyClose = TRUE,
        p(HTML(glue("<code>?ver={htmltools::htmlEscape(req)}</code> is not a published ",
                    "version. Showing <b>{ver}</b>."))),
        p("Published versions are listed at ",
          a(href = paste0(msens::atlas_base_url(), "/versions.json"),
            target = "_blank", "versions.json"), ".")))
    } else if (!identical(resolved, ver)) {
      showModal(modalDialog(
        title = glue("Version {resolved} is not served here yet"), easyClose = TRUE,
        p(HTML(glue("This app currently renders <b>{ver}</b>. Support for rendering ",
                    "any published version is in progress: {resolved}'s layers have to be ",
                    "published before they can be drawn."))),
        p("Showing ", tags$b(ver), " instead.")))
    }
  })

  # canonicalise the URL to the path form, so a shared link says which release it
  # shows. A bare /scores/ visit resolves the promoted release and then reads
  # /v7/scores/; a versioned path is already canonical and this is a no-op. Same
  # shape on both hosts, so no instance branch -- and RELATIVE, because an
  # absolute URL would be cross-origin on the preview host and replaceState
  # throws on those.
  observe({
    updateQueryString(sprintf("/%s/scores/", ver), mode = "replace", session = session)
  })

  # observe(session$setCurrentTheme(
  #   if (isTRUE(input$tgl_dark)) dark else light
  # ))

  # usage tracking ----
  # msens::ms_track() only pushes a websocket message the session already has
  # open — it makes no HTTP request, so instrumenting a hot control (a layer
  # switch, a species pick) can't add latency to the reactive that follows.
  # `ignoreInit = TRUE` everywhere so app startup doesn't emit a burst of
  # synthetic "selections" the user never made.
  # push the session token (and a fallback IP) to the browser before any event,
  # so no logged row is missing them. The IP is only a fallback: the page
  # request already supplied the real one — see msens::ms_track_session().
  msens::ms_track_session(session)

  trk <- function(event, ...) msens::ms_track(session, event, ...)

  # which tab users actually work in (the navset already carries an id)
  observeEvent(input$main_tabs, trk("select_tab", tab = input$main_tabs),
               ignoreInit = TRUE)

  # sidebar controls — `sel_lyr` is the layer question; the rest give the
  # context a layer choice was made in.
  observeEvent(input$sel_subregion,
               trk("select_subregion", subregion = input$sel_subregion),
               ignoreInit = TRUE)
  observeEvent(input$sel_unit,
               trk("select_unit", unit = input$sel_unit),
               ignoreInit = TRUE)
  observeEvent(input$sel_lyr,
               trk("select_layer",
                   layer     = input$sel_lyr,
                   subregion = input$sel_subregion,
                   unit      = input$sel_unit),
               ignoreInit = TRUE)
  observeEvent(input$sel_palette,
               trk("select_palette", palette = input$sel_palette),
               ignoreInit = TRUE)

  # engagement / help
  observeEvent(input$btn_about, trk("open_about"),        ignoreInit = TRUE)
  observeEvent(input$btn_tour,  trk("start_tour"),        ignoreInit = TRUE)
  observeEvent(input$btn_tbl_info, trk("open_table_info"), ignoreInit = TRUE)

  # report area building (the funnel into "Generate report")
  observeEvent(input$btn_add_drawn,
               trk("report_add_area", area_type = "drawn"), ignoreInit = TRUE)
  observeEvent(input$btn_add_pra,
               trk("report_add_area", area_type = "program_area"), ignoreInit = TRUE)

  # show_welcome helper ----
  show_welcome <- function() {
    showModal(modalDialog(
      title     = "Welcome to BOEM Marine Sensitivity",
      size      = "m",
      easyClose = TRUE,
      tags$div(
        style = "text-align: left;",
        tags$img(
          # the figure ships inside each release's book; the unversioned
          # /docs/figures/... path stopped existing when the docs went
          # per-version and the CI pruned the flat root
          src   = paste0(product_url(ver, "docs"), "figures/overview-methods.svg"),
          style = "max-width: 80%; height: auto; max-height: 300px; margin-bottom: 10px;",
          alt   = "Marine Sensitivity Methods Overview"),
        tags$p(
          "Explore composite sensitivity scores across US Program Areas,",
          "component scores, and species found in cells or Program Areas. Also see:"),
        tags$ul(
          tags$li(tags$a(
            href   = product_url(ver, "species"),
            target = "_blank",
            "Species app"), " for mapping individual species distributions"),
          tags$li(tags$a(
            href   = product_url(ver, "docs"),
            target = "_blank",
            "Documentation"), " for methods and data sources"))),
      footer = tagList(
        checkboxInput(
          "chk_show_splash",
          tagList(
            "Show this welcome screen on startup", br(),
            helpText("Click About (upper right) to revisit this screen later")),
          value = TRUE),
        actionButton("btn_tour", "Take a Tour", icon = icon("route")),
        modalButton("Explore")
      )
    ))
  }

  # welcome modal on startup ----
  observe({
    if (isTRUE(input$show_splash_pref == "true"))
      show_welcome()
  }) |> bindEvent(input$show_splash_pref, once = TRUE)

  # about link ----
  observe({ show_welcome() }) |> bindEvent(input$btn_about)

  # save splash preference ----
  observe({
    session$sendCustomMessage(
      "saveSplashPref",
      tolower(as.character(input$chk_show_splash)))
  }) |> bindEvent(input$chk_show_splash)

  # conductor tour ----
  tour <- Conductor$new()$
    step(
      title    = "Study Area",
      text     = "Pick a region to focus on. 'Full study area' shows all US federal waters from the Aleutians to the Caribbean and out to the Pacific Island Territories. The other choices zoom into a single subregion.",
      el       = "#tour_subregion",
      position = "right")$
    step(
      title    = "Spatial Units",
      text     = "Toggle between fine-grained raster cells (0.05\u00b0) and aggregated BOEM Program Area polygons. Cell mode lets you click any pixel; Program Area mode shows pre-aggregated zone scores.",
      el       = "#tour_unit",
      position = "right")$
    step(
      title    = "Layer Selection",
      text     = "Choose which sensitivity metric to display \u2014 composite score, individual species categories (bird, fish, mammal, etc.), or primary productivity. Cells in the Atlantic, Gulf of America, Hawaii, Puerto Rico and the Pacific Islands are scored and drawn even though they lie outside every BOEM Program Area \u2014 the Study area selector only moves the camera, it never hides data. Turn on the 'Cells outside Program Areas' layer to see which those are.",
      el       = "#tour_lyr",
      position = "right")$
    step(
      title    = "Map tab",
      text     = "The Map tab is where you explore scores spatially \u2014 click cells or click Program Areas to see their scores.",
      el       = "[data-value='Map'].nav-link",
      position = "bottom")$
    step(
      title    = "Layers control",
      text     = "Toggle individual map layers on and off \u2014 program area outlines, ecoregion outlines, raster cells, and the gray 'Cells outside Program Areas' overlay.",
      el       = ".layers-control",
      position = "right")$
    step(
      title    = "Go to location",
      text     = "Search for a place name and the map will fly there \u2014 useful for jumping to a specific Program Area, port, or feature.",
      el       = ".maplibregl-ctrl-geocoder",
      position = "left")$
    step(
      title    = "Full screen",
      text     = "Expand the map to fill the entire window.",
      el       = ".maplibregl-ctrl-fullscreen",
      position = "left")$
    step(
      title    = "Zoom in / out",
      text     = "Zoom and reset the view. You can also use the mouse wheel, pinch gesture, or double-click.",
      el       = ".maplibregl-ctrl-zoom-in",
      position = "left")$
    step(
      title    = "Plot of Scores tab",
      text     = "Switch here to see the flower plot of aggregated sensitivity scores for the current selection, broken out by species category. Petal length = score (0\u2013100); center number = weighted mean. Defaults to 'All USA' until you click a cell or click a Program Area.",
      el       = "[data-value='Plot of Scores'].nav-link",
      position = "bottom")$
    step(
      title    = "Table of Species tab",
      text     = "A sortable, downloadable table of every species in the currently selected area, with extinction-risk codes, areas, and per-category contributions.",
      el       = "[data-value='Table of Species'].nav-link",
      position = "bottom")$
    step(
      title    = "Report tab",
      text     = "Generate a sensitivity report for one or more custom areas. Build up a list of labeled areas by drawing a polygon on the map (using the polygon tool) and/or by selecting a Program Area, then clicking 'Add' for each. Set a title, data version, and output format (HTML, Word, or PDF), then click 'Generate report'.",
      el       = "[data-value='Report'].nav-link",
      position = "bottom")
  tour$init()
  if (verbose) {
    message("conductor tour initialized")
  }

  observe({
    if (verbose) {
      message("starting conductor tour")
    }
    removeModal()
    session$onFlushed(
      function() {
        tour$start()
        if (verbose) message("conductor tour started")
      },
      once = TRUE
    )
  }) |>
    bindEvent(input$btn_tour)

  # reactive values ----
  # A clicked zone carries its unit, so the key/name columns follow from it
  # rather than being assumed to be programarea_*.
  zone_unit_of  <- function(z) if (is.null(z)) NULL else z$unit %||% primary_unit
  zone_key_of   <- function(z) {
    u <- zone_unit_of(z); if (is.null(u) || is.null(z)) return(NULL)
    z$properties[[paste0(u, "_key")]]
  }
  zone_label_of <- function(z) {
    u <- zone_unit_of(z); if (is.null(u) || is.null(z)) return("")
    z$properties[[paste0(u, "_name")]] %||% z$properties[[paste0(u, "_key")]] %||% ""
  }

  rx <- reactiveValues(
    clicked_pa       = NULL,
    clicked_pra      = NULL,   # a clicked ZONE of any unit: list(properties, unit)
    clicked_cell     = NULL,
    rpt_areas        = list(),  # Report tab: list of {label, kind, value}
    spp_tbl          = NULL,
    spp_tbl_hdr      = NULL,
    spp_tbl_filename = NULL
  )

  # dynamic title shown in the flower panel drag handle
  output$flower_panel_title <- renderText({
    if (!is.null(rx$clicked_cell)) {
      glue("Cell {rx$clicked_cell$cell_id}")
    } else if (!is.null(rx$clicked_pra)) {
      zone_label_of(rx$clicked_pra)
    } else {
      # the plot is the whole study area regardless of where the camera is
      # pointing, so the title must not claim otherwise (apps#14)
      "Full study area (default)"
    }
  })

  # * get_rast_rx ----
  # returns a metadata list for the msens cell-tile layer; no terra raster
  # is materialized in R (the browser fetches tiles on demand from
  # titilecache). NULL when the unit is not "cell" (pa/pra use vector fills).
  get_rast_rx <- reactive({
    req(input$sel_subregion, input$sel_unit, input$sel_lyr, input$sel_palette)

    if (input$sel_unit != "cell") {
      return(NULL)
    }

    pal    <- input$sel_palette
    m_key  <- input$sel_lyr
    sr_key <- input$sel_subregion

    if (verbose) {
      message(glue(
        "get_rast_rx() lyr: {m_key} | subregion: {sr_key} | palette: {pal}"))
    }

    # fast path: the default layer + FULL subregion + default palette was pre-warmed at boot
    if (m_key == lyr_default && sr_key == sr_choices[[1]] && pal == "spectral_r") {
      return(list(
        m_key    = lyr_default,
        sr_key   = sr_choices[[1]],
        sql      = initial_sql,
        rescale  = initial_rescale,
        tile_url = initial_tile_url,
        # `view`, matching the slow path below. It returned `bbox` before, which
        # nothing read -- so meta$view was NULL on the default selection and
        # fly_to() got center = NULL. Latent until the bbox went away with the
        # Program-Area-derived extents (apps#14) and made it a hard error.
        view     = initial_view))
    }

    lt <- layer_tiles(m_key, sr_key, palette = pal)

    list(
      m_key    = m_key,
      sr_key   = sr_key,
      sql      = cell_sql(m_key, sr_key),
      rescale  = lt$rescale,
      tile_url = lt$url,
      view     = sr_view(sr_key))
  })

  # build_initial_map ----
  # construct the initial map state (base layers, controls) shared by the
  # Map tab and the Report tab's embedded map. The Report tab chains
  # add_draw_control() after calling this. The main cell-values layer is
  # an XYZ raster source backed by the msens TiTiler factory; only
  # viewport-intersecting tiles (~4-16 per initial load) are fetched.
  # mapgl declares every optional bundle in its widget definition, so a plain
  # maplibre() ships them whether or not the app calls them. Measured on a cold
  # load: 3.7 MB of JavaScript across 39 files, of which html2canvas (194 KB) and
  # the globe minimap (149 KB) are never touched -- `add_globe_minimap` and
  # `screenshot` appear zero times in this app. Dropping them is 343 KB less to
  # download and, more to the point, less to parse on the main thread.
  #
  # Conservative by construction: an unknown name is simply not matched, and
  # anything the app does use stays. If a feature is added later that needs one
  # of these, remove it from the list rather than working around this.
  build_initial_map <- function(sphere = TRUE) {
    n_cols <- 11
    cols_r <- get_pal_colors("spectral_r", n_cols)
    rng_r  <- signif(initial_rescale, digits = 3)

    maplibre(
      style      = carto_style("dark-matter"),
      projection = ifelse(sphere, "globe", "mercator")
    ) |>
      # see sr_view(): a centre+zoom cannot invert across the antimeridian the
      # way fit_bounds(bbox) did
      set_view(center = initial_view$center, zoom = initial_view$zoom) |>
      # A PMTiles source + outline per SCORED unit, so the choropleth can target
      # any of them later: a fill layer can only name a source that already
      # exists, and mapgl has no set_layer_visibility, so they are all created up
      # front. PMTiles are lazy -- an unused source fetches nothing.
      msens::add_pmline(Filter(Negate(is.null), c(
        # styled PER UNIT TYPE by msens::zone_style(), the one table the species app
        # draws from too: white Program Areas, black 3px Ecoregions, dashed unlabelled
        # Subregions. One white line for all three made them indistinguishable.
        if (!is.null(zone_units)) lapply(seq_len(nrow(zone_units)), function(i) c(list(
          url          = zone_units$url[i],
          source_layer = zone_units$source_layer[i],
          id           = paste0(zone_units$type[i], "_ln"),
          source_id    = paste0(zone_units$type[i], "_src")),
          msens::zone_line_args(zone_units$type[i]))),
        list(
          if (!is.null(er <- ztile("ecoregion", tbl_er)) &&
              !("ecoregion" %in% (zone_units$type %||% character())))
            c(er, list(id = "er_ln", source_id = "er_src", before_id = before_er),
              msens::zone_line_args("ecoregion")))))) |>
      msens::add_pmlabel(Filter(Negate(is.null),
        if (is.null(zone_units)) list() else lapply(seq_len(nrow(zone_units)), function(i) {
          la <- msens::zone_label_args(zone_units$type[i])
          if (is.null(la)) return(NULL)   # drawn without labels (subregions)
          pts <- zone_pts(zone_units$type[i])
          if (is.null(pts) || !nrow(pts)) return(NULL)
          c(list(source     = pts,
                 text_field = paste0(zone_units$type[i], "_key"),
                 id         = paste0(zone_units$type[i], "_lbl")),
            la)
        }))) |>
      msens::add_cell_tiles(
        initial_tile_url, raster_opacity = 0.6, before_id = before_r) |>
      msens::add_cell_tiles(
        outside_pra_tile_url,
        id             = "outside_pra_lyr",
        source_id      = "outside_pra_lyr",
        raster_opacity = 0.55,
        visibility     = "none",
        before_id      = before_r) |>
      mapgl::add_legend(
        get_lyr_name(lyr_default),
        values   = rng_r,
        colors   = cols_r,
        position = "bottom-right") |>
      add_fullscreen_control() |>
      add_navigation_control() |>
      add_scale_control() |>
      add_layers_control(
        layers = c(
          zone_ctrl_layers(),
          list("Raster cell values"          = "r_lyr",
               "Cells outside Program Areas" = "outside_pra_lyr"))) |>
      add_geocoder_control(placeholder = "Go to location")
  }

  # a draw_control spec the Map tab and Report tab share; only the Report
  # tab actually adds it (Task 4 removed drawing from the Map tab)
  add_msens_draw_control <- function(m) {
    m |> add_draw_control(
      position     = "top-right",
      fill_color   = "#fbb03b",
      line_color   = "#fbb03b",
      fill_opacity = 0.2,
      controls     = list(
        point              = FALSE,
        line_string        = FALSE,
        polygon            = TRUE,
        trash              = TRUE,
        combine_features   = FALSE,
        uncombine_features = FALSE))
  }

  # map ----
  output$map <- renderMaplibre({
    build_initial_map(sphere = input$tgl_sphere)
  })

  # hide the main map loading overlay once style.load fires
  observeEvent(input$map_zoom, {
    session$sendCustomMessage("hideMapOverlay", "map-overlay")
  }, once = TRUE, ignoreInit = FALSE)

  # rpt_map_loaded: one-shot trigger that flips the first time
  # map_rpt finishes its style.load (detected via the input$map_rpt_zoom
  # event mapgl JS emits from map.on("load", ...)). The main update
  # observer depends on it so that the *first* apply_*_update against
  # map_rpt runs after the widget exists, not before — previously the
  # Report map could be stranded with the cell raster layer because the
  # proxy messages arrived before pra_ln was registered on the client.
  rpt_map_loaded <- reactiveVal(0)
  observeEvent(input$map_rpt_zoom, {
    rpt_map_loaded(rpt_map_loaded() + 1)
    session$sendCustomMessage("hideMapOverlay", "map-rpt-overlay")
  }, once = TRUE, ignoreInit = FALSE)

  # update map ----
  # `input$main_tabs` is included so switching to the Report tab
  # re-applies the current layer state to map_rpt; `rpt_map_loaded()`
  # covers the initial-load case (see above).
  observeEvent(
    c(input$sel_subregion, input$sel_unit, input$sel_lyr, input$sel_palette,
      input$main_tabs, rpt_map_loaded()),
    {
      req(input$sel_subregion, input$sel_unit, input$sel_lyr)

      # explicitly reference either select to force update
      sr_key <- input$sel_subregion
      unit <- input$sel_unit
      lyr <- input$sel_lyr

      if (unit == "cell") {
        # * cell ----

        if (verbose) {
          message(glue("update map cell - beg"))
        }

        # rx$clicked_pa <- NULL
        rx$clicked_pra <- NULL

        # metadata for the tile-backed cell-values layer (no terra raster)
        meta   <- get_rast_rx()
        n_cols <- 11
        cols_r <- get_pal_colors(input$sel_palette, n_cols)
        rng_r  <- signif(meta$rescale, digits = 3)

        # applied to both the Map tab's proxy and the Report tab's
        # embedded map proxy so the Report map stays in sync with
        # sidebar selections.
        apply_cell_update <- function(map_proxy) {
          map_proxy |>
            clear_layer("pra_lyr") |>
            clear_layer("outside_pra_lyr") |>
            clear_layer("r_lyr") |>
            clear_layer("r_src") |>
            clear_legend() |>
            msens::add_cell_tiles(
              meta$tile_url, raster_opacity = 0.6, before_id = before_r) |>
            msens::add_cell_tiles(
              outside_pra_tile_url,
              id             = "outside_pra_lyr",
              source_id      = "outside_pra_lyr",
              raster_opacity = 0.55,
              visibility     = "none",
              before_id      = before_r) |>
            mapgl::add_legend(
              get_lyr_name(input$sel_lyr),
              values   = rng_r,
              colors   = cols_r,
              position = "bottom-right") |>
            mapgl::fly_to(center = meta$view$center, zoom = meta$view$zoom) |>
            clear_controls("layers") |>
            add_layers_control(
              layers = list(
                "Program Area outlines"       = "pra_ln",
                "Program Area labels"         = "pra_lbl",
                "Ecoregion outlines"          = "er_ln",
                "Raster cell values"          = "r_lyr",
                "Cells outside Program Areas" = "outside_pra_lyr"))
        }
        apply_cell_update(maplibre_proxy("map"))
        apply_cell_update(maplibre_proxy("map_rpt"))

        if (verbose) {
          message(glue("update map cell - end"))
        }
        # } else if (unit == "pa") {
        #   # * planarea ---
        #
        #   if (verbose) {
        #     message(glue("update map pa - beg"))
        #   }
        #
        #   rx$clicked_cell <- NULL
        #   rx$clicked_pra <- NULL
        #
        #   sr_bb <- sr |>
        #     filter(subregion_key == !!sr_key) |>
        #     st_shift_longitude() |>
        #     st_bbox() |>
        #     as.numeric()
        #
        #   pa_keys <- d_sr_pa |>
        #     filter(subregion_key == !!sr_key) |>
        #     pull(planarea_key)
        #   pa_filter <- c("in", "planarea_key", pa_keys)
        #   # TODO: consider filtering pa and er outlines too
        #
        #   # show planning area layer
        #   n_cols <- 11
        #   cols_r <- rev(RColorBrewer::brewer.pal(n_cols, "Spectral"))
        #
        #   # get range of planning area values
        #   rng_pa <- tbl(con_sdm, "zone") |>
        #     filter(
        #       fld == "planarea_key",
        #       value %in% pa_keys
        #     ) |>
        #     select(planarea_key = value, zone_seq) |>
        #     inner_join(tbl(con_sdm, "zone_metric"), by = join_by(zone_seq)) |> # zone_seq metric_seq  value
        #     inner_join(
        #       tbl(con_sdm, "metric") |>
        #         filter(metric_key == !!lyr),
        #       by = join_by(metric_seq)
        #     ) |>
        #     pull(value) |>
        #     range()
        #
        #   # colors
        #   cols_pa <- colorRampPalette(cols_r, space = "Lab")(n_cols)
        #   brks_pa <- seq(rng_pa[1], rng_pa[2], length.out = n_cols)
        #
        #   # remove layers if exist
        #   map_proxy |>
        #     clear_layer("r_lyr") |>
        #     clear_layer("pa_lyr") |>
        #     clear_layer("pra_lyr") |>
        #     clear_legend()
        #
        #   # add planning area fill layer
        #   map_proxy |>
        #     add_fill_layer(
        #       id = "pa_lyr",
        #       source = "pa_src",
        #       source_layer = "public.ply_planareas_2025",
        #       fill_color = interpolate(
        #         column = lyr,
        #         values = brks_pa,
        #         stops = cols_pa,
        #         na_color = "lightgrey"
        #       ),
        #       fill_outline_color = "white",
        #       tooltip = concat("Value: ", get_column(lyr)),
        #       hover_options = list(
        #         fill_color = "purple",
        #         fill_opacity = 1
        #       ),
        #       before_id = "pa_ln",
        #       filter = pa_filter
        #     ) |>
        #     mapgl::add_legend(
        #       get_lyr_name(input$sel_lyr),
        #       values = round(rng_pa, 1),
        #       colors = cols_pa,
        #       position = "bottom-right"
        #     ) |>
        #     mapgl::fit_bounds(sr_bb, animate = T)
        #   # TODO: sr_bb is odd when AK included
        #
        #   if (verbose) {
        #     message(glue("update map pa - end"))
        #   }
      } else {
        # * zone choropleth -- ANY scored spatial unit -------------------------
        #
        # One code path for Program Areas, Planning Areas, Ecoregions and
        # whatever a later release scores. `unit` is the zone TYPE, so the key
        # column, the tile layer and the label field all derive from it; nothing
        # here names a particular unit.
        zu <- zone_unit_row(unit)
        req(!is.null(zu))
        kcol <- paste0(unit, "_key")
        ncol <- paste0(unit, "_name")

        if (verbose) message(glue("update map {unit} - beg"))

        rx$clicked_cell <- NULL

        sr_v <- sr_view(sr_key)

        # ALL of the unit's zones, always. The study area used to filter this
        # too -- Program Areas were intersected with the subregion they belonged
        # to -- which is the same masking-by-study-area that apps#13 removed from
        # the raster, just applied to the outlines. It moves the camera; the map
        # shows what the release scored.
        zone_keys   <- zu$keys[[1]]
        zone_filter <- c("in", kcol, zone_keys)

        n_cols <- 11
        cols_r <- get_pal_colors(input$sel_palette, n_cols)

        d_zone <- zone_vals() |>
          filter(fld == !!kcol, value %in% !!zone_keys) |>
          select(zone_key = value, zone_seq) |>
          inner_join(zone_metric_vals(), by = join_by(zone_seq)) |>
          inner_join(
            tbl(con_sdm, "metric") |> filter(metric_key == !!lyr),
            by = join_by(metric_seq)) |>
          select(zone_key, value) |>
          collect()

        # An empty result is a real state, not an error: a release can carry a
        # unit that this particular LAYER was never computed for. Draw nothing and
        # say so, rather than calling range() on nothing -- which returns
        # c(Inf, -Inf) and renders an `InfM/-InfM` legend over a blank map, the
        # exact symptom that made the v1/v2 bug so hard to read.
        if (!nrow(d_zone)) {
          message(glue("no {lyr} values for {unit} in {ver} - nothing to draw"))
          showNotification(
            sprintf("%s has no %s values for %s.", ver, get_lyr_name(lyr), tolower(zu$label)),
            type = "warning", duration = 8)
          return()
        }

        rng_zone  <- range(d_zone$value, na.rm = TRUE)
        cols_zone <- colorRampPalette(cols_r, space = "Lab")(n_cols)
        d_zone <- d_zone |>
          mutate(
            val_scaled = (value - rng_zone[1]) / max(rng_zone[2] - rng_zone[1], 1e-6),
            col_idx    = pmin(pmax(round(val_scaled * (n_cols - 1)) + 1, 1), n_cols),
            fill_color = cols_zone[col_idx])

        # tooltips: name where the geometry has one, else the key
        pts  <- zone_pts(unit)
        look <- if (!is.null(pts) && nrow(pts)) {
          d <- st_drop_geometry(pts)
          setNames(as.character(d[[ncol]]), as.character(d[[kcol]]))
        } else character()
        zone_tooltip <- d_zone |>
          mutate(nm  = ifelse(zone_key %in% names(look), unname(look[zone_key]), zone_key),
                 tip = glue("{nm}: {round(value)}")) |>
          select(zone_key, tip) |> deframe() |> as.list()
        session$sendCustomMessage("setPraTooltips", list(
          tips = zone_tooltip, layer = paste0(unit, "_fill"),
          keyProp = kcol, nameProp = ncol))

        # applied to both the Map tab's proxy and the Report tab's embedded map
        apply_zone_update <- function(map_proxy) {
          m <- map_proxy |>
            clear_layer("r_lyr") |>
            clear_layer("r_src") |>
            clear_layer("outside_pra_lyr")
          # clear EVERY unit's fill, not just this one, so switching units cannot
          # leave the previous choropleth painted underneath
          for (t in zone_units$type) m <- m |> clear_layer(paste0(t, "_fill"))
          m <- m |>
            clear_legend() |>
            add_fill_layer(
              id           = paste0(unit, "_fill"),
              source       = paste0(unit, "_src"),
              source_layer = zu$source_layer,
              fill_color   = match_expr(
                column  = kcol,
                values  = d_zone$zone_key,
                stops   = d_zone$fill_color,
                default = "lightgrey"),
              fill_opacity       = 0.7,
              fill_outline_color = "white",
              hover_options      = list(fill_color = "purple", fill_opacity = 1),
              before_id = paste0(unit, "_ln"),
              filter    = zone_filter) |>
            msens::add_cell_tiles(
              outside_pra_tile_url,
              id             = "outside_pra_lyr",
              source_id      = "outside_pra_lyr",
              raster_opacity = 0.55,
              visibility     = "none",
              before_id      = before_r) |>
            mapgl::add_legend(
              get_lyr_name(input$sel_lyr),
              values   = round(rng_zone, 1),
              colors   = cols_zone,
              position = "bottom-right") |>
            mapgl::fly_to(center = sr_v$center, zoom = sr_v$zoom) |>
            clear_controls("layers")
          ctl <- c(
            zone_ctrl_layers(),
            setNames(list(paste0(unit, "_fill")), paste(zu$label, "values")),
            list("Cells outside Program Areas" = "outside_pra_lyr"))
          m |> add_layers_control(layers = ctl)
        }
        apply_zone_update(maplibre_proxy("map"))
        apply_zone_update(maplibre_proxy("map_rpt"))

        if (verbose) message(glue("update map {unit} - end"))
      }
    },
    ignoreInit = FALSE
  )

  # map_click ----
  observeEvent(input$map_click, {
    req(input$map_click)

    click <- input$map_click

    if (input$sel_unit == "cell") {
      rx$clicked_pa <- NULL

      # handle raster click
      lng <- click$lng
      lat <- click$lat

      # extract cell value at clicked location
      # Resolve the click through titiler, not a local raster.
      #
      # GET /cog/point/{lon},{lat} returns the pixel value, so the number in the
      # popup comes from the SAME surface the user is looking at. Reading a
      # raster in the app instead produced three separate bugs: the band is named
      # `r_cellid` on usa05 and `depth_mean` on global05 (so `$cell_id` was NULL),
      # longitude was shifted to 0-360 while both images are stored -180..180 (so
      # every Americas click sampled outside), and a SpatRaster cached across
      # sessions is a stale external pointer that SEGFAULTS the process.
      #
      # Two calls: the cell id from the grid's categorical cell-id COG (INT4U, no
      # overviews, so ids are never averaged), and the value from whichever COG
      # the layer is currently drawn from.
      cell_id <- msens::cog_point_value(
        msens::grid_cellid_url(msens::grid_for_ver(ver)), lng, lat)
      # arithmetic fallback: the grid registry defines the mapping exactly, so a
      # transient tile-server failure need not cost the user their click
      if (is.na(cell_id))
        cell_id <- msens::cell_from_lonlat(
          lng, lat, msens::grid_spec_for(msens::grid_for_ver(ver)))

      cur <- cog_of(input$sel_lyr %||% lyr_default,
                    input$sel_subregion %||% sr_choices[[1]])
      cell_val <- if (is.null(cur)) NA_real_ else
        msens::cog_point_value(cur$url, lng, lat)

      if (length(cell_id) == 1 && !is.na(cell_id)) {
        rx$clicked_cell <- list(
          lng = lng,
          lat = lat,
          cell_id = as.integer(cell_id),
          value = cell_val,
          lyr = input$sel_lyr
        )
      }
    } else {
      # handle program area click
      rx$clicked_cell <- NULL
      rx$clicked_pa <- NULL

      if (!is.null(input$map_feature_click)) {
        rx$clicked_pra <- list(
          id = input$map_feature_click$id,
          properties = input$map_feature_click$properties,
          unit = input$sel_unit
        )
      }
    }
  })

  # (drawn-polygon reactive removed — drawing lives on the Report tab now)

  # plot_flower ----
  output$plot_flower <- renderGirafe({
    # set height based on container size
    height <- "100%"

    if (input$sel_unit == "cell" && !is.null(rx$clicked_cell)) {
      # get data for cell
      cell_id <- rx$clicked_cell$cell_id
      lng <- rx$clicked_cell$lng |> round(3)
      lat <- rx$clicked_cell$lat |> round(3)

      if (verbose) {
        message(glue("Rendering flower plot for cell id: {cell_id}"))
      }

      # get species group scores for the cell
      d_fl <- tbl(con_sdm, "metric") |>
        filter(str_detect(metric_key, ".*_ecoregion_rescaled$")) |>
        left_join(
          cell_metric_vals(),
          by = "metric_seq"
        ) |>
        filter(cell_id == !!cell_id) |>
        select(metric_key, score = value) |>
        mutate(
          component = metric_key |>
            str_replace("extrisk_", "") |>
            str_replace("_ecoregion_rescaled", "") |>
            str_replace("_", " "),
          even = 1
        ) |>
        filter(component != "all") |>
        collect()

      if (nrow(d_fl) > 0) {
        return(
          d_fl |>
            plot_flower(
              fld_category = component,
              fld_height = score,
              fld_width = even,
              tooltip_expr = "{component}: {round(score, 2)}",
              title = glue("Cell ID: {cell_id} (x: {lng}, y: {lat})")
            ))
      }
      # } else if (input$sel_unit == "pa" && !is.null(rx$clicked_pa)) {
      #   # get data for planning area
      #   pa_name <- rx$clicked_pa$properties$planarea_name
      #   pa_key <- rx$clicked_pa$properties$planarea_key
      #
      #   l <- rx$clicked_pa$properties
      #   l <- l[str_detect(names(l), "_ecoregion_rescaled$")]
      #
      #   d_fl <- tibble(
      #     metric_key = names(l),
      #     score = unlist(l)
      #   ) |>
      #     mutate(
      #       component = metric_key |>
      #         str_replace("extrisk_", "") |>
      #         str_replace("_ecoregion_rescaled", "") |>
      #         str_replace("_", " "),
      #       even = 1
      #     ) |>
      #     filter(component != "all")
      #
      #   if (nrow(d_fl) > 0) {
      #     d_fl |>
      #       plot_flower(
      #         fld_category = component,
      #         fld_height = score,
      #         fld_width = even,
      #         tooltip_expr = "{component}: {round(score, 2)}",
      #         title = pa_name
      #       )
      #   }
    } else if (input$sel_unit != "cell" && !is.null(rx$clicked_pra)) {
      # get data for the clicked zone, whatever unit it belongs to
      pra_name <- zone_label_of(rx$clicked_pra)
      pra_key  <- zone_key_of(rx$clicked_pra)
      z_fld    <- paste0(zone_unit_of(rx$clicked_pra), "_key")

      if (verbose) {
        message(glue("Rendering flower plot for {z_fld}: {pra_name} ({pra_key})"))
      }

      # look up zone_seq by FIELD + key, not by table name: the table name is
      # version-suffixed on some releases and not others, which is what silently
      # emptied the subregion mapping on v2
      z_seq <- zone_vals() |>
        filter(fld == !!z_fld, value == !!pra_key) |>
        pull(zone_seq)

      if (length(z_seq) > 0) {
        d_fl <- tbl(con_sdm, "metric") |>
          filter(str_detect(metric_key, ".*_ecoregion_rescaled$")) |>
          left_join(
            zone_metric_vals(),
            by = "metric_seq") |>
          filter(zone_seq == !!z_seq) |>
          select(metric_key, score = value) |>
          mutate(
            component = metric_key |>
              str_replace("extrisk_", "") |>
              str_replace("_ecoregion_rescaled", "") |>
              str_replace("_", " "),
            even = 1) |>
          filter(component != "all") |>
          collect()

        if (nrow(d_fl) > 0) {
          return(
            d_fl |>
              plot_flower(
                fld_category = component,
                fld_height   = score,
                fld_width    = even,
                tooltip_expr = "{component}: {round(score, 2)}",
                title        = pra_name))
        }
      }
    }

    # ** subregion default ----
    # nothing clicked: read pre-cached flower data for the current subregion
    # zone (FULL falls back to USA). The cache is built at app startup from
    # zone_metric, populated by cell_metrics_to_zone_metrics in calc_scores.qmd.
    if (is.null(rx$clicked_cell) && is.null(rx$clicked_pra)) {
      # the whole study area, whatever the camera is pointing at (apps#14)
      z_sr_key <- zone_all_key
      sr_lbl   <- "Full study area"
      d_fl <- d_flower_default |>
        filter(subregion_key == !!z_sr_key) |>
        select(component, score, even)
      if (nrow(d_fl) > 0) {
        return(
          d_fl |>
            plot_flower(
              fld_category = component,
              fld_height   = score,
              fld_width    = even,
              tooltip_expr = "{component}: {round(score, 2)}",
              title        = sr_lbl))
      }
    }
  })

  # click_info ----
  output$click_info <- renderPrint({
    if (input$sel_unit == "cell" && !is.null(rx$clicked_cell)) {
      cat(
        "Location:",
        round(rx$clicked_cell$lng, 4),
        ",",
        round(rx$clicked_cell$lat, 4)
      )
    } else if (FALSE) {
      cat("Planning Area:", rx$clicked_pa$feature$properties$planarea_name)
    } else if (input$sel_unit != "cell" && !is.null(rx$clicked_pra)) {
      cat(paste0(zone_unit_row(zone_unit_of(rx$clicked_pra))$label %||% "Zone", ": "),
          zone_label_of(rx$clicked_pra))
    }
  })

  # spp_tbl ----

  # * spp_tbl_hdr ----
  output$spp_tbl_hdr <- renderText({
    req(rx$spp_tbl_hdr)
    rx$spp_tbl_hdr
  })

  # * btn_tbl_info ----
  observe({
    showModal(modalDialog(
      title = "Species table information",
      size = "l",
      easy_close = T,
      helpText(markdown(
        "Species are listed for the entire USA waters, or the currently
          selected area — a clicked Planning Area or cell. The columns correspond to:
          - `cat` species categorical component; one of: bird, coral, fish, invertebrate, mammal, reptile, other
          - `taxon` taxonomic identifier from Birds of the World (botw) or World Registry of Marine Species (worms) for non-bird species
          - `scientific` scientific name
          - `common` common name, if available
          - `er_code` extinction risk code with authority prefix: `NMFS` (National Marine Fisheries Service), `FWS` (Fish & Wildlife Service), or `IUCN` (International Union for Conservation of Nature), followed by status code — CR (Critically Endangered), EN (Endangered), VU (Vulnerable), TN (Threatened), NT (Near Threatened), LC (Least Concern), Data Deficient (DD). US national listings (NMFS, FWS) take precedence over IUCN.
          - `er_score` extinction risk score (1-100%): derived from the max of extinction risk codes (NMFS|FWS:EN=100, NMFS|FWS:TN=50, IUCN:CR=50, IUCN:EN=25, IUCN:VU=5, IUCN:NT=2, IUCN:LC|DD=1) and if protected under MMPA (20) or MBTA (10).
          - `is_mmpa` whether the species is protected under the Marine Mammal Protection Act (MMPA)
          - `is_mbta` whether the species is protected under the Migratory Bird Treaty Act (MBTA)
          - `model` model identifier; click to visit species distribution in seperate app
          - `area_km2` area of cells with non-zero value for distribution, within selected area (USA, Program Area or Cell)
          - `avg_suit` average suitability across all non-zero cells, ranging from 1 to 100%
          - `pct_cat` percent contribution of the species (`er_score * avg_suit * area_km2`) towards the total summed category (`cat`) within selected area (USA, Program Area or Cell).
             Note rescaling by Ecoregion min/max that contributes to the component and overall scores is not captured by this simpler metric."
      ))
    ))
  }) |>
    bindEvent(input$btn_tbl_info)

  # * get_spp_tbl ----
  get_spp_tbl <- reactive({
    # Species table computed LIVE via msens, not read from a precomputed table.
    #
    # v7 read `zone_taxon`, but v8 never builds one — and this query had been
    # duplicated inline here against v7 column names (is_ok, mdl_seq, value)
    # that v8 renamed. Both together meant the tab came up empty with
    # "Can't select columns that don't exist", and the CSV download could never
    # produce a file. msens::species_for_zone()/species_for_cells() are
    # schema-adaptive and unit-tested against BOTH schemas, so the app and the
    # tests can no longer drift. Measured on v8, the largest zone (subregion
    # USA, ~349k cells, ~10k species) takes ~5 s, so precomputation isn't needed.

    # ** cell ----
    if (input$sel_unit == "cell" && !is.null(rx$clicked_cell)) {
      cell_id <- rx$clicked_cell$cell_id
      if (verbose)
        message(glue("Getting species table for cell id: {cell_id}"))
      rx$spp_tbl_hdr      <- glue("Species for Cell ID: {cell_id}")
      rx$spp_tbl_filename <- glue("species_cellid-{cell_id}")
      # Served by the CELL-oriented `cell_model` surface: the same rows as
      # model_cell but partitioned by a 2.5-degree spatial tile and kept as LOCAL
      # Parquet on the server. msens::species_for_cells() picks it up
      # automatically and prunes to the relevant tiles. model_cell alone could
      # not answer this at all — it is partitioned by mdl_id for per-model tile
      # reads, so a per-cell question meant scanning ~580M rows over HTTPS.
      # tryCatch stays as a guard: a missing cell_model should explain itself
      # rather than surface a bare "An error has occurred".
      return(tryCatch(
        msens::species_for_cells(
          con_sdm,
          data.frame(cell_id = as.integer(cell_id), pct_covered = 100)),
        error = function(e) {
          showNotification(
            paste("Couldn't load species for this cell:", conditionMessage(e)),
            type = "warning", duration = 12)
          rx$spp_tbl_hdr <- glue("Species for Cell ID: {cell_id} — unavailable")
          NULL
        }))
    }

    # ** pra ----
    if (input$sel_unit != "cell" && !is.null(rx$clicked_pra)) {
      pra_key  <- zone_key_of(rx$clicked_pra)
      pra_name <- zone_label_of(rx$clicked_pra)
      if (verbose)
        message(glue("Getting species table for Program Area: {pra_name}"))
      rx$spp_tbl_hdr      <- glue("Species for Program Area: {pra_name}")
      rx$spp_tbl_filename <- glue(
        "species_programarea-{str_replace(pra_name, ' ', '-') |> str_to_lower()}")
      # species_for_zone() already takes the field, so the clicked unit's own
      # field is passed rather than assuming Program Areas
      return(msens::species_for_zone(
        con_sdm, paste0(zone_unit_of(rx$clicked_pra), "_key"), pra_key))
    }

    # ** whole study area ----
    # Nothing clicked: the species of everything this release scored. NOT scoped
    # by the study-area picker, which only moves the camera (apps#14) -- and
    # which offers keys like AT that a given release may not publish as a zone.
    z_sr_key <- zone_all_key
    sr_lbl   <- "Full study area"
    if (verbose)
      message(glue("Getting species table for subregion: {sr_lbl} ({z_sr_key})"))
    rx$spp_tbl_hdr      <- glue("Species in {sr_lbl}")
    rx$spp_tbl_filename <- glue("species_{z_sr_key}")
    msens::species_for_zone(con_sdm, "subregion_key", z_sr_key)
  })

  # rename columns for display: kept as its own reactive so the raw msens
  # output (used by the download + composition plot) stays unformatted.
  fmt_spp_tbl <- reactive({
    d_spp <- get_spp_tbl()
    if (is.null(d_spp) || !nrow(d_spp)) return(d_spp)
    # The public model id differs by generation: v8 keys on the stable `mdl_key`,
    # v1-v7 on `mdl_seq`, which is what the manifest's id_field records. Linking
    # unconditionally on mdl_key made the Table of Species error out on every
    # older release, because the column simply is not there.
    # msens::species_for_zone() now normalises every release to `mdl_key`, so this
    # resolves for all of them — kept adaptive so a raw v1-v7 frame still links.
    id_col <- if ("mdl_key" %in% names(d_spp)) "mdl_key" else
              if ("mdl_seq" %in% names(d_spp)) "mdl_seq" else NA_character_
    # rename columns
    d_spp |>
      mutate(
        # `../species/`, NOT `../species_v8/`: since the 2026-08-12 cutover there is
        # one app per app, and /species_v8 is a RETIRED path that Caddy 301s to
        # /species/?ver=v8 -- a redirect whose target carries its own query string,
        # so the model id was dropped in flight and the species app opened on its
        # default taxon (the leatherback turtle) whatever row was clicked (#6).
        # URL-encoded because a v8 mdl_key contains `|` and `:`.
        model_id  = if (is.na(id_col)) NA_character_ else as.character(.data[[id_col]]),
        # relative, so it stays inside /v{ver}/ on either host and the version
        # needs no repeating
        model_url = if (is.na(id_col)) NA_character_ else
          glue("../species/?mdl_key=",
               "{vapply(model_id, utils::URLencode, '', reserved = TRUE)}"),
        taxon_str = glue("{taxon_authority}:{taxon_id}"),
        taxon_url = ifelse(
          taxon_authority == "botw",
          "https://birdsoftheworld.org",
          glue(
            "https://www.marinespecies.org/aphia.php?p=taxdetails&id={taxon_id}"
          )
        )
      ) |>
      # TODO: construct URL
      # Search: "Limosa lapponica" taxon_id: 22693158
      # at https://birdsoftheworld.org/
      # https://birdsoftheworld.org/bow/api/v1/taxa?limit=100&q=Limosa%20lapponica
      # [{  "code": "batgod",
      #     "name": "Bar-tailed Godwit - Limosa lapponica",
      #     "order": 5973  }]
      # https://birdsoftheworld.org/bow/species/batgod/cur/introduction
      # https://www.iucnredlist.org/species/22693158/111221714
      # https://www.iucnredlist.org/species/22693158 taxon_id alone doesn't work
      select(
        component = sp_cat,
        taxon_authority,
        taxon_id,
        taxon_str,
        taxon_url,
        scientific = sp_scientific,
        common = sp_common,
        er_code,
        er_score,
        is_mmpa,
        is_mbta,
        model_id,
        model_url,
        area_km2,
        avg_suit,
        pct_component = pct_cat
      ) |>
      arrange(component, scientific)
  })

  # * spp_tbl ----
  output$spp_tbl <- renderDT(
    {
      d <- fmt_spp_tbl()

      # store for download
      rx$spp_tbl <- d

      d |>
        mutate(
          taxon = glue('<a href="{taxon_url}" target="_blank">{taxon_str}</a>'),
          model = glue('<a href="{model_url}" target="_blank">{model_id}</a>'),
        ) |>
        relocate(taxon, .after = component) |>
        relocate(model, .after = er_score) |>
        select(
          -taxon_id,
          -taxon_authority,
          -taxon_str,
          -taxon_url,
          -model_url,
          -model_id
        ) |>
        rename(cat = component, pct_cat = pct_component) |>
        datatable(
          escape = F,
          rownames = F,
          fillContainer = T,
          filter = "top",
          # style         = "auto",  # "bootstrap4",
          # style         = "bootstrap4",  # "auto",
          class = "display compact",
          extensions = c("ColReorder", "KeyTable", "Responsive"),
          options = list(
            colReorder = T,
            keys = T,
            pageLength = 5,
            lengthMenu = c(5, 50, 100),
            scrollX = TRUE, # scrollY    = "600px",
            dom = 'lfrtip'
          )
        ) |>
        formatPercentage(
          c(
            "er_score"
          ),
          0
        ) |>
        formatPercentage(
          c(
            "avg_suit",
            "pct_cat"
          ),
          2
        ) |>
        formatSignif(c("area_km2"), 4)
    },
    server = T
  )

  # * download_tbl ----
  output$download_tbl <- downloadHandler(
    filename = function() {
      rx$spp_tbl_filename |>
        paste0("_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(rx$spp_tbl_hdr, rx$spp_tbl)
      write_csv(rx$spp_tbl, file)
      # what was actually downloaded, and how much of it — tracked here rather
      # than on the button so a failed/aborted render isn't counted as a
      # download. The Sheet leg keeps the full (unbounded) area label that GA4
      # would bucket away.
      trk("download_species_csv",
          n_rows    = nrow(rx$spp_tbl),
          area      = rx$spp_tbl_hdr,
          subregion = input$sel_subregion,
          unit      = input$sel_unit,
          layer     = input$sel_lyr)
    }
  )

  # * spp_comp ----
  output$spp_comp <- renderPlotly({
    # TODO:
    # - [ ] birds
    # - [ ] ranks_with_variety
    # taxon_id arrives as integer from some releases and character from others,
    # and the authority is spelled "worms" or "WORMS" depending on generation --
    # an exact-type, exact-case join errored with "Can't join x$taxon_id with
    # y$species_id due to incompatible types" and took the Composition tab down.
    d <- fmt_spp_tbl() |>
      filter(tolower(taxon_authority) == "worms") |>
      mutate(taxon_id_chr = as.character(taxon_id)) |>
      inner_join(
        d_taxonomy |>
          select(-component) |>
          mutate(species_id_chr = as.character(species_id)) |>
          select(-species_id),
        by = join_by(taxon_id_chr == species_id_chr)
      ) |>
      mutate(
        name = glue("{scientific} ({common}; worms:{taxon_id})"),
        n = 1
      ) |>
      select(component, Kingdom, Phylum, Class, Order, Family, Genus, name, n)

    p <- count_to_treemap(d)

    theme <- bs_current_theme()
    if (input$tgl_dark == "dark") {
      bg <- bs_get_variables(theme, "body-bg-dark")[["body-bg-dark"]]
      fg <- bs_get_variables(theme, "body-color-dark")[["body-color-dark"]]
    } else {
      bg <- bs_get_variables(theme, "body-bg")[["body-bg"]]
      fg <- bs_get_variables(theme, "body-color")[["body-color"]]
    }

    p |>
      layout(
        font = list(
          color = fg
        ),
        plot_bgcolor = bg,
        paper_bgcolor = bg
      )
  })

  # Report tab ----

  # * map_rpt: embedded map with draw control ----
  # reuses build_initial_map() from the Map tab so layer/subregion/sphere
  # toggles in the sidebar apply here too; add_msens_draw_control() is
  # chained on top.
  output$map_rpt <- renderMaplibre({
    build_initial_map(sphere = input$tgl_sphere) |>
      add_msens_draw_control()
  })

  # * rpt_drawn_sf: most-recently-drawn polygon on map_rpt ----
  # mapgl's draw control pushes its FeatureCollection to
  # `input$map_rpt_drawn_features`. The SHAPE of that value depends on the
  # mapgl version — older builds stringified it, current builds send the
  # object (Shiny then delivers a list) — so parsing is msens::drawn_features_sf(),
  # which handles both and is unit-tested against both (test-drawn-features.R).
  # This app previously required `is.character()` and so silently ignored every
  # polygon drawn under the newer mapgl.
  rpt_drawn_sf <- reactive({
    sf_feats <- msens::drawn_features_sf(input$map_rpt_drawn_features)
    req(sf_feats)
    sf_feats[nrow(sf_feats), ]
  })

  # * rpt_areas_sf: sf of all currently-added Report areas ----
  # rebuilt whenever rx$rpt_areas changes. PRA polygons come from the
  # versioned gpkg loaded at startup (pra_full_sf); wkt areas are
  # parsed from their stored WKT strings.
  rpt_areas_sf <- reactive({
    areas <- rx$rpt_areas
    if (length(areas) == 0) return(NULL)
    rows <- purrr::map(areas, function(a) {
      g <- tryCatch(
        {
          if (identical(a$kind, "pra")) {
            # NULL when the release publishes no program-area geometry; the
            # tryCatch below turns that into a skipped row rather than a crash
            pg <- pra_geom()
            if (is.null(pg)) stop("no program-area geometry for this version")
            pg |>
              filter(programarea_key == a$value) |>
              st_geometry()
          } else {
            st_as_sfc(a$value, crs = 4326)
          }
        },
        error = function(e) NULL)
      if (is.null(g) || length(g) == 0) return(NULL)
      st_sf(label = a$label, kind = a$kind, geometry = g, crs = 4326)
    })
    rows <- Filter(Negate(is.null), rows)
    if (length(rows) == 0) return(NULL)
    do.call(rbind, rows)
  })

  # * render added Report areas on map_rpt with thick pink border ----
  # + symbol label at the polygon's point-on-surface. Re-runs whenever
  # rpt_areas_sf() invalidates; clears stale layers first.
  observe({
    sf_data <- rpt_areas_sf()
    proxy   <- maplibre_proxy("map_rpt")
    proxy |>
      clear_layer("rpt_added_lbl") |>
      clear_layer("rpt_added_ln") |>
      clear_layer("rpt_added_fill")
    if (is.null(sf_data) || nrow(sf_data) == 0) return()
    pts <- suppressWarnings(st_point_on_surface(sf_data))
    proxy |>
      add_fill_layer(
        id           = "rpt_added_fill",
        source       = sf_data,
        fill_color   = "#ff00aa",
        fill_opacity = 0.15) |>
      add_line_layer(
        id         = "rpt_added_ln",
        source     = sf_data,
        line_color = "#ff00aa",
        line_width = 4) |>
      add_symbol_layer(
        id              = "rpt_added_lbl",
        source          = pts,
        # get_column() emits ["get","label"] so the text resolves
        # per-feature; a bare string would render the literal "label".
        text_field      = get_column("label"),
        text_size       = 14,
        text_color      = "#ffffff",
        text_halo_color = "#ff00aa",
        text_halo_width = 2,
        text_offset     = c(0, -1))
  })

  # * map_rpt_click: register Program Area clicks on the Report map ----
  # mirrors the main Map tab's click handler so `btn_add_pra` works after
  # clicking a Program Area on either map.
  observeEvent(input$map_rpt_click, {
    req(input$map_rpt_click)
    if (input$sel_unit == "cell") return()
    if (is.null(input$map_rpt_feature_click)) return()
    rx$clicked_cell <- NULL
    rx$clicked_pa   <- NULL
    rx$clicked_pra  <- list(
      id         = input$map_rpt_feature_click$id,
      properties = input$map_rpt_feature_click$properties)
  })

  # * prepopulate the "Label for next area" input with the clicked
  # Program Area name so the user can click > Add without typing.
  observeEvent(rx$clicked_pra, {
    props <- rx$clicked_pra$properties
    req(props)
    nm <- zone_label_of(rx$clicked_pra)
    if (!is.null(nm) && nzchar(nm))
      updateTextInput(session, "rpt_area_label", value = nm)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # * highlight the currently-clicked Program Area on both maps with a
  # thick bright-pink border so it stands out against the Spectral
  # cell colormap. Applies to the Map tab and the Report tab's
  # embedded map; clears the highlight when rx$clicked_pra is NULL.
  observe({
    clicked <- rx$clicked_pra
    u_hl <- zone_unit_of(clicked)
    zu_hl <- zone_unit_row(u_hl)
    key <- zone_key_of(clicked)
    for (mid in c("map", "map_rpt")) {
      proxy <- maplibre_proxy(mid)
      proxy |> clear_layer("pra_highlight_ln")
      if (!is.null(key) && nzchar(key) && !is.null(zu_hl)) {
        proxy |> add_line_layer(
          id           = "pra_highlight_ln",
          source       = paste0(u_hl, "_src"),
          source_layer = zu_hl$source_layer,
          line_color   = "#ff00aa",
          line_width   = 4,
          filter       = list("==", paste0(u_hl, "_key"), key))
      }
    }
  })

  # * highlight the currently-clicked cell on both maps with a bright
  # pink circle marker at the click coordinates. Simpler than drawing
  # the cell polygon since the raster uses 0-360 longitudes and would
  # need shifting. Cleared when rx$clicked_cell is NULL.
  observe({
    clicked <- rx$clicked_cell
    pt <- if (!is.null(clicked))
      st_sf(
        cell_id  = clicked$cell_id,
        geometry = st_sfc(st_point(c(clicked$lng, clicked$lat)), crs = 4326))
    for (mid in c("map", "map_rpt")) {
      proxy <- maplibre_proxy(mid)
      proxy |> clear_layer("cell_highlight")
      if (!is.null(pt)) {
        proxy |> add_circle_layer(
          id                  = "cell_highlight",
          source              = pt,
          circle_color        = "#ffffff",
          circle_opacity      = 0.4,
          circle_radius       = 8,
          circle_stroke_color = "#ff00aa",
          circle_stroke_width = 4)
      }
    }
  })

  # * btn_add_drawn ----
  observeEvent(input$btn_add_drawn, {
    p   <- tryCatch(rpt_drawn_sf(), error = function(e) NULL)
    if (is.null(p)) {
      showNotification(
        "Draw a polygon on the map first.",
        type = "warning")
      return()
    }
    wkt <- sf::st_as_text(sf::st_geometry(p))
    if (nchar(wkt) > 8000) {
      p   <- sf::st_simplify(p, dTolerance = 0.01, preserveTopology = TRUE)
      wkt <- sf::st_as_text(sf::st_geometry(p))
      showNotification(
        "Polygon simplified to fit request.",
        type = "warning")
    }
    lbl <- input$rpt_area_label %||% paste0("Area ", length(rx$rpt_areas) + 1)
    rx$rpt_areas <- c(
      rx$rpt_areas,
      list(list(label = lbl, kind = "wkt", value = wkt)))
    updateTextInput(
      session, "rpt_area_label",
      value = paste0("Area ", length(rx$rpt_areas) + 1))
  })

  # * btn_add_pra ----
  observeEvent(input$btn_add_pra, {
    if (is.null(rx$clicked_pra)) {
      showNotification(
        "Click a Program Area on the map first (set Spatial units = Program areas).",
        type = "warning")
      return()
    }
    props <- rx$clicked_pra$properties
    key   <- props$programarea_key %||% props$planarea_key
    nm    <- props$programarea_name %||% props$planarea_name
    lbl   <- if (nzchar(input$rpt_area_label)) input$rpt_area_label else nm
    rx$rpt_areas <- c(
      rx$rpt_areas,
      list(list(label = lbl, kind = "pra", value = key)))
    updateTextInput(
      session, "rpt_area_label",
      value = paste0("Area ", length(rx$rpt_areas) + 1))
  })

  # * rpt_areas_ui: list of added areas with delete buttons ----
  output$rpt_areas_ui <- renderUI({
    areas <- rx$rpt_areas
    if (length(areas) == 0)
      return(tags$p(class = "text-muted small", "No areas yet."))
    tagList(lapply(seq_along(areas), function(i) {
      a <- areas[[i]]
      kind_lbl <- switch(a$kind, pra = "Program Area", wkt = "drawn", a$kind)
      div(
        class = "d-flex align-items-center mb-1",
        tags$span(
          class = "flex-grow-1 small",
          sprintf("%d. %s (%s)", i, a$label, kind_lbl)),
        actionButton(
          paste0("rpt_del_", i),
          "",
          icon  = icon("trash"),
          class = "btn-sm btn-outline-danger"))
    }))
  })

  # dynamically wire up the per-row delete buttons
  observe({
    lapply(seq_along(rx$rpt_areas), function(i) {
      local({
        idx <- i
        observeEvent(
          input[[paste0("rpt_del_", idx)]],
          {
            cur <- rx$rpt_areas
            cur[[idx]] <- NULL
            rx$rpt_areas <- cur
          },
          ignoreInit = TRUE,
          once       = TRUE)
      })
    })
  })

  # * btn_rpt_submit: POST to plumber in a background worker and open
  # the returned URL on resolve. Keeps the Shiny session responsive
  # while the render runs (which can take a couple of minutes).
  observeEvent(input$btn_rpt_submit, {
    areas <- rx$rpt_areas
    if (length(areas) == 0) {
      trk("report_submit", status = "no_areas")
      showNotification("Add at least one area first.", type = "error")
      return()
    }
    t_rpt <- Sys.time()

    # unique ID for this report request — associates the pre-opened
    # placeholder tab (JS side) with the response when it arrives
    req_id <- paste0("rpt_", as.integer(Sys.time()), "_", sample.int(1e6, 1))
    session$sendCustomMessage("setReportReqId", req_id)

    body <- list(
      title  = input$rpt_title,
      ver    = input$rpt_ver,
      format = input$rpt_format,
      areas  = areas)
    endpoint <- Sys.getenv(
      "MSENS_REPORT_URL",
      unset = "https://api.marinesensitivity.org/report")

    # The API refuses to render a report of a RESTRICTED (under-review) release
    # to the public -- a titled, citable PDF of unreviewed results is exactly
    # what must not circulate. This instance proves it is the preview app with a
    # shared secret (MSENS_PREVIEW_TOKEN, set on both containers); the public
    # instance sends no header and never needs one, since it cannot resolve a
    # restricted version in the first place.
    # a plain character scalar, not a spliced list: this value is captured by the
    # background worker below, and `!!!` splicing there would depend on future's
    # globals detection seeing through rlang's dynamic dots -- a failure that
    # would break reports for EVERYONE, public included
    rpt_tok <- if (msens::atlas_is_preview()) Sys.getenv("MSENS_PREVIEW_TOKEN", "") else ""

    # the report is the highest-value "download" in the toolkit, and the file
    # itself is fetched from file.marinesensitivity.org (a different host, with
    # no JS) — so this is the ONLY place it can be counted. Logged at submit,
    # then again on resolve/reject with the outcome + duration.
    # rx$rpt_areas is an UNNAMED list of list(label, kind, value) — pull the
    # fields out explicitly rather than via names().
    area_lbl  <- vapply(areas, function(a) a$label %||% "", character(1))
    area_kind <- vapply(areas, function(a) a$kind  %||% "", character(1))
    trk("report_submit",
        status   = "submitted",
        rpt_ver  = input$rpt_ver,
        format   = input$rpt_format,
        n_areas  = length(areas),
        area_kinds = paste(sort(unique(area_kind)), collapse = ","),
        areas    = paste(area_lbl, collapse = "; "),
        title    = input$rpt_title)

    # sticky indeterminate progress notification with a bootstrap spinner;
    # removed in both the resolve and reject handlers below.
    notif_id <- showNotification(
      tags$div(
        tags$div(
          class        = "spinner-border spinner-border-sm me-2",
          role         = "status",
          `aria-hidden`= "true"),
        tags$span(
          "Generating report in new tab — this may take a couple of minutes...", br(),
          "Tab will close when report done and download begins. Meanwhile, you can continue using the app.")),
      duration    = NULL,
      closeButton = FALSE,
      type        = "message")

    # run the request in a background R worker. `body` and `endpoint`
    # are captured and serialized to the worker; httr2 is referenced via
    # namespace so the worker loads it automatically. The .then callbacks
    # run back on the main Shiny thread, so they can safely touch
    # `session` and the notification stack.
    promises::future_promise(
      {
        rq <- httr2::request(endpoint) |>
          httr2::req_body_json(body) |>
          httr2::req_timeout(600)
        if (nzchar(rpt_tok))
          rq <- httr2::req_headers(rq, `X-MS-Preview-Token` = rpt_tok)
        rq |> httr2::req_perform() |> httr2::resp_body_json()
      },
      seed = TRUE) |>
      promises::then(
        onFulfilled = function(resp) {
          removeNotification(notif_id)
          rpt_ms <- round(as.numeric(difftime(Sys.time(), t_rpt, units = "secs")) * 1000)
          if (is.null(resp$url)) {
            # ms / status / error are RESERVED names: ms_event() hoists them
            # into their own Sheet columns, so they stay numeric and filterable
            # instead of being buried in the params JSON.
            trk("report_result", status = "no_url", ms = rpt_ms,
                format = input$rpt_format, error = resp$error %||% "")
            showNotification(
              paste0(
                "Report finished but returned no URL",
                if (!is.null(resp$error)) paste0(": ", resp$error) else "."),
              type = "error", duration = 10)
            return()
          }
          # the delivered artifact: report_url is what the user ends up with
          trk("report_result", status = "ok", ms = rpt_ms,
              rpt_ver = input$rpt_ver, format = input$rpt_format,
              n_areas = length(areas), report_url = resp$url)
          session$sendCustomMessage("openUrl", list(url = resp$url, reqId = req_id))
          showNotification(
            "Report ready — downloading.",
            type = "message", duration = 5)
        },
        onRejected = function(e) {
          removeNotification(notif_id)
          trk("report_result", status = "error",
              ms = round(as.numeric(difftime(Sys.time(), t_rpt, units = "secs")) * 1000),
              format = input$rpt_format, error = conditionMessage(e))
          showNotification(
            paste("Report request failed:", conditionMessage(e)),
            type = "error", duration = 10)
        })

    # return invisibly so the observer doesn't block on the promise
    invisible(NULL)
  })
}
# ---- version-aware entry points ---------------------------------------------
#
# `ver` is a URL parameter, not a fork of this app. Both the UI and the server
# are evaluated with their enclosing environment set to the requested version's
# bundle, so every name inside them (con_sdm, d_lyrs, layer_tiles, pra_full_sf,
# cog_of, ...) resolves to THAT release, and anything not version-specific falls
# through to the globals.

ui <- function(req) {
  b <- bundle(ver_of_req(req))
  f <- ui_impl; environment(f) <- b
  f(req)
}

server <- function(input, output, session) {
  v <- ver_of_session(input, session)
  b <- bundle(v)
  f <- server_impl; environment(f) <- b
  f(input, output, session)
}

shinyApp(ui, server)
