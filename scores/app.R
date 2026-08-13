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
  sr_pra_csv <- glue("{dir_cache}/subregion_programareas.csv")
  # filename carries a schema tag: the cache gained clon/clat (the median cell,
# used to centre the map), so a cache written before that would be read with
# missing columns. Bump the tag whenever the columns change.
sr_bb_csv <- glue("{dir_cache}/subregion_bboxes_v2.csv")
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

  # Which zone outline layers this release actually draws, and therefore what a
  # later layer may sit BEFORE.
  #
  # MapLibre rejects an add with a before_id naming a layer that does not exist,
  # and the failure cascades: v1 has no Program Areas, so `pra_ln` was never
  # added, the ecoregion layer asking for before_id="pra_ln" failed, and then the
  # score raster asking for before_id="er_ln" failed too. The map came up with
  # nothing but the labels on it.
  has_pra_ln <- !is.null(ztile("programarea", tbl_pra_pm))
  has_er_ln  <- !is.null(ztile("ecoregion",  tbl_er))
  before_er  <- if (has_pra_ln) "pra_ln" else NULL
  before_r   <- if (has_er_ln) "er_ln" else if (has_pra_ln) "pra_ln" else NULL

  cog_of <- function(metric_key, subregion_key = "FULL") {
    if (is.null(cog_tbl)) return(NULL)
    i <- which(cog_tbl$metric_key == metric_key & cog_tbl$subregion_key == subregion_key)
    if (!length(i) || is.na(cog_tbl$cog[i[1]])) return(NULL)
    list(url      = cog_tbl$cog[i[1]],
         rescale  = c(cog_tbl$rescale_min[i[1]], cog_tbl$rescale_max[i[1]]),
         colormap = cog_tbl$colormap[i[1]])
  }

  # one place that answers "how do I draw this layer?", COG-first
  layer_tiles <- function(metric_key, subregion_key = "FULL", palette = "spectral_r") {
    cg <- cog_of(metric_key, subregion_key)
    if (!is.null(cg))
      return(list(rescale = cg$rescale,
                  url = msens::cog_tile_url(cg$url, colormap = palette,
                                            rescale = cg$rescale, base = tile_base_url)))
    sql <- cell_sql(metric_key, subregion_key)
    st  <- msens::cell_stats(sql, mtime = db_mtime, base = tile_base_url)
    rs  <- c(st$min, st$max)
    list(rescale = rs,
         url = msens::cell_tile_url(sql, colormap = palette, rescale = rs,
                                    mtime = db_mtime, base = tile_base_url))
  }

  # build the (cell_id, value) SELECT for a given metric + subregion; passed to
  # msens::cell_tile_url() / cell_stats(). strict allowlist on the identifiers
  # to keep the string string-interpolation-safe before it ever hits DuckDB.
  cell_sql <- function(metric_key, subregion_key = "FULL") {
    stopifnot(
      is.character(metric_key),   length(metric_key) == 1,
      grepl("^[A-Za-z0-9_.-]+$", metric_key))
    stopifnot(
      is.character(subregion_key), length(subregion_key) == 1,
      grepl("^[A-Za-z0-9_]+$",   subregion_key))
    if (subregion_key == "FULL") {
      glue(
        "SELECT cm.cell_id, cm.val AS value ",
        "FROM cell_metric cm ",
        "JOIN metric m ON cm.metric_seq = m.metric_seq ",
        "WHERE m.metric_key = '{metric_key}'")
    } else {
      glue(
        "SELECT cm.cell_id, cm.val AS value ",
        "FROM cell_metric cm ",
        "JOIN metric     m  ON cm.metric_seq = m.metric_seq ",
        "JOIN zone_cell  zc ON cm.cell_id    = zc.cell_id ",
        "JOIN zone       z  ON zc.zone_seq   = z.zone_seq ",
        "WHERE m.metric_key = '{metric_key}' ",
        "AND   z.tbl       = '{tbl_sr}' ",
        "AND   z.fld       = 'subregion_key' ",
        "AND   z.val       = '{subregion_key}'")
    }
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
        tbl(con_sdm, "cell_metric") |>
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
          tbl(con_sdm, "zone") |>
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
.pra_geom <- NULL
pra_geom <- function() {
  if (!is.null(.pra_geom)) return(.pra_geom)
  g <- NULL
  if (file.exists(pra_gpkg))
    g <- tryCatch(read_sf(pra_gpkg) |> select(programarea_key, programarea_name),
                  error = function(e) NULL)
  if (is.null(g)) {
    zk <- if (!is.null(zone_tbl) && "zone_set_key" %in% names(zone_tbl)) {
      i <- which(zone_tbl$fld == "programarea_key")
      if (length(i)) zone_tbl$zone_set_key[i[1]] else NA_character_
    } else NA_character_
    if (!is.na(zk)) {
      u <- glue("/vsicurl/{msens::atlas_base_url()}/zones/{zk}/zones.fgb")
      g <- tryCatch(read_sf(u), error = function(e) {
        message("zone fgb unavailable (", conditionMessage(e), ")"); NULL })
      if (!is.null(g) && "programarea_key" %in% names(g))
        g <- g |> select(any_of(c("programarea_key", "programarea_name")))
    }
  }
  if (is.null(g)) message("no program-area geometry for ", ver, " - Report tab area picker limited")
  .pra_geom <<- g
  g
}

  # * pra_pts: program area label points (cached) ----
  # per VERSION: this cache is program-area geometry, and a release without
  # Program Areas (v1) was drawing v7's labels over an empty map
  pra_pts_csv <- glue("{dir_cache}/pra_label_pts.csv")
  if (file.exists(pra_pts_csv)) {
    pra_pts <- read_csv(pra_pts_csv, show_col_types = FALSE)
  } else {
    # pra_geom() resolves the release's OWN program areas (published FlatGeobuf,
    # or the local gpkg) and returns NULL when it has none. Reading pra_gpkg
    # directly here meant v1/v2 -- which predate Program Areas and ship no such
    # file -- died at startup with "The file doesn't seem to exist", i.e. HTTP 500
    # for the whole release rather than a map without labels.
    g <- pra_geom()
    pra_pts <- if (is.null(g) || !nrow(g)) {
      tibble(programarea_key = character(), programarea_name = character(),
             lng = numeric(), lat = numeric())
    } else {
      # st_coordinates() on the SF OBJECT, not on a named geometry column: the
      # column is `geom` from a GeoPackage and `geometry` from FlatGeobuf, so
      # naming it broke bundle construction outright the moment the source
      # changed -- and that took the whole app down, not just the labels.
      suppressWarnings({
        pts <- g |> st_shift_longitude() |> st_point_on_surface()
        crd <- st_coordinates(pts)
        pts |> st_drop_geometry() |>
          transmute(programarea_key, programarea_name,
                    lng = crd[, 1], lat = crd[, 2])
      })
    }
    tryCatch(write_csv(pra_pts, pra_pts_csv), error = function(e)
      message("could not cache program-area labels: ", conditionMessage(e)))
  }

  # * pra_full_sf: full program-area polygons, used to render areas added
  # on the Report tab (so the user can see what they're submitting) ----
  
  pra_pts <- st_as_sf(pra_pts, coords = c("lng", "lat"), crs = 4326)

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
        tbl(con_sdm, "cell_metric") |>
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

  # * programareas by subregion ----

  if (!file.exists(sr_pra_csv)) {
    # calculate subregion - programarea cells
    message(glue("Calculating subregion - programarea cells..."))

    # subregion cells
    tbl_sr_cell <- tbl(con_sdm, "zone") |>
      filter(
        tbl == !!tbl_sr,
        fld == "subregion_key"
      ) |>
      select(sr_key = value, zone_seq) |>
      inner_join(
        tbl(con_sdm, "zone_cell") |>
          select(zone_seq, cell_id),
        by = join_by(zone_seq)
      ) |>
      select(sr_key, cell_id)

    # programarea cells
    tbl_pra_cell <- tbl(con_sdm, "zone") |>
      filter(fld == "programarea_key") |>
      select(pra_key = value, zone_seq) |>
      inner_join(
        tbl(con_sdm, "zone_cell") |>
          select(zone_seq, cell_id),
        by = join_by(zone_seq)
      ) |>
      select(pra_key, cell_id)

    # programareas per subregion
    d_sr_pra <- tbl_sr_cell |>
      inner_join(
        tbl_pra_cell,
        by = join_by(cell_id)
      ) |>
      group_by(sr_key, pra_key) |>
      summarise(n_cells = n(), .groups = "drop") |>
      arrange(sr_key, pra_key) |>
      select(
        subregion_key = sr_key,
        programarea_key = pra_key
      ) |>
      collect()

    # NEVER cache an empty mapping for a release that HAS Program Areas. This
    # file is computed only when absent, so one bad write is permanent: v2's was
    # written empty (the tbl filter above used to match nothing) and the app
    # served a blank choropleth from it indefinitely. An empty result is only
    # legitimate where the release genuinely has no Program Areas -- v1.
    if (!nrow(d_sr_pra) && isTRUE(manifest$capabilities$programareas)) {
      warning(glue(
        "{ver}: subregion<->programarea mapping came back EMPTY while the manifest ",
        "declares Program Areas -- not caching. The choropleth will be blank until ",
        "this is fixed; check the zone `tbl` names for this release."))
    } else {
      write_csv(d_sr_pra, sr_pra_csv)
    }
  }
  d_sr_pra <- if (file.exists(sr_pra_csv)) read_csv(sr_pra_csv) else
    tibble(subregion_key = character(), programarea_key = character())

  # NOTE: the old `r_init` terra raster was a ~4 GiB cached default-layer
  # raster (see `scores/cache/r_init_full.tif`) used to seed the initial
  # map's image source via msens::add_cells(). With tiles it's no longer
  # needed — the browser fetches only the viewport's tiles on startup
  # (~4-16 PNGs @ ~5-20 KB each), served cached from Varnish after the
  # first hit. The cache file can be deleted whenever.

  # * d_sr_bb (cached) ----
  # Was derived from r_metrics_{ver}.tif, a per-version raster stack that existed
  # only for v3-v8 and so made older releases unopenable. A subregion's extent is
  # just the extent of its cells, and the cells are in zone_cell -- so ask the view
  # DB and convert cell ids to lon/lat with the grid registry. No raster, and it
  # works for every published version.
  #
  # Longitude stays in the grid's own frame (usa05 is 0-360) so an Alaska bbox
  # remains contiguous instead of splitting at the antimeridian.
  get_sr_bbox <- function(sr_key) {
    pra_sr <- d_sr_pra |>
      filter(subregion_key == !!sr_key) |>
      pull(programarea_key)
    cells <- tbl(con_sdm, "zone") |>
      filter(fld == "programarea_key", value %in% !!pra_sr) |>
      select(zone_seq) |>
      inner_join(tbl(con_sdm, "zone_cell") |> select(zone_seq, cell_id),
                 by = join_by(zone_seq)) |>
      pull(cell_id)
    if (!length(cells)) return(rep(NA_real_, 4))
    g  <- msens::grid_spec_for(msens::grid_for_ver(ver))
    ll <- msens::cell_lonlat(cells, g, wrap = FALSE)
    # cell_lonlat returns CENTRES; an extent must cover the cells, so grow by half
    # a cell. Without this the bbox is inset by 0.025 deg on every side and differs
    # from the raster-derived values the cached csv holds.
    #
    # Also carry the MEDIAN cell position. A bbox centre is a poor place to point
    # a globe: "all US waters" spans 24-82 N, so its bbox centre is 53 N -- up in
    # the Chukchi, with the Gulf and Caribbean below the horizon. The median cell
    # sits where the data actually is.
    c(min(ll$lon) - g$resx / 2, min(ll$lat) - g$resy / 2,
      max(ll$lon) + g$resx / 2, max(ll$lat) + g$resy / 2,
      stats::median(ll$lon), stats::median(ll$lat))
  }
  if (!file_exists(sr_bb_csv)) {
    d_sr_bb <- NULL
    for (sr_key in unique(d_sr_pra$subregion_key)) {
      # sr_key = "GA"
      bbox <- get_sr_bbox(sr_key)
      d_bb_sr <- tibble(
        subregion_key = sr_key,
        xmin = bbox[1], ymin = bbox[2], xmax = bbox[3], ymax = bbox[4],
        clon = bbox[5], clat = bbox[6]
      )
      d_sr_bb <- if (is.null(d_sr_bb)) d_bb_sr else bind_rows(d_sr_bb, d_bb_sr)
    }
    # A release with no Program Areas (v1) has no subregion->programarea rows, so
    # the loop above never runs. `exists("d_sr_bb")` used to paper over that by
    # finding some other frame's copy; inside a per-version bundle it simply
    # errored and took the whole app down for that version. Write the empty
    # frame instead, so the extent falls back to the full map rather than failing.
    if (is.null(d_sr_bb))
      d_sr_bb <- tibble(subregion_key = character(), xmin = numeric(),
                        ymin = numeric(), xmax = numeric(), ymax = numeric(),
                        clon = numeric(), clat = numeric())
    write_csv(d_sr_bb, sr_bb_csv)
  }
  # Explicit types: a release with no subregions writes a HEADER-ONLY csv, and
  # read_csv then guesses `character` for the empty numeric columns, so binding
  # the FULL row on failed with "Can't combine <double> and <character>" and took
  # v1/v2 down. The schema is known; state it rather than let it be inferred.
  d_sr_bb <- read_csv(sr_bb_csv, show_col_types = FALSE,
                      col_types = cols(subregion_key = col_character(),
                                       xmin = col_double(), ymin = col_double(),
                                       xmax = col_double(), ymax = col_double(),
                                       clon = col_double(), clat = col_double()))

  # append FULL bbox = union of subregion bboxes (in-memory only).
  # Previously derived from st_bbox(r_init); with tiles we no longer
  # materialize a full-extent raster, so take the min/max of the cached
  # subregion bboxes instead — same extent in the 0-360° longitude
  # convention the bboxes share with r_cell / r_metrics.
  if (!"FULL" %in% d_sr_bb$subregion_key) {
    # With no subregion rows (a release without Program Areas, e.g. v1) min() of
    # an empty vector is +Inf, which yields an inverted bbox and a map that
    # cannot fit_bounds. Fall back to the release's own grid extent.
    full <- if (nrow(d_sr_bb)) {
      c(min(d_sr_bb$xmin), min(d_sr_bb$ymin), max(d_sr_bb$xmax), max(d_sr_bb$ymax))
    } else {
      g <- msens::grid_spec_for(msens::grid_for_ver(ver))
      c(g$xmin, g$ymax - g$nr * g$resy, g$xmin + g$nc * g$resx, g$ymax)
    }
    d_sr_bb <- bind_rows(
      tibble(subregion_key = "FULL",
             xmin = full[1], ymin = full[2], xmax = full[3], ymax = full[4],
             clon = if (nrow(d_sr_bb)) stats::median(d_sr_bb$clon) else mean(full[c(1,3)]),
             clat = if (nrow(d_sr_bb)) stats::median(d_sr_bb$clat) else mean(full[c(2,4)])),
      d_sr_bb)
  }

  # helper: numeric length-4 bbox c(xmin, ymin, xmax, ymax) for a subregion key
  #
  # The stored bboxes are in the GRID's frame, which for usa05 is 0-360 so that
  # Alaska stays contiguous across the antimeridian. MapLibre expects -180..180
  # and normalises anything above it, so xmax = 275.4 became -84.6, west ended up
  # EAST of east, and fitBounds fitted the COMPLEMENT -- the camera swung to the
  # North Pole with Europe and Africa in view while the actual study area sat on
  # the horizon. Shifting the whole span below 180 keeps west < east and keeps
  # the antimeridian crossing continuous, which fitBounds handles correctly.
  sr_bbox <- function(sr_key) {
    b <- d_sr_bb |>
      filter(subregion_key == !!sr_key) |>
      select(xmin, ymin, xmax, ymax) |>
      as.numeric()
    if (length(b) == 4 && !anyNA(b) && b[3] > 180) { b[1] <- b[1] - 360; b[3] <- b[3] - 360 }
    b
  }

  # CENTER + ZOOM rather than fitBounds.
  #
  # Fitting a bbox that crosses the antimeridian is fragile: the extent is stored
  # in the grid's 0-360 frame, MapLibre normalises past 180, and the fit silently
  # inverts (west ends up east of east) so the camera swings to the North Pole.
  # A centre and a zoom cannot invert. The centre is computed in the CONTINUOUS
  # frame and wrapped once at the end, so an Alaska spanning the dateline centres
  # in the Bering Sea rather than halfway around the world.
  #
  # Zoom from the span: the globe shows ~360 deg at z0 and halves each level, so
  # z = log2(360 / span). Latitude counts double because the viewport is wider
  # than it is tall. Clamped to a sane range and pulled in slightly (-0.35) so the
  # area sits inside the view rather than flush against the edges.
  sr_view <- function(sr_key) {
    b <- sr_bbox(sr_key)
    if (length(b) != 4 || anyNA(b)) return(list(center = c(-100, 40), zoom = 1.6))
    ctr <- d_sr_bb |> filter(subregion_key == !!sr_key)
    lon <- if (nrow(ctr) && !is.na(ctr$clon[1])) ctr$clon[1] else (b[1] + b[3]) / 2
    lat <- if (nrow(ctr) && !is.na(ctr$clat[1])) ctr$clat[1] else (b[2] + b[4]) / 2
    if (lon >  180) lon <- lon - 360
    if (lon < -180) lon <- lon + 360
    span <- max(b[3] - b[1], (b[4] - b[2]) * 2, 1)
    # +0.55 rather than -0.35: at the theoretical fit the globe sat small in the
    # middle of the viewport with empty space around it, because a sphere shows
    # only its facing hemisphere. Nudged in so the study area fills the frame.
    z    <- max(1.2, min(5, log2(360 / span) + 0.55))
    list(center = c(lon, lat), zoom = z)
  }

  # Study areas are DERIVED from what the release actually published, not
  # hardcoded. The set genuinely differs: v1 has AK/AKL48/L48/USA, v2-v3 add
  # GA/PA, v8 adds AT (Atlantic) -- which a fixed list silently hid, even though
  # the whole point of the canonical subregions is to span all US waters.
  #
  # "Full study area" (FULL) and "All USA" (USA) were also two labels for one
  # extent (FULL is the union of the subregion bboxes, i.e. the USA extent), so
  # the picker offered the same view twice. USA wins where both exist; FULL is
  # kept only as the fallback when a release has no USA surface.
  sr_labels <- c(USA = "All US waters", FULL = "Full study area",
                 AK = "Alaska", AT = "Atlantic", GA = "Gulf of America",
                 PA = "Pacific", L48 = "Mainland USA",
                 AKL48 = "Mainland USA & Alaska")
  sr_choices <- local({
    have_cog <- if (!is.null(cog_tbl)) unique(cog_tbl$subregion_key) else character()
    # a study area needs BOTH a surface to draw and an extent to zoom to
    have_bb  <- d_sr_bb$subregion_key
    k <- intersect(have_cog, have_bb)
    if ("USA" %in% k) k <- setdiff(k, "FULL")
    if (!length(k)) k <- "FULL"
    ord <- c("USA", "FULL", "AKL48", "L48", "AK", "AT", "GA", "PA")
    k <- c(intersect(ord, k), sort(setdiff(k, ord)))
    setNames(k, ifelse(is.na(sr_labels[k]), k, sr_labels[k]))
  })
  message("study areas: ", paste(names(sr_choices), collapse = ", "))

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
  initial_bbox     <- sr_bbox(sr_choices[[1]])
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
    d_flower_default <- tbl(con_sdm, "zone") |>
      filter(tbl == !!tbl_sr, fld == "subregion_key") |>
      select(zone_seq, subregion_key = value) |>
      inner_join(tbl(con_sdm, "zone_metric"), by = "zone_seq") |>
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
ver_of <- function(qs) {
  v <- tryCatch({
    q <- shiny::parseQueryString(qs %||% "")
    msens::atlas_resolve_ver(q$ver)
  }, error = function(e) NULL)
  if (is.null(v)) tryCatch(msens::atlas_resolve_ver(NULL), error = function(e) ver_fallback) else v
}

ui_impl <- function(req) page_sidebar(
  tags$head(
    tags$link(rel = "icon", type = "image/x-icon", href = "favicon.ico"),
    # usage tracking: GA4 (aggregate) + a batched beacon to the usage-log Sheet
    # (detail). Both legs are driven from the browser, so no reactive ever
    # performs network I/O — see msens::ga_js(). The Sheet leg is a silent no-op
    # unless MSENS_LOG_URL is set, so local dev writes nothing.
    msens::ga_head("scores", app_version = APP_VERSION,
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
      var praHandlersAdded = false;
      Shiny.addCustomMessageHandler('setPraTooltips', function(data) {
        praTooltips = data;
        // add hover handlers once map is ready
        if (praHandlersAdded) return;
        var widget = HTMLWidgets.find('#map');
        if (!widget) return;
        var map = widget.getMap();
        if (!map) return;
        praHandlersAdded = true;
        praPopup = new maplibregl.Popup({
          closeButton: false, closeOnClick: false
        });
        map.on('mousemove', 'pra_lyr', function(e) {
          if (!e.features || !e.features.length) return;
          var key = e.features[0].properties.programarea_key;
          var tip = praTooltips[key] ||
            e.features[0].properties.programarea_name || key;
          map.getCanvas().style.cursor = 'pointer';
          praPopup.setLngLat(e.lngLat).setHTML(tip).addTo(map);
        });
        map.on('mouseleave', 'pra_lyr', function() {
          map.getCanvas().style.cursor = '';
          praPopup.remove();
        });
      });
    "))
  ),
  useConductor(),
  title = div(
    style = "display: flex; align-items: center; width: 100%;",
    span("BOEM Marine Sensitivity ",
         actionLink("show_versions", glue("({ver})"),
                    title = "data version - click to switch")),
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
        # Driven by the RELEASE, not hardcoded. v1 predates Program Areas and
        # scored Planning Areas, so a fixed "Program areas" choice offered the one
        # unit v1 does not have and hid the one it does -- an empty map with an
        # Inf/-Inf legend and no explanation.
        choices = c(
          "Raster cells (0.05°)" = "cell",
          if (isTRUE(manifest$capabilities$planareas) && !is.na(tbl_pa))
            c("Planning areas" = "pa"),
          if (isTRUE(manifest$capabilities$programareas) && !is.na(tbl_pra))
            c("Program areas" = "pra")
        )
      )
    ),
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
      title = "Plot of Scores",
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
      title = "Table of Species",
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
        msens::version_picker_html(ver),
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

    resolved <- tryCatch(msens::atlas_resolve_ver(req), error = function(e) NULL)
    if (is.null(resolved)) {
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

  # echo the version into the URL so a shared link is explicit about what it shows
  # (and keeps working once other versions render here)
  observe({
    updateQueryString(glue("?ver={ver}"), mode = "replace", session = session)
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
          src   = "https://marinesensitivity.org/docs/figures/overview-methods.svg",
          style = "max-width: 80%; height: auto; max-height: 300px; margin-bottom: 10px;",
          alt   = "Marine Sensitivity Methods Overview"),
        tags$p(
          "Explore composite sensitivity scores across US Program Areas,",
          "component scores, and species found in cells or Program Areas. Also see:"),
        tags$ul(
          tags$li(tags$a(
            href   = "../species/",
            target = "_blank",
            "Species app"), " for mapping individual species distributions"),
          tags$li(tags$a(
            href   = "https://marinesensitivity.org/docs/",
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
      text     = "Choose which sensitivity metric to display \u2014 composite score, individual species categories (bird, fish, mammal, etc.), or primary productivity. Note that some cells (Atlantic, Gulf of America, Hawaii, Puerto Rico, Pacific Islands) have scores but lie outside any v6 BOEM Program Area; they appear dimmed under a 'Cells outside Program Areas' overlay.",
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
  rx <- reactiveValues(
    clicked_pa       = NULL,
    clicked_pra      = NULL,
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
      glue("{rx$clicked_pra$properties$programarea_name}")
    } else {
      sr_key <- input$sel_subregion %||% "FULL"
      sr_lbl <- if (sr_key == "FULL") "Full study area" else
        names(sr_choices)[sr_choices == sr_key]
      glue("{sr_lbl} (default)")
    }
  })

  # * get_rast_rx ----
  # returns a metadata list for the msens cell-tile layer; no terra raster
  # is materialized in R (the browser fetches tiles on demand from
  # titilecache). NULL when the unit is not "cell" (pa/pra use vector fills).
  get_rast_rx <- reactive({
    req(input$sel_subregion, input$sel_unit, input$sel_lyr, input$sel_palette)

    if (input$sel_unit %in% c("pa", "pra")) {
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
        bbox     = initial_bbox))
    }

    lt <- layer_tiles(m_key, sr_key, palette = pal)

    list(
      m_key    = m_key,
      sr_key   = sr_key,
      sql      = cell_sql(m_key, sr_key),
      rescale  = lt$rescale,
      tile_url = lt$url,
      bbox     = sr_bbox(sr_key),
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
      msens::add_pmline(Filter(Negate(is.null), list(
        if (!is.null(pa <- ztile("programarea", tbl_pra_pm)))
          c(pa, list(id = "pra_ln", source_id = "pra_src",
                     line_color = "white", line_width = 1)),
        if (!is.null(er <- ztile("ecoregion", tbl_er)))
          c(er, list(id = "er_ln", source_id = "er_src",
                     line_color = "black", line_width = 3, before_id = before_er))))) |>
      msens::add_pmlabel(list(
        list(source     = pra_pts,
             text_field = "programarea_key",
             id         = "pra_lbl"))) |>
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
        layers = list(
          "Program Area outlines"       = "pra_ln",
          "Program Area labels"         = "pra_lbl",
          "Ecoregions outlines"         = "er_ln",
          "Raster cell values"          = "r_lyr",
          "Cells outside Program Areas" = "outside_pra_lyr")) |>
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
      } else if (unit == "pra") {
        # * programarea ----

        if (verbose) {
          message(glue("update map pra - beg"))
        }

        rx$clicked_cell <- NULL
        # rx$clicked_pa <- NULL

        # sr_bbox()/sr_view() rather than reading d_sr_bb raw: the stored frame is
        # 0-360 and must be shifted before it reaches MapLibre (see sr_view)
        sr_bb <- sr_bbox(sr_key)
        sr_v  <- sr_view(sr_key)
        if (verbose) {
          message(glue("sr_bb: {paste(round(sr_bb,2), collapse = ', ')}"))
        }

        # full study area = all program areas; otherwise filter by subregion
        pra_keys <- if (sr_key == "FULL") {
          unique(d_sr_pra$programarea_key)
        } else {
          d_sr_pra |>
            filter(subregion_key == !!sr_key) |>
            pull(programarea_key)
        }
        pra_filter <- c("in", "programarea_key", pra_keys)

        # query program area values from db
        n_cols <- 11
        cols_r <- get_pal_colors(input$sel_palette, n_cols)

        d_pra <- tbl(con_sdm, "zone") |>
          filter(
            fld == "programarea_key",
            value %in% pra_keys) |>
          select(programarea_key = value, zone_seq) |>
          inner_join(tbl(con_sdm, "zone_metric"), by = join_by(zone_seq)) |>
          inner_join(
            tbl(con_sdm, "metric") |>
              filter(metric_key == !!lyr),
            by = join_by(metric_seq)) |>
          select(programarea_key, value) |>
          collect()

        rng_pra <- range(d_pra$value)
        cols_pra <- colorRampPalette(cols_r, space = "Lab")(n_cols)

        # assign color per program area by scaling value to color index
        d_pra <- d_pra |>
          mutate(
            val_scaled = (value - rng_pra[1]) / max(rng_pra[2] - rng_pra[1], 1e-6),
            col_idx    = pmin(pmax(round(val_scaled * (n_cols - 1)) + 1, 1), n_cols),
            fill_color = cols_pra[col_idx])

        # build tooltip lookup and send to client
        pra_tooltip <- d_pra |>
          left_join(
            pra_pts |> st_drop_geometry() |> select(programarea_key, programarea_name),
            by = "programarea_key") |>
          mutate(tip = glue("{programarea_name}: {round(value)}")) |>
          select(programarea_key, tip) |>
          deframe() |>
          as.list()
        session$sendCustomMessage("setPraTooltips", pra_tooltip)

        # applied to both the Map tab's proxy and the Report tab's
        # embedded map proxy so the Report map shows Program Areas too.
        apply_pra_update <- function(map_proxy) {
          map_proxy |>
            clear_layer("r_lyr") |>
            clear_layer("r_src") |>
            clear_layer("pra_lyr") |>
            clear_layer("outside_pra_lyr") |>
            clear_legend() |>
            add_fill_layer(
              id           = "pra_lyr",
              source       = "pra_src",
              source_layer = pra_src_layer,
              fill_color   = match_expr(
                column  = "programarea_key",
                values  = d_pra$programarea_key,
                stops   = d_pra$fill_color,
                default = "lightgrey"),
              fill_opacity       = 0.7,
              fill_outline_color = "white",
              hover_options      = list(
                fill_color   = "purple",
                fill_opacity = 1),
              before_id = "pra_ln",
              filter    = pra_filter) |>
            msens::add_cell_tiles(
              outside_pra_tile_url,
              id             = "outside_pra_lyr",
              source_id      = "outside_pra_lyr",
              raster_opacity = 0.55,
              visibility     = "none",
              before_id      = before_r) |>
            mapgl::add_legend(
              get_lyr_name(input$sel_lyr),
              values   = round(rng_pra, 1),
              colors   = cols_pra,
              position = "bottom-right") |>
            mapgl::fly_to(center = sr_v$center, zoom = sr_v$zoom) |>
            clear_controls("layers") |>
            add_layers_control(
              layers = list(
                "Program Area outlines"       = "pra_ln",
                "Program Area labels"         = "pra_lbl",
                "Ecoregion outlines"          = "er_ln",
                "Program Area values"         = "pra_lyr",
                "Cells outside Program Areas" = "outside_pra_lyr"))
        }
        apply_pra_update(maplibre_proxy("map"))
        apply_pra_update(maplibre_proxy("map_rpt"))

        if (verbose) {
          message(glue("update map pra - end"))
        }
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
    } else if (input$sel_unit == "pa") {
      # handle planning area click
      rx$clicked_cell <- NULL
      rx$clicked_pra <- NULL

      if (!is.null(input$map_feature_click)) {
        rx$clicked_pa <- list(
          id = input$map_feature_click$id,
          properties = input$map_feature_click$properties
        )
      }
    } else if (input$sel_unit == "pra") {
      # handle program area click
      rx$clicked_cell <- NULL
      rx$clicked_pa <- NULL

      if (!is.null(input$map_feature_click)) {
        rx$clicked_pra <- list(
          id = input$map_feature_click$id,
          properties = input$map_feature_click$properties
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
          tbl(con_sdm, "cell_metric"),
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
    } else if (input$sel_unit == "pra" && !is.null(rx$clicked_pra)) {
      # get data for program area from database
      pra_name <- rx$clicked_pra$properties$programarea_name
      pra_key  <- rx$clicked_pra$properties$programarea_key

      if (verbose) {
        message(glue("Rendering flower plot for Program Area: {pra_name} ({pra_key})"))
      }

      # look up zone_seq for this program area
      z_seq <- tbl(con_sdm, "zone") |>
        filter(tbl == !!tbl_pra, value == !!pra_key) |>
        pull(zone_seq)

      if (length(z_seq) > 0) {
        d_fl <- tbl(con_sdm, "metric") |>
          filter(str_detect(metric_key, ".*_ecoregion_rescaled$")) |>
          left_join(
            tbl(con_sdm, "zone_metric"),
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
      sr_key   <- input$sel_subregion %||% "FULL"
      z_sr_key <- if (sr_key == "FULL") "FULL" else sr_key
      sr_lbl   <- if (sr_key == "FULL") "Full study area" else
        names(sr_choices)[sr_choices == sr_key]
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
    } else if (input$sel_unit == "pa" && !is.null(rx$clicked_pa)) {
      cat("Planning Area:", rx$clicked_pa$feature$properties$planarea_name)
    } else if (input$sel_unit == "pra" && !is.null(rx$clicked_pra)) {
      cat("Program Area:", rx$clicked_pra$feature$properties$programarea_name)
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
    if (input$sel_unit == "pra" && !is.null(rx$clicked_pra)) {
      pra_key  <- rx$clicked_pra$properties$programarea_key
      pra_name <- rx$clicked_pra$properties$programarea_name
      if (verbose)
        message(glue("Getting species table for Program Area: {pra_name}"))
      rx$spp_tbl_hdr      <- glue("Species for Program Area: {pra_name}")
      rx$spp_tbl_filename <- glue(
        "species_programarea-{str_replace(pra_name, ' ', '-') |> str_to_lower()}")
      return(msens::species_for_zone(con_sdm, "programarea_key", pra_key))
    }

    # ** subregion default ----
    # "Full study area" is a UI-only choice with no zone row of its own; USA is
    # the superset zone. (The old code mapped FULL -> "FULL", which matches
    # nothing, so the default view returned no species at all.)
    sr_key   <- input$sel_subregion %||% "FULL"
    z_sr_key <- if (sr_key == "FULL") "USA" else sr_key
    sr_lbl   <- if (sr_key == "FULL") "Full study area" else
      names(sr_choices)[sr_choices == sr_key]
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
    id_col <- if ("mdl_key" %in% names(d_spp)) "mdl_key" else
              if ("mdl_seq" %in% names(d_spp)) "mdl_seq" else NA_character_
    # rename columns
    d_spp |>
      mutate(
        # from /scores_v8/ this must point at the v8 species app; the old
        # "../species/?mdl_seq=" resolved to the *v7* app and used the v7 key.
        model_url = if (is.na(id_col)) NA_character_ else
          glue("../species_v8/?ver={ver}&{id_col}={.data[[id_col]]}"),
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
        model_id = mdl_key,
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
    if (input$sel_unit != "pra") return()
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
    nm <- props$programarea_name %||% props$programarea_key
    if (!is.null(nm) && nzchar(nm))
      updateTextInput(session, "rpt_area_label", value = nm)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # * highlight the currently-clicked Program Area on both maps with a
  # thick bright-pink border so it stands out against the Spectral
  # cell colormap. Applies to the Map tab and the Report tab's
  # embedded map; clears the highlight when rx$clicked_pra is NULL.
  observe({
    clicked <- rx$clicked_pra
    key <- if (!is.null(clicked))
      clicked$properties$programarea_key %||% clicked$properties$planarea_key
    for (mid in c("map", "map_rpt")) {
      proxy <- maplibre_proxy(mid)
      proxy |> clear_layer("pra_highlight_ln")
      if (!is.null(key) && nzchar(key)) {
        proxy |> add_line_layer(
          id           = "pra_highlight_ln",
          source       = "pra_src",
          source_layer = pra_src_layer,
          line_color   = "#ff00aa",
          line_width   = 4,
          filter       = list("==", "programarea_key", key))
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
        httr2::request(endpoint) |>
          httr2::req_body_json(body) |>
          httr2::req_timeout(600) |>
          httr2::req_perform() |>
          httr2::resp_body_json()
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
  b <- bundle(ver_of(req$QUERY_STRING))
  f <- ui_impl; environment(f) <- b
  f(req)
}

server <- function(input, output, session) {
  v <- ver_of(isolate(session$clientData$url_search))
  b <- bundle(v)
  f <- server_impl; environment(f) <- b
  f(input, output, session)
}

shinyApp(ui, server)
