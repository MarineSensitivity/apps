# packages ----
librarian::shelf(
  bslib,
  etiennebacher/conductor,
  DBI,
  dplyr,
  duckdb,
  glue,
  here,
  htmltools,
  MarineSensitivity/msens,
  RColorBrewer,
  readr,
  scales,
  sf,
  shiny,
  stringr,
  terra,
  tibble,
  tidyr,
  quiet = T
)

options(
  shiny.minified = T,
  sass.cache = F,
  bslib.precompiled = T,
  bslib.color_contrast_warnings = F
  # shiny.autoreload is a DEVELOPMENT option and was left on here: in production it
  # runs a (legacy) file watcher over the app directory for every process and makes
  # each page open a `wss://.../autoreload/` socket that nothing answers -- one
  # failed WebSocket and a red console line per load, on both the public and the
  # preview host. Set it in your own session while developing, not in the app.
)

# variables ----
verbose <- T

# version ----
# The app renders ANY published release; this is only the last-resort fallback for when
# the version registry itself is unreachable (atlas_resolve_ver errors).
VER_FALLBACK <- "v8"


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
  if (!is.null(sha) && !is.na(sha) && nzchar(sha)) sha else VER_FALLBACK
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
mapbox_tkn_txt <- glue("{dir_private}/mapbox_token_bdbest.txt")
Sys.setenv(MAPBOX_PUBLIC_TOKEN = readLines(mapbox_tkn_txt))
librarian::shelf(
  mapgl,
  quiet = T
)

# helper functions ----

#' Add raster with fixed color range to mapgl map
#'
#' Colors are drawn from a fixed scale but truncated to the data's actual range,
#' ensuring consistent color mapping across multiple rasters.
#'
#' @param map A mapgl map object
#' @param r A SpatRaster
#' @param id Layer ID
#' @param fixed_range Numeric vector c(min, max) defining the full color scale
#' @param colors Color palette vector spanning the full fixed_range
#' @param ... Additional arguments passed to add_raster_layer()
add_fixed_range_raster <- function(
  map,
  data,
  id,
  fixed_range = c(1, 100),
  colors = viridisLite::viridis(256),
  ...) {

  # Get raster data range, test if within fixed range
  dr <- terra::minmax(data) |> as.numeric()
  stopifnot(dr[1] >= fixed_range[1] & dr[2] <= fixed_range[2])

  # Expand colors to full fixed range, then truncate to data range
  n_clrs  <- diff(fixed_range) + 1
  clrs    <- colorRampPalette(colors)(n_clrs)
  clrs_dr <- clrs[seq.int(dr[1], dr[2])]

  map |>
    add_image_source(
      id     = paste0(id, "_src"),
      data   = terra::clamp(data, lower = fixed_range[1], upper = fixed_range[2], values = TRUE),
      colors = clrs_dr) |>
    add_raster_layer(
      id     = id,
      source = paste0(id, "_src"),
      ...)
}
# Usage
# maplibre() |>
#   add_fixed_range_raster(raster1, "layer1", range = c(1, 100),  colors = viridisLite::viridis(256), raster_opacity = 0.7)

# OBIS occurrence overlay (h3t) ----
# optional per-species OBIS occurrence hexagons (default off), reusing the apps/h3-db `h3t` layer.
# Now that this map is MapLibre (aligned with the scores app), the h3t maplibre vector-tile protocol
# (add_h3t_source → maplibregl.addProtocol) renders here. The tile SQL/URL is the single source of
# truth in obisindicators::obis_h3t_sql()/obis_h3t_url(); locate the sibling checkout like h3-db does.
# Guarded: if obisindicators isn't present, has_obis = FALSE and the toggle is hidden, so the app
# still runs. obis_h3t_sql(aphiaid=) filters to the species' WoRMS-AphiaID subtree.
H3T_HOST            <- "https://h3t.marinesensitivity.org"
H3T_TILES_BASE      <- "h3tiles://h3t.marinesensitivity.org/h3t/{z}/{x}/{y}.h3t"
H3T_RELEASE         <- format(Sys.Date(), "v%Y%m%d")
H3T_VIRIDIS5        <- c("#440154", "#3b528b", "#21918c", "#5ec962", "#fde725")
H3T_STATS_TIMEOUT_S <- 12L

h3t_src <- Sys.glob(c(
  "/share/github/marinebon/obisindicators/R/h3t.R",   # MST server
  "../../marinebon/obisindicators/R/h3t.R",           # local sibling checkout
  "~/Github/marinebon/obisindicators/R/h3t.R"))[1]
has_obis <- !is.na(h3t_src)
if (has_obis) {
  source(file.path(dirname(h3t_src), "taxon.R"))      # .h3t_taxon_tree_cte() for aphiaid filtering
  source(h3t_src)                                     # obis_h3t_sql() / obis_h3t_url()
}

# per-species OBIS occurrence stats (from the h3t /stats endpoint) -> color-ramp range
h3t_stats <- function(sql, res_h3 = 4) {
  q   <- gsub("\n", "", base64enc::base64encode(charToRaw(sql)))
  url <- glue("{H3T_HOST}/h3t/stats?q={utils::URLencode(q, reserved = TRUE)}&res_h3={res_h3}")
  tryCatch({
    resp <- curl::curl_fetch_memory(url, handle = curl::new_handle(timeout = H3T_STATS_TIMEOUT_S))
    body <- rawToChar(resp$content)
    if (nzchar(body)) jsonlite::fromJSON(body) else list()
  }, error = function(e) list(error = conditionMessage(e)))
}

# per-version bundle ----
#
# The version is a URL PARAMETER, not a fork of this app (`ver <- "v8"` used to sit here,
# which is why /species/?ver=v7 answered "not served here yet" while every other surface of
# the toolkit had already been made version-independent). Everything below that depends on
# WHICH release is being drawn is built per version and memoised; the UI and server are then
# re-enclosed in that bundle (see the bottom of this file), so names like `con_sdm`, `d_spp`,
# `native_asset` and `tile_base_url` keep their spelling and resolve to the requested release,
# while anything version-independent falls through to the globals above.
.bundles <- new.env(parent = emptyenv())

build_bundle <- function(ver) {

  dir_v   <- glue("{dir_data}/derived/{ver}")
  dir_big <- ifelse(
    is_server,
    glue("/share/data/big/{ver}"),
    glue("~/_big/msens/derived/{ver}"))
  is_prod <- Sys.getenv("MSENS_ENV") == "prod"
  pmtiles_base_url <- ifelse(
    is_prod,
    "/pmtiles",
    "https://file.marinesensitivity.org/pmtiles")
  tbl_er <- "ply_ecoregions_2025"
  tbl_pra <- glue("ply_programareas_2026_{ver}")
  tbl_pra_pm <- "ply_programareas_2026"

  # zone outlines from the version manifest ----
  #
  # Same contract as scores/app.R: zone PMTiles are published per VINTAGE
  # (`zones/{zone_set_key}/zones.pmtiles`) and each release's manifest names the
  # one it used, replacing two unversioned filenames on the file host that no
  # committed notebook built. The layer id inside the tiles is the zone TYPE, not
  # the old table name, so URL and source_layer must move together.
  #
  # NULL means the manifest is present and names no such zone type -- the release
  # genuinely lacks it (v1 predates Program Areas), so draw nothing rather than an
  # outline from the wrong era. The unversioned fallback is for a MISSING manifest.
  zone_manifest <- tryCatch(
    msens::atlas_manifest(ver),
    error = function(e) { message("manifest unavailable (", conditionMessage(e),
                                  ") - zone outlines fall back to unversioned tiles"); NULL })
  zone_tbl <- if (!is.null(zone_manifest)) zone_manifest$zones else NULL

  ztile <- function(zone_type, fallback_tbl) {
    if (!is.null(zone_tbl) && "pmtiles" %in% names(zone_tbl)) {
      i <- which(zone_tbl$fld == paste0(zone_type, "_key") & !is.na(zone_tbl$pmtiles))
      return(if (length(i)) list(url = zone_tbl$pmtiles[i[1]], source_layer = zone_type)
             else NULL)
    }
    list(url          = glue("{pmtiles_base_url}/{fallback_tbl}.pmtiles"),
         source_layer = fallback_tbl)
  }
  pra_src_layer <- ztile("programarea", tbl_pra_pm)$source_layer %||% tbl_pra_pm
  er_src_layer  <- ztile("ecoregion",  tbl_er)$source_layer      %||% tbl_er

  # PER GRID, not a constant: usa05 (v1-v7) is 3103x2006 in 0-360 longitude and global05 (v8)
  # is 7200x3600 in -180..180, so the same cell_id names a different place on each. The grid
  # registry says which lookup raster belongs to this release.
  grid_id  <- msens::grid_for_ver(ver)
  grid     <- msens::grid_spec_for(grid_id)
  cell_tif <- glue("{dir_data}/derived/{msens::grid_registry()$cellid_tif[msens::grid_registry()$grid_id == grid_id][1]}")
  pra_gpkg <- glue("{dir_v}/ply_programareas_2026_{ver}.gpkg")
  sdm_db   <- { s <- glue("{dir_big}/serve.duckdb"); if (file.exists(s)) s else glue("{dir_big}/sdm.duckdb") }
  # titiler-v8 is the stock-COG tiler for EVERY release (it serves /cog from any public URL);
  # only the v8-specific `model_cell` SQL path is version-bound. mtime cache-busts tile URLs.
  tile_base_url <- "https://titiler-v8.marinesensitivity.org"
  db_mtime <- format(file.info(sdm_db)$mtime, "%Y%m%dT%H%M%SZ", tz = "UTC")

  # database ----
  # source(here("../workflows/libs/db.R")) # con
  con_sdm <- dbConnect(duckdb(), dbdir = sdm_db, read_only = T)
  db_tables <- dbListTables(con_sdm)
  # v8 alone publishes the per-model serving surface titiler queries by SQL; every other
  # release draws exclusively from per-model COGs (model_asset). Derived from PRESENCE, so a
  # release that has never had it cannot be asked for it.
  has_model_cell <- "model_cell" %in% db_tables

  # data prep ----

  # * pra_pts: program area label points (cached PER VERSION) ----
  #
  # Two changes from the single-version app, both load-bearing. The cache is keyed by
  # version -- one shared csv would have labelled every release with v8's program areas.
  # And the per-version gpkg is a FALLBACK, not a requirement: v1 ships no program-area
  # file at all, and reading the path directly is what made the scores app die at startup
  # for a whole release rather than draw a map without labels. The published per-VINTAGE
  # FlatGeobuf is the general source; no geometry -> no labels, which is the honest answer
  # for a release that has no such unit.
  pra_pts_csv <- here(glue("species/cache/pra_label_pts_{ver}.csv"))
  if (!file.exists(pra_pts_csv)) {
    g <- NULL
    if (file.exists(pra_gpkg))
      g <- tryCatch(read_sf(pra_gpkg), error = function(e) NULL)
    if (is.null(g) && !is.null(zone_tbl) && "zone_set_key" %in% names(zone_tbl)) {
      i <- which(zone_tbl$fld == "programarea_key")
      if (length(i)) g <- tryCatch(
        read_sf(glue("/vsicurl/{msens::atlas_base_url()}/zones/{zone_tbl$zone_set_key[i[1]]}/zones.fgb")),
        error = function(e) { message("programarea fgb unavailable for ", ver, " (",
                                      conditionMessage(e), ") - no labels"); NULL })
    }
    pra_pts <- if (is.null(g) || !nrow(g)) {
      tibble(programarea_key = character(), programarea_name = character(),
             lng = double(), lat = double())
    } else {
      if (!"programarea_name" %in% names(g)) g$programarea_name <- g$programarea_key
      p <- suppressWarnings(st_point_on_surface(g))
      tibble(programarea_key  = p$programarea_key,
             programarea_name = p$programarea_name,
             lng = st_coordinates(p)[, 1], lat = st_coordinates(p)[, 2])
    }
    write_csv(pra_pts, pra_pts_csv)
  } else {
    pra_pts <- read_csv(pra_pts_csv, show_col_types = FALSE)
  }
  # an sf with zero features still needs its geometry column, but st_as_sf() on an empty
  # frame warns four times about min/max of nothing — noise that would appear in the app log
  # for every release with no program areas (v1), where the empty result is CORRECT.
  pra_pts <- if (nrow(pra_pts))
    st_as_sf(pra_pts, coords = c("lng", "lat"), crs = 4326, na.fail = FALSE) else
    st_sf(programarea_key = character(), programarea_name = character(),
          geometry = st_sfc(crs = 4326))

  # * er_bbox: default map extent, from THIS release's own scored cells ----
  #
  # Was derived from a per-version `r_metrics_{ver}.tif` that only v8 ships, and cached to a
  # single shared csv. Now computed from `zone_cell` + the release's grid, so it needs no
  # local raster and cannot hand one release another's extent. Longitude stays in the grid's
  # own frame (usa05 is 0-360) so an Alaska-spanning extent stays contiguous rather than
  # splitting at the antimeridian.
  er_bbox_csv <- here(glue("species/cache/ecoregions_bbox_{ver}.csv"))
  if (!file.exists(er_bbox_csv)) {
    cells <- tryCatch(
      tbl(con_sdm, "zone") |> filter(fld == "ecoregion_key") |> select(zone_seq) |>
        inner_join(tbl(con_sdm, "zone_cell") |> select(zone_seq, cell_id), by = join_by(zone_seq)) |>
        pull(cell_id),
      error = function(e) integer(0))
    er_bbox <- if (!length(cells)) c(-180, -90, 180, 90) else {
      ll <- msens::cell_lonlat(cells, grid, wrap = FALSE)
      # cell_lonlat returns CENTRES; an extent must cover the cells, so grow by half a cell
      c(min(ll$lon) - grid$resx / 2, min(ll$lat) - grid$resy / 2,
        max(ll$lon) + grid$resx / 2, max(ll$lat) + grid$resy / 2)
    }
    tibble(xmin = er_bbox[1], ymin = er_bbox[2], xmax = er_bbox[3], ymax = er_bbox[4]) |>
      write_csv(er_bbox_csv)
  } else {
    er_bbox <- read_csv(er_bbox_csv, show_col_types = FALSE) |> as.numeric()
  }

  # query dataset metadata once at startup.
  #
  # The display fields (`name_display`, `value_info`, `is_mask`, `sort_order`) arrived in v3;
  # v1 and v2 publish only the long-form `name_short`. Selecting them unconditionally made the
  # whole of v1/v2 fail at startup, so they are filled in from what the release DOES publish
  # rather than required: the short name is name_short's first comma-clause, capped at a word
  # boundary, and the key itself is the last resort — never a hardcoded per-key table, which
  # would silently mislabel the next dataset added.
  d_datasets <- tbl(con_sdm, "dataset") |> collect()
  .short <- function(x, ds) {
    s <- sub(",.*$", "", x)
    s <- ifelse(is.na(s) | !nzchar(s), ds, s)
    ifelse(nchar(s) > 30, paste0(sub("\\s+\\S*$", "", substr(s, 1, 30)), "…"), s)
  }
  if (!"name_short" %in% names(d_datasets)) d_datasets$name_short <- NA_character_
  if (!"name_display" %in% names(d_datasets)) d_datasets$name_display <- NA_character_
  d_datasets$name_display <- ifelse(
    is.na(d_datasets$name_display) | !nzchar(d_datasets$name_display),
    .short(d_datasets$name_short, d_datasets$ds_key), d_datasets$name_display)
  if (!"value_info" %in% names(d_datasets)) d_datasets$value_info <- NA_character_
  if (!"is_mask"    %in% names(d_datasets)) d_datasets$is_mask    <- FALSE
  if (!"sort_order" %in% names(d_datasets)) d_datasets$sort_order <- seq_len(nrow(d_datasets))
  # on_grid (v9+): the native raster IS the analysis grid (AquaX), so its two representations are
  # "as delivered" vs "as ingested" (scale, integer, the ingest threshold), not original vs
  # interpolated -- the toggle is labelled accordingly. Absent before v9 -> FALSE.
  if (!"on_grid"    %in% names(d_datasets)) d_datasets$on_grid    <- FALSE
  d_datasets <- d_datasets |>
    mutate(on_grid = coalesce(as.logical(on_grid), FALSE)) |>
    select(ds_key, name_display, value_info, is_mask, on_grid, sort_order) |>
    arrange(sort_order)

  # derive what was previously hardcoded
  ds_keys      <- d_datasets |> filter(!ds_key %in% c("ms_merge")) |> pull(ds_key)
  layer_names  <- c(
    "mdl_key" = "Merged Model",
    deframe(d_datasets |> filter(ds_key != "ms_merge", !is.na(name_display)) |> select(ds_key, name_display)))
  mdl_names    <- deframe(d_datasets |> filter(ds_key != "ms_merge", !is.na(name_display)) |> select(ds_key, name_display))
  mdl_info     <- deframe(d_datasets |> filter(!is.na(value_info)) |> select(ds_key, value_info))
  ds_keys_mask <- d_datasets |> filter(is_mask) |> pull(ds_key)

  # * the schema adapter: v1-v7 speak mdl_seq, v8 speaks mdl_key ----
  #
  # `id_field` in the manifest is the whole story. v1-v7 identify a model by an INTEGER
  # `mdl_seq` that renumbers between releases; v8 by the stable string `mdl_key`. The rest of
  # this app speaks the v8 shape, so each release is normalised HERE, once, into:
  #
  #   d_spp        one row per listed taxon, `mdl_key` = its MERGED model id (as character)
  #   d_edges      (ms_merge_key, mdl_key, ds_key) -- one row per INPUT model, merged excluded
  #   native_asset (ms_merge_key, mdl_key, ds_key, asset_type, representation, asset_url, ...)
  #
  # For v1-v7 that means reading `model_asset` (per-model COGs on the usa05 grid, published by
  # backfill_versions) as the asset registry, and joining `taxon_model(taxon_id, ds_key,
  # mdl_seq)` through `taxon` to recover which taxon each model feeds. Two traps, both real:
  #   - v1-v7 `taxon_model` INCLUDES an `ms_merge` edge and `n_ds` counts it; v8's does not.
  #     So the input count is taken from the normalised edges, never from `n_ds`.
  #   - v1-v7 validity is `is_ok`, v8 splits it into is_valid_global / is_valid_usa.
  id_field   <- if (!is.null(zone_manifest)) zone_manifest$id_field %||% "mdl_key" else "mdl_key"
  is_mdl_seq <- identical(id_field, "mdl_seq")
  taxon_cols <- colnames(tbl(con_sdm, "taxon"))

  if (is_mdl_seq) {
    # ---- v1-v7 ----------------------------------------------------------------
    d_spp <- tbl(con_sdm, "taxon") |>
      filter(if ("is_ok" %in% taxon_cols) is_ok else TRUE, !is.na(mdl_seq)) |>
      filter(!sp_cat %in% c("reptile", "amphibian")) |>
      collect() |>
      transmute(
        taxon_id, taxon_authority,
        scientific_name, common_name, sp_cat,
        mdl_key      = as.character(mdl_seq),
        redlist_code = if ("redlist_code" %in% taxon_cols) redlist_code else NA_character_,
        esa_code     = if ("extrisk_code" %in% taxon_cols) extrisk_code else NA_character_,
        esa_source   = if ("esa_source"   %in% taxon_cols) esa_source   else NA_character_,
        er_score     = if ("er_score"     %in% taxon_cols) er_score     else NA_real_,
        rarity       = NA_character_,
        is_mmpa      = if ("is_mmpa" %in% taxon_cols) is_mmpa else NA,
        is_mbta      = if ("is_mbta" %in% taxon_cols) is_mbta else NA,
        # every listed taxon of a v1-v7 release is a US-study-area taxon by construction
        is_valid_usa = TRUE)
    # taxon_model keys on taxon_id; recover the merged id from the taxon it belongs to, and
    # drop the ms_merge self-edge so `d_edges` means INPUTS on every version
    d_edges <- tryCatch(
      tbl(con_sdm, "taxon_model") |> collect() |>
        transmute(taxon_id = as.character(taxon_id), ds_key, mdl_key = as.character(mdl_seq)),
      error = function(e) tibble(taxon_id = character(), ds_key = character(), mdl_key = character()))
    d_edges <- d_edges |>
      inner_join(d_spp |> transmute(taxon_id = as.character(taxon_id), ms_merge_key = mdl_key),
                 by = "taxon_id") |>
      filter(ds_key != "ms_merge", mdl_key != ms_merge_key) |>
      select(ms_merge_key, mdl_key, ds_key)
    # model_asset IS the v1-v7 native_asset: one COG per model, already public on S3
    native_asset <- tryCatch(
      tbl(con_sdm, "model_asset") |> collect() |>
        transmute(mdl_key = as.character(mdl_seq), ds_key, asset_url = cog_url),
      error = function(e) tibble(mdl_key = character(), ds_key = character(), asset_url = character()))
    native_asset <- native_asset |>
      # an input model maps to the taxa it feeds; a merged model maps to itself
      left_join(d_edges |> distinct(mdl_key, ms_merge_key), by = "mdl_key",
                relationship = "many-to-many") |>
      mutate(ms_merge_key = coalesce(ms_merge_key, mdl_key)) |>
      filter(ms_merge_key %in% d_spp$mdl_key | mdl_key %in% d_spp$mdl_key) |>
      transmute(
        ms_merge_key, mdl_key, ds_key,
        asset_type = "cog", representation = "native", asset_url,
        rescale_min = 1L, rescale_max = 100L, colormap = "spectral_r",
        source_layer = NA_character_,
        xmin = NA_real_, ymin = NA_real_, xmax = NA_real_, ymax = NA_real_)
  } else {
    # ---- v8 -------------------------------------------------------------------
    # base picker set = every taxon valid in EITHER merged surface. v8 merges two — a global one
    # and a US-scoped scoring one — and NEITHER flag is a superset of the other: 4,473 taxa are
    # global-only, and 46 are US-only (their whole distribution is US waters, so they are scored
    # here yet absent from the global surface). Filtering on is_valid_global alone, as this did,
    # hid those 46 scored species from the picker entirely.
    #
    # Each row still carries is_valid_usa so the "Only species in US waters" checkbox can filter
    # to US presence. A taxon is is_valid_usa iff it has >=1 merged cell in US waters
    # (merge_taxon: n_usa>0) — so the ~750 AquaMaps over-predictions whose IUCN range is wholly
    # outside the US (Sotalia etc.) are is_valid_usa=FALSE.
    d_spp_tbl <- tbl(con_sdm, "taxon") |>
      filter(is_marine, !is.na(ms_merge_key), !sp_cat %in% c("reptile", "amphibian"))
    d_spp_tbl <- if (all(c("is_valid_global", "is_valid_usa") %in% taxon_cols))
      filter(d_spp_tbl, coalesce(is_valid_global, FALSE) | coalesce(is_valid_usa, FALSE))
    else if ("is_valid_global" %in% taxon_cols) filter(d_spp_tbl, is_valid_global)
    else filter(d_spp_tbl, is_valid_usa)
    d_spp <- d_spp_tbl |>
      select(
        taxon_id, taxon_authority, scientific_name, common_name, sp_cat,
        mdl_key = ms_merge_key,
        redlist_code = iucn_code, esa_code = extrisk_code, er_score,
        rarity, is_mmpa, is_mbta, is_valid_usa) |>
      collect() |>
      mutate(esa_source = NA_character_)
    d_edges <- tryCatch(
      tbl(con_sdm, "taxon_model") |> collect() |>
        transmute(ms_merge_key, mdl_key, ds_key = str_extract(mdl_key, "^[^|]+")),
      error = function(e) {
        message("taxon_model unavailable (", conditionMessage(e), ") — inputs from native_asset")
        tibble(ms_merge_key = character(), mdl_key = character(), ds_key = character()) })
    native_asset <- tryCatch(
      tbl(con_sdm, "native_asset") |> collect(),
      error = function(e) tibble(
        ms_merge_key = character(), mdl_key = character(), ds_key = character(),
        asset_type = character(), representation = character(), asset_url = character(),
        rescale_min = integer(), rescale_max = integer(), colormap = character(),
        source_layer = character(), xmin = double(), ymin = double(), xmax = double(), ymax = double()))
    if (!nrow(d_edges))
      d_edges <- native_asset |> filter(ds_key != "ms_merge") |> distinct(ms_merge_key, mdl_key, ds_key)
  }

  d_spp <- d_spp |>
    mutate(
      lbl_cmn = ifelse(!is.na(common_name) & common_name != "",
                       glue(" ({common_name})", .trim = F), ""),
      label = glue("{sp_cat}: {scientific_name}{lbl_cmn}"),
      worms_url = ifelse(
        taxon_authority == "worms" & !is.na(taxon_id),
        glue('<a href="https://www.marinespecies.org/aphia.php?p=taxdetails&id={taxon_id}" target="_blank">{taxon_id}</a>'),
        NA_character_))

  # grouped-by-category choice lists; the checkbox swaps between US-waters-only and all valid marine
  .make_choices <- function(df) df |>
    arrange(sp_cat, label) |>
    group_by(sp_cat) |>
    summarise(layer = list(setNames(mdl_key, label)), .groups = "drop") |>
    deframe()
  spp_choices_all <- .make_choices(d_spp)
  spp_choices_us  <- .make_choices(d_spp |> filter(coalesce(is_valid_usa, FALSE)))
  spp_choices     <- spp_choices_us   # default view = "Only species in US waters" (checkbox TRUE)

  sel_sp_default <- d_spp |> filter(scientific_name == "Dermochelys coriacea",
                                    coalesce(is_valid_usa, FALSE)) |> pull(mdl_key)
  if (length(sel_sp_default) == 0)
    sel_sp_default <- (d_spp |> filter(coalesce(is_valid_usa, FALSE)) |> pull(mdl_key))[1]

  # * inputs = what a taxon is ACTUALLY built from, availability tracked separately ----
  #
  # The input list used to be derived from `native_asset` — the registry of published
  # ASSETS — which silently conflated two different questions: "what fed this taxon" and
  # "what can we draw". When the v8 registry lost its vector-range PMTiles rows, every
  # range input simply vanished from the bar, which went on reporting a count taken from
  # `taxon.n_datasets`: the leatherback advertised 5 inputs (really 6) and offered 1.
  #
  # `d_edges` (normalised above from each release's taxon_model) IS the taxon->model relation
  # the merge consumed, so inputs are listed from it and availability becomes a separate,
  # VISIBLE property: an input with no published surface renders as a disabled pill, not as
  # an absence.
  # older releases had no `representation` column (all rows were the single per-model asset)
  if (!"representation" %in% names(native_asset)) native_asset$representation <- "native"
  # the picker lists only the valid taxa; edges to anything else are not drawable here
  d_edges <- d_edges |> filter(ms_merge_key %in% d_spp$mdl_key)

  # published-asset lookup: does this raw input have a surface we can actually render?
  asset_keys <- unique(native_asset$mdl_key)
  has_asset  <- function(mk) !is.na(mk) & mk %in% asset_keys

  # INPUT COUNT per taxon, counted from the normalised edges rather than taken from a stored
  # column. `n_ds`/`n_datasets` cannot be trusted across releases: v1-v7 count the ms_merge
  # edge (so the app subtracted one) and v8 does not (so subtracting one under-reported every
  # taxon by one — the leatherback's 6 inputs read as 5). Counting what we list cannot drift.
  # match(), not `[[`: `[[` on a table ERRORS with "subscript out of bounds" for an absent
  # key rather than returning NULL, so the is.null() guard never ran. Every v1 taxon whose
  # only model IS its merged model has no input edges, so the layer bar died on the first
  # such species — visible only in the browser, since a bundle-level check never renders it.
  n_inputs_of <- local({
    tb <- table(d_edges$ms_merge_key)
    nm <- names(tb); vals <- as.integer(tb)
    function(mk) { i <- match(as.character(mk)[1], nm); if (is.na(i)) 0L else vals[i] }
  })

  # wide per-taxon input columns: one column per ds_key holding that input's raw mdl_key (or NA),
  # so `sp_row[[ds_key]]` gives the input to render for the selected taxon (v7 mapsp pattern).
  d_inputs <- d_edges |>
    distinct(ms_merge_key, ds_key, mdl_key) |>
    pivot_wider(names_from = ds_key, values_from = mdl_key, values_fn = dplyr::first)
  d_spp <- d_spp |> left_join(d_inputs, by = c("mdl_key" = "ms_merge_key"))
  input_ds_keys <- intersect(ds_keys, names(d_inputs))   # ds_keys offered as input layers

  # how much of the true input list this release can draw — logged at startup so a registry
  # that loses an asset class is visible in the app log instead of only in the UI
  local({
    n_edge <- nrow(d_edges); n_ok <- sum(has_asset(d_edges$mdl_key))
    if (n_edge && n_ok < n_edge)
      message(glue("native_asset covers {n_ok}/{n_edge} taxon-model inputs ",
                   "({n_edge - n_ok} shown as unavailable): ",
                   paste(sort(unique(d_edges$ds_key[!has_asset(d_edges$mdl_key)])), collapse = ", ")))
  })

  # render lookup: raw input mdl_key -> how to draw it (asset_type/url/rescale/colormap/bbox).
  # native_asset carries BOTH representations per mdl_key: the ORIGINAL source surface
  # (representation=="native": AquaMaps 0.5° COG, vector-range PMTiles) and the INTERPOLATED
  # 0.05°-grid surface used in scoring (representation=="model": a gridded COG). Keep every
  # representation; the layer bar offers an Original/Interpolated toggle per input and the
  # render picks the row. pick_asset falls back to native when a rep is absent (e.g. vector
  # inputs have no model COG).
  # SORTED TABLE + INDEX, not split(). All three callers below want the same
  # thing -- "the native_asset rows for this mdl_key" -- and `split()` answered it
  # by materialising one tibble per key: 47,034 of them, 156 MB, to hold a table
  # that is 21.7 MB. That single line was 70% of this app's bundle and most of
  # the reason a species worker cost 222 MB against the scores app's 9.6 MB.
  # Sorting once and remembering each key's row range costs ~5 MB and answers in
  # O(1) through a hashed environment.
  native_asset <- native_asset[order(native_asset$mdl_key), , drop = FALSE]
  .na_runs  <- rle(native_asset$mdl_key)
  .na_end   <- cumsum(.na_runs$lengths)
  .na_start <- .na_end - .na_runs$lengths + 1L
  .na_at    <- new.env(hash = TRUE, parent = emptyenv(), size = length(.na_runs$values))
  for (.i in seq_along(.na_runs$values)) assign(.na_runs$values[.i], .i, envir = .na_at)
  # the rows for one key (zero-row frame when the key has no asset), the shape
  # every caller used to get from native_by_key[[mk]]
  rows_for <- function(mk) {
    if (is.null(mk) || is.na(mk) || !nzchar(mk)) return(native_asset[0, , drop = FALSE])
    i <- .na_at[[mk]]
    if (is.null(i)) return(native_asset[0, , drop = FALSE])
    native_asset[.na_start[i]:.na_end[i], , drop = FALSE]
  }
  reps_for   <- function(mk) unique(rows_for(mk)$representation)
  pick_asset <- function(mk, rep = "native") {
    a <- rows_for(mk)
    if (!nrow(a)) return(NULL)
    r <- a[a$representation == rep, , drop = FALSE]
    if (!nrow(r)) r <- a[order(a$representation != "native"), , drop = FALSE]   # fallback: native first
    r[1, ]
  }

  # URL routing: a ?mdl_key=<key> may name the merged model OR any raw input -> resolve to
  # (merged model, ds_layer) so the picker opens on that layer.
  mdl_key_lookup <- bind_rows(
    d_spp   |> transmute(merged_mdl_key = mdl_key, ds_layer = "mdl_key", input_mdl_key = mdl_key),
    d_edges |> distinct(ms_merge_key, ds_key, mdl_key) |>
      transmute(merged_mdl_key = ms_merge_key, ds_layer = ds_key, input_mdl_key = mdl_key))


  environment()
}

# memoised: a bundle opens a DuckDB connection, reads the manifest and normalises the
# release's tables, so a second visitor asking for the same version must not pay it again
bundle <- function(v) {
  v <- as.character(v)[1]
  if (is.null(.bundles[[v]])) .bundles[[v]] <- build_bundle(v)
  .bundles[[v]]
}

# ?ver= from a request/session, resolved against the published registry. An unknown or absent
# value falls back to the PROMOTED release (latest.txt) rather than erroring -- this app used
# to default to a hardcoded v8, so /species/ served the pre-release while /scores/ served v7.
#
# `allow_access` is the pre-release review gate: this process may resolve only
# the `access` values msens::atlas_allow_access() returns -- "public" on the
# public Shiny Server instance, everything on the PREVIEW instance (a second
# Shiny Server block whose wrapper app.R sets MS_PREVIEW=1, reachable only via
# the signed-in preview.marinesensitivity.org vhost). Policy is an env var of the
# PROCESS, never a request header: shiny-server opens its own websocket to this
# worker, so no proxy header reaches session$request. See scores/app.R.
ver_of <- function(qs) {
  v <- tryCatch({
    q <- shiny::parseQueryString(qs %||% "")
    msens::atlas_resolve_ver(q$ver, allow_access = msens::atlas_allow_access())
  }, error = function(e) NULL)
  if (is.null(v)) tryCatch(msens::atlas_resolve_ver(NULL, allow_access = msens::atlas_allow_access()),
                           error = function(e) VER_FALLBACK) else v
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

# preview instance chrome: the signed-in reviewer, from Caddy's X-MS-User header
# (set from the verified Cloudflare Access JWT; present only on the page GET,
# stripped on the public vhost). Display only -- policy is MS_PREVIEW, above.
preview_badge <- function(req) {
  if (!msens::atlas_is_preview()) return(NULL)
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
# the same source as product_nav(), for links OUTSIDE the nav (the welcome modal)
product_url <- function(ver, key) msens::product_urls(
  ver, access = if (msens::atlas_is_preview()) "restricted" else "public")[[key]]

product_nav <- function(ver, current) {
  u   <- msens::product_urls(
    ver, access = if (msens::atlas_is_preview()) "restricted" else "public")
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

# ui ----
# ui is a FUNCTION of the request, not a static object, for one reason: the
# client IP. shiny-server does not proxy the websocket upgrade — it opens a
# fresh localhost connection to the R worker — so the server session sees
# REMOTE_ADDR 127.0.0.1 and no X-Forwarded-For (Caddy sets it correctly; it is
# lost at the shiny-server hop). This page request is the only one that still
# carries the real address, so it is captured here and baked into the snippet.
ui_impl <- function(req) page_sidebar(
  # mobile: keep the page fillable so the map takes what the controls leave
  # (the default, FALSE, is why the map was a sliver below them on a phone)
  fillable_mobile = TRUE,
  tags$head(
    tags$link(rel = "icon", type = "image/x-icon", href = "favicon.ico"),
    # curl-checkable sentinels (see scores/app.R): which release this page
    # renders, and whether this is the preview instance
    tags$meta(name = "ms-ver",     content = ver),
    tags$meta(name = "ms-preview", content = if (msens::atlas_is_preview()) "1" else "0"),
    # usage tracking: GA4 (aggregate) + a batched beacon to the usage-log Sheet
    # (detail). Both legs are driven from the browser, so no reactive ever
    # performs network I/O — see msens::ga_js(). The Sheet leg is a silent no-op
    # unless MSENS_LOG_URL is set, so local dev writes nothing. Reviewer sessions
    # on the preview instance are tagged apart from public counts.
    msens::ga_head(if (msens::atlas_is_preview()) "species-preview" else "species",
                   app_version = APP_VERSION,
                   ip = msens::ms_client_ip(req)),
    tags$style(HTML("
      .maplibregl-popup-content{color:black;}
      #ds_layer_container {display: none;}
      #representation_container {display: none;}
      .layer-bar .rep-pill { margin: 0 1px; font-style: italic; }
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

      /* layer status bar */
      .layer-bar {
        display: flex; align-items: center; gap: 6px;
        padding: 6px 12px; border-radius: 6px;
        margin-top: -8px; margin-bottom: 8px; font-size: 0.9em;
      }
      .layer-bar.is-merged { background-color: #198754; color: white; }
      .layer-bar.is-input  { background-color: #fd7e14; color: white; }
      .layer-bar .layer-icon  { font-size: 1.1em; margin-right: 2px; }
      .layer-bar .layer-label { font-weight: 600; }
      .layer-bar .layer-links {
        margin-left: auto; display: flex; gap: 4px;
        flex-wrap: wrap; align-items: center;
      }
      .layer-bar .layer-pill {
        display: inline-block; padding: 2px 10px; border-radius: 12px;
        cursor: pointer; font-size: 0.85em;
        border: 1.5px solid rgba(255,255,255,0.5);
        background: rgba(255,255,255,0.15); color: inherit;
        text-decoration: none; transition: background 0.15s, border-color 0.15s;
      }
      .layer-bar .layer-pill:hover {
        background: rgba(255,255,255,0.3); border-color: rgba(255,255,255,0.8);
      }
      .layer-bar .layer-pill.active {
        background: rgba(255,255,255,0.35); border-color: white; font-weight: 700;
      }
      /* fed the merge, but this release publishes no surface for it */
      .layer-bar .layer-pill.unavailable {
        cursor: not-allowed; opacity: 0.5; text-decoration: line-through;
        border-style: dashed; background: transparent;
      }
      .layer-bar .layer-pill.unavailable:hover {
        background: transparent; border-color: rgba(255,255,255,0.5);
      }
      .layer-bar .merged-link {
        text-decoration: underline; cursor: pointer; color: inherit; font-weight: 600;
      }

      /* selected taxon, as SELECTABLE text (apps#10). selectize renders its item in a
         div that swallows clicks to open the dropdown, so the names in the picker cannot
         be dragged over or right-clicked -> Copy. This line is ordinary DOM. */
      .sp-title {
        display: flex; align-items: baseline; flex-wrap: wrap; gap: 2px;
        margin: -6px 0 8px 2px; font-size: 1.05em;
        -webkit-user-select: text; user-select: text; cursor: text;
      }
      .sp-title .sci { font-style: italic; }
      .sp-title .sep { opacity: 0.45; margin: 0 6px; }
      .sp-copy {
        border: none; background: none; color: inherit; cursor: pointer;
        opacity: 0.45; padding: 0 3px; font-size: 0.85em; line-height: 1;
      }
      .sp-copy:hover, .sp-copy:focus { opacity: 1; }
      .sp-copy.copied      { opacity: 1; color: #198754; }
      .sp-copy.copy-failed { opacity: 1; color: #dc3545; }
      .layer-bar .layer-toggle { display: none; }
      .layer-bar .layer-toggle::after { content: ' \u25BE'; }
      .layer-bar.expanded .layer-toggle::after { content: ' \u25B4'; }
      /* ---- mobile (bslib's own sidebar breakpoint) -----------------------------
         The map used to be invisible on a phone: page_sidebar() is NOT fillable on
         mobile by default (.bslib-flow-mobile makes every fill item flex:0 0 auto,
         so the map card kept its intrinsic ~0 height) and sidebar(open = NULL)
         resolves to mobile = 'always' (stacked below main, no toggle). ui_impl now
         sets fillable_mobile = TRUE, and this sidebar's open = F already means closed
         on mobile, so bslib draws its toggle row and overlays the sidebar on the map.
         What is left for CSS: fit the header, pickers and layer bar in ~400px. */
      .ms-header { display: flex; align-items: center; width: 100%; }
      @media (max-width: 575.98px) {
        .bslib-page-sidebar { --bslib-spacer: 0.5rem; }
        .bslib-sidebar-layout { --bslib-sidebar-padding: 0.5rem; }
        .bslib-page-sidebar > .navbar { --bs-navbar-padding-y: 0.3rem; }
        .ms-header { flex-wrap: wrap; row-gap: 2px; }
        .ms-header .ms-title { flex: 1 1 auto; }
        .ms-header .header-right { flex: 0 0 auto; margin-left: auto; padding-left: 8px; }
        .ms-header .header-nav { order: 3; flex-basis: 100%; margin-left: 0; }
        .ms-title-sub { display: none; }
        /* the pickers speak for themselves; their labels cost a row each */
        #tour_sp .control-label, #tour_mask .control-label { display: none; }
        #tour_sp .form-group, #tour_mask .form-group { margin-bottom: 0.25rem; }
        .sp-title { margin: 0 0 4px 2px; font-size: 0.95em; }
        .layer-bar { flex-wrap: wrap; padding: 4px 8px; margin-top: 0; }
        .layer-bar .layer-toggle { display: inline-block; margin-left: auto; }
        .layer-bar .layer-links { display: none; flex-basis: 100%; margin-left: 0; padding-top: 4px; }
        .layer-bar.expanded .layer-links { display: flex; }
      }
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('updateTitle', function(title) {
        document.title = title;
      });
      // open a map popup at a clicked location on the first click (mapgl's marker popup
      // otherwise needs a second click on the marker to open).
      Shiny.addCustomMessageHandler('clickPopup', function(m) {
        var w = HTMLWidgets.find('#' + m.map);
        var map = w && w.getMap ? w.getMap() : null;
        if (!map || typeof maplibregl === 'undefined') return;
        if (window._msPopup) { window._msPopup.remove(); }
        window._msPopup = new maplibregl.Popup({ closeButton: true, closeOnClick: true, maxWidth: '260px' })
          .setLngLat([m.lng, m.lat]).setHTML(m.html).addTo(map);
      });
      $(document).on('shiny:connected', function() {
        var params = new URLSearchParams(window.location.search);
        if (params.get('splash') === 'false') {
          Shiny.setInputValue('show_splash_pref', 'false');
        } else {
          var show = localStorage.getItem('msens_mapsp_show_splash');
          Shiny.setInputValue('show_splash_pref', show === null ? 'true' : show);
        }
      });
      Shiny.addCustomMessageHandler('saveSplashPref', function(val) {
        localStorage.setItem('msens_mapsp_show_splash', val);
      });

      // ---- copy a species name to the clipboard (apps#10) -------------------
      // Delegated from document, because #sp_title is re-rendered on every
      // selection and a handler bound to the button would die with it.
      document.addEventListener('click', function(e) {
        var b = e.target.closest ? e.target.closest('.sp-copy') : null;
        if (!b) return;
        var txt  = b.getAttribute('data-copy') || '';
        // restore innerHTML, not textContent: the button holds an <i> icon
        var prev  = b.innerHTML;
        var flash = function(ok) {
          b.innerHTML = ok ? '\\u2713' : '\\u2717';
          b.classList.add(ok ? 'copied' : 'copy-failed');
          setTimeout(function() {
            b.innerHTML = prev;
            b.classList.remove('copied', 'copy-failed');
          }, 1200);
        };
        // execCommand on a throwaway textarea. Both the pre-secure-context
        // fallback AND the recovery path: navigator.clipboard.writeText REJECTS
        // with NotAllowedError whenever the document is not focused, and an
        // empty rejection handler turns that into a button that does nothing
        // and says nothing. Verified in-browser — that is exactly what the
        // first version of this did.
        var legacy = function() {
          var ta = document.createElement('textarea');
          ta.value = txt;
          ta.setAttribute('readonly', '');
          ta.style.position = 'fixed'; ta.style.top = '0'; ta.style.opacity = '0';
          document.body.appendChild(ta);
          ta.select();
          ta.setSelectionRange(0, txt.length);
          var ok = false;
          try { ok = document.execCommand('copy'); } catch (err) { ok = false; }
          document.body.removeChild(ta);
          flash(ok);
        };
        if (navigator.clipboard && window.isSecureContext) {
          navigator.clipboard.writeText(txt).then(function() { flash(true); }, legacy);
        } else {
          legacy();
        }
      });

      // ---- species search terms (incl. the ones that find NOTHING) ----------
      // The server only ever sees a *completed* selection, so a search that
      // returned no match — arguably the more actionable signal — is invisible
      // to it. Hook selectize's `type` event to capture the query itself.
      //
      // Debounced to the settled query: without it every keystroke of
      // 'Dermochelys' would be its own event.
      //
      // A generic $(document).on('change','select') handler (as used in the
      // CalCOFI app) does NOT work here: selectize hides the underlying <select>
      // and its .val() is the mdl_key, not anything human-readable.
      //
      // WHY NO RESULT COUNT (verified in-browser, do not re-add): with
      // server=TRUE selectize's `currentResults.total` cannot distinguish a hit
      // from a miss. A zero-match query leaves the previously loaded options in
      // place (total reads back as the full set, not 0), while a query that DOES
      // match makes the server replace the option set (so total === loaded too).
      // Both states look identical, and 'Balaenoptera' — many real matches —
      // came back indistinguishable from a nonsense string.
      //
      // The query text is reliable, so log only that. Whether it matched is
      // recovered far more robustly at analysis time, two ways: join the logged
      // query against the taxon list, or treat a `search_species` with no
      // following `select_species` in the same session as an unsuccessful search.
      $(document).on('shiny:connected', function() {
        var attach = function(tries) {
          var el = document.getElementById('sel_sp');
          if (!el || !el.selectize) {
            if (tries > 0) setTimeout(function(){ attach(tries - 1); }, 500);
            return;
          }
          var st = el.selectize, timer = null, last = '';
          st.on('type', function(str) {
            clearTimeout(timer);
            timer = setTimeout(function() {
              str = (str || '').trim();
              if (str.length < 3 || str === last) return;   // ignore prefixes/repeats
              last = str;
              window.msTrack('search_species', { query: str });
            }, 900);
          });
        };
        attach(20);   // selectize is created async with server=TRUE
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
         span(class = "ms-title-sub", " species distribution"),
         preview_badge(req)),
    product_nav(ver, "species"),
    div(
      class = "header-right",
      actionLink("btn_about", "About"),
      input_dark_mode(id = "tgl_dark", mode = "dark")
    )
  ),
  # Same fix scores/app.R already carries. Without it bslib derives the browser
  # and bookmark title by FLATTENING the `title` argument, and ours is a div of
  # controls -- the tab read "BOEM Marine Sensitivity (v8) species distribution
  # Scores · Species · Docs · Home About bslib-component-js 0.12.0
  # components/dist components.min.js ...". The server replaces this with the
  # taxon once one renders (the `updateTitle` message), so it was only ever the
  # INITIAL title -- which is exactly what a bookmark keeps. Adding the nav made
  # a latent mess visible; carrying the version means a bookmark says which
  # release it points at.
  window_title = glue("BOEM Marine Sensitivity ({ver}) species distribution"),

  sidebar = sidebar(
    open = F,
    input_switch(
      "tgl_sphere",
      "Sphere",
      T
    ),
    # OBIS occurrences overlay: per-species h3t hexagons of OBIS record counts (default off; shown
    # only when obisindicators is available, and applies to taxa with a WoRMS AphiaID).
    if (has_obis) input_switch(
      "tgl_obis",
      "OBIS occurrences",
      F
    ),
    uiOutput("species_info")
  ),
  fluidRow(
    column(9,
      tags$div(
        id = "tour_sp",
        selectizeInput(
          "sel_sp",
          "Species:",
          choices = NULL,
          width   = "100%"),
        tags$div(
          style = "margin-top: -8px;",
          checkboxInput(
            "us_only",
            "Only species in US waters",
            value = TRUE)))),
    column(3,
      tags$div(
        id = "tour_mask",
        selectInput(
          "sel_mask",
          "Outlines:",
          choices  = c("Program Areas (white)" = "programarea_key",
                       "Ecoregions (black)"    = "ecoregion_key",
                       "None"                  = "none"),
          selected = "ecoregion_key",
          width    = "100%")))
  ),
  # the selected taxon as plain, selectable text — the names ARE on the page (the browser
  # tab title carries them) but nowhere a person can select them, since selectize's item is
  # not selectable text. Below the selector, above the map, per apps#10.
  uiOutput("sp_title"),
  uiOutput("layer_bar"),
  # hidden radioButtons to maintain ds_layer input
  div(
    id = "ds_layer_container",
    radioButtons(
      "ds_layer",
      "Display Layer",
      choices  = c("Merged Model" = "mdl_key"),   # v8: only the merged surface is served
      selected = "mdl_key",
      inline   = TRUE
    )
  ),
  # hidden radioButtons for the native/model (Original/Interpolated) toggle; driven by the
  # layer-bar rep pills, shown only for inputs that publish both representations (AquaMaps)
  div(
    id = "representation_container",
    radioButtons(
      "representation",
      "Representation",
      choices  = c("Original (native)" = "native", "Interpolated (model)" = "model"),
      selected = "native",
      inline   = TRUE
    )
  ),
  card(
    style = "position: relative;",
    maplibreOutput("map"),
    # "Zoom to layer" overlaid at the map's top-left, right of the layer selector. Auto-fit on layer
    # switch is off (so users can compare models at one view); this restores fit-to-extent on demand.
    tags$div(
      style = "position: absolute; top: 27px; left: 75px; z-index: 2;",   # top-aligned + 10px right of the layers control (card pad 17 + ctrl margin 10 + icon 38)
      actionButton(
        "btn_zoom_extent", "Zoom to layer",
        icon  = icon("expand"),
        class = "btn-sm",
        style = "background:#fff; color:#333; border:1px solid #bbb; box-shadow:0 1px 4px rgba(0,0,0,.3);"))
  )
)

# server ----
server_impl <- function(input, output, session) {

  # version picker ----
  # One app renders any published release, so the header says which one is on
  # screen and offers the rest. Markup comes from msens::version_picker_html()
  # off the same versions.json the pipeline and docs read, so the three cannot
  # disagree about what exists.

  # ?ver= — the version is a URL parameter, not a fork of this app ----
  # This app now RENDERS the requested release (the session was enclosed in its bundle before
  # this observer ran), so the only thing left to report is a version that does not exist.
  # It used to answer "Version v7 is not served here yet" for every release but v8 — while
  # also defaulting to v8, the PRE-release, when no ?ver= was given at all.
  observeEvent(session$clientData$url_search, once = TRUE, {
    q   <- parseQueryString(session$clientData$url_search)
    req <- q$ver
    if (is.null(req) || !nzchar(req)) return(invisible())
    resolved <- tryCatch(
      msens::atlas_resolve_ver(req, allow_access = msens::atlas_allow_access()),
      msens_restricted = function(e) e, error = function(e) NULL)
    if (inherits(resolved, "msens_restricted")) {
      # a pre-release under review: say so, and point at the door (ver_of()
      # already refused it, so `ver` is the promoted release)
      pv <- tryCatch(msens::preview_app_url("species", req), error = function(e) msens::atlas_preview_url())
      showModal(modalDialog(
        title = glue("Version {htmltools::htmlEscape(req)} is under review"), easyClose = TRUE,
        p(HTML(glue("<code>?ver={htmltools::htmlEscape(req)}</code> is a pre-release restricted ",
                    "to reviewers. Showing <b>{ver}</b>."))),
        p("Reviewers sign in at ", a(href = pv, target = "_blank", pv), ".")))
    } else if (is.null(resolved))
      showModal(modalDialog(
        title = "Unknown data version", easyClose = TRUE,
        p(HTML(glue("<code>?ver={htmltools::htmlEscape(req)}</code> is not a published ",
                    "version. Showing <b>{ver}</b>."))),
        p("Published versions are listed at ",
          a(href = paste0(msens::atlas_base_url(), "/versions.json"),
            target = "_blank", "versions.json"), ".")))
  })

  # echo the version into the URL so a shared link is explicit about what it shows
  observe({
    q <- parseQueryString(isolate(session$clientData$url_search))
    # `mdl_seq` counts as owned too — it is the v1-v7 spelling of the same parameter. Left
    # out, this observer rewrote the URL to a bare ?ver= before the deep-link observer had
    # read it, so a published /mapsp/?mdl_seq= link raced against its own fix.
    if (is.null(q$mdl_key) && is.null(q$mdl_seq) &&  # the ?mdl_key= observer owns the URL otherwise
        !msens::atlas_is_preview())                  # preview: the version is the PATH; Caddy forces ?ver=
      updateQueryString(glue("?ver={ver}"), mode = "replace", session = session)
  })

  observeEvent(input$show_versions, {
    showModal(modalDialog(
      title = "Data version", easyClose = TRUE, size = "l",
      p("This app renders one published release of the Marine Sensitivity Toolkit."),
      tryCatch(
        # public instance: restricted releases link OUT to the signed-in preview host
        # preview instance: the version is the PATH there (/v9/species/), so every
        # row links by path; public instance: restricted rows link OUT to the
        # signed-in preview host, public rows stay in-app (?ver=)
        if (msens::atlas_is_preview())
          msens::version_picker_html(ver, href = function(v) sprintf("/%s/species/", v))
        else
          msens::version_picker_html(
            ver, href_restricted = function(v) msens::preview_app_url("species", v)),
        error = function(e)
          p(class = "text-muted", "Version list unavailable: ", conditionMessage(e)))))
  })
  # usage tracking ----
  # msens::ms_track() only pushes a websocket message the session already has
  # open — no HTTP request — so instrumenting the species picker adds no latency
  # to the map render that follows. `ignoreInit = TRUE` so the default species
  # and default toggles at startup aren't logged as user selections.
  # push the session token (and a fallback IP) to the browser before any event,
  # so no logged row is missing them. The IP is only a fallback: the page
  # request already supplied the real one — see msens::ms_track_session().
  msens::ms_track_session(session)

  trk <- function(event, ...) msens::ms_track(session, event, ...)

  # WHICH SPECIES — the headline signal. input$sel_sp is a merged mdl_key, so
  # resolve it against d_spp here: the browser only has an opaque key, and the
  # names are exactly what makes the usage log readable. High cardinality
  # (~16k taxa) is why this detail belongs in the Sheet leg — GA4 buckets
  # dimensions this wide into "(other)" once past its daily cardinality limit.
  # de-duplicated: toggling `us_only` (or a ?mdl_key= deep link) calls
  # updateSelectizeInput to swap the choice list, which re-fires this observer —
  # transiently with an unresolvable value, then again with the SAME species.
  # Logging those would inflate species counts with selections the user never
  # made, so drop unresolved values and repeats of the last logged taxon.
  #
  # SEEDED WITH THE DEFAULT: the app opens on sel_sp_default, and that arrives
  # as a normal input change (ignoreInit only skips observer creation, when the
  # input is still NULL). Without seeding, every session would log a selection
  # of the default species that no user made — quietly making it the most
  # "viewed" taxon in the data. A deep-linked species differs from the default,
  # so it still logs.
  last_sp <- reactiveVal(as.character(sel_sp_default[1]))
  observeEvent(input$sel_sp, {
    sp <- d_spp |> filter(mdl_key == input$sel_sp)
    if (nrow(sp) != 1) return()                              # mid-swap, not a selection
    if (identical(last_sp(), input$sel_sp)) return()          # same taxon re-emitted
    last_sp(input$sel_sp)
    trk("select_species",
        mdl_key         = sp$mdl_key,
        scientific_name = sp$scientific_name,
        common_name     = sp$common_name,
        sp_cat          = sp$sp_cat,
        taxon_id        = sp$taxon_id,
        n_datasets      = n_inputs_of(sp$mdl_key),
        redlist_code    = sp$redlist_code,
        us_only         = isTRUE(input$us_only))
  }, ignoreInit = TRUE)

  # which surface is being looked at: the merged model vs a specific raw input
  # (AquaMaps, a vector range, ...), and native "Original" vs gridded
  # "Interpolated" — the layer question for this app.
  observeEvent(input$ds_layer,
               trk("select_layer", layer = input$ds_layer,
                   mdl_key = input$sel_sp %||% ""),
               ignoreInit = TRUE)
  observeEvent(input$representation,
               trk("select_representation", representation = input$representation,
                   mdl_key = input$sel_sp %||% ""),
               ignoreInit = TRUE)

  # map framing / overlays
  observeEvent(input$sel_mask, trk("select_outlines", outlines = input$sel_mask),
               ignoreInit = TRUE)
  observeEvent(input$us_only,
               trk("toggle_us_only", enabled = isTRUE(input$us_only)),
               ignoreInit = TRUE)
  observeEvent(input$btn_zoom_extent, trk("zoom_to_layer"), ignoreInit = TRUE)
  observeEvent(input$btn_about,       trk("open_about"),    ignoreInit = TRUE)
  if (has_obis)
    observeEvent(input$tgl_obis,
                 trk("toggle_obis", enabled = isTRUE(input$tgl_obis)),
                 ignoreInit = TRUE)

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
          "Explore individual species distribution models across US waters.",
          "Search by common or scientific name and view merged model or any input distribution (range or suitability).",
          "Also see:"),
        tags$ul(
          tags$li(tags$a(
            href   = product_url(ver, "scores"),
            target = "_blank",
            "Composite Scores app"), " for aggregated sensitivity maps"),
          tags$li(tags$a(
            href   = product_url(ver, "docs"),
            target = "_blank",
            "Documentation"), " for methods and data sources"))
      ),
      footer = tagList(
        checkboxInput(
          "chk_show_splash",
          tagList(
            "Show this welcome screen on startup", br(),
            helpText("Click About (upper right) to revisit this screen later")),
          value = TRUE),
        actionButton("btn_tour", "Take a Tour", icon = icon("route")),
        modalButton("Explore"))
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
      title    = "Select a Species",
      text     = "Search by common or scientific name. Results are grouped by category (bird, fish, mammal, etc.).",
      el       = "#tour_sp",
      position = "bottom"
    )$
    step(
      title    = "Mask Selection",
      text     = "Choose whether to overlay BOEM Program Area or Ecoregion boundaries on the map.",
      el       = "#tour_mask",
      position = "bottom"
    )$
    step(
      title    = "Layer Selector",
      text     = "The colored bar shows which model layer is displayed. Green means the final Merged Model; orange means you are viewing a single input. Click the pills to switch layers.",
      el       = ".layer-bar",
      position = "bottom"
    )$
    step(
      title    = "Species Map",
      text     = "The map shows the species distribution model. Cell values range from 1 (low sensitivity) to 100 (high sensitivity). Click cells for details.",
      el       = "#map",
      position = "top"
    )$
    step(
      title    = "Species Info",
      text     = "Open the sidebar to see ESA listing, IUCN status, MMPA/MBTA flags, and extinction risk score for the selected species.",
      el       = ".collapse-toggle",
      position = "right"
    )
  tour$init()
  if (verbose) message("conductor tour initialized")

  observe({
    if (verbose) message("starting conductor tour")
    removeModal()
    session$onFlushed(function() {
      tour$start()
      if (verbose) message("conductor tour started")
    }, once = TRUE)
  }) |>
    bindEvent(input$btn_tour)

  # rx_er_clr ----
  rx_er_clr <- reactiveVal(NULL)
  # rx_ds_layer: store ds_layer from URL to apply after species loads
  rx_ds_layer <- reactiveVal(NULL)
  # rx_marker_clicked: flag to prevent map_click from recreating marker when marker is clicked
  rx_marker_clicked <- reactiveVal(FALSE)
  # rx_url_initialized: flag to prevent re-processing URL after updateQueryString

  rx_url_initialized <- reactiveVal(FALSE)

  # v8 click-to-inspect: surfaces are titiler tiles / PMTiles, not an in-R raster, so record
  # what's currently shown (mdl_key + how it's served) and sample the value on click by
  # cell_id (merged cell-SQL) or a titiler /cog/point query (COG input).
  rx_shown <- reactiveVal(NULL)
  # last species the map recentered on — only re-fit bounds when the SPECIES changes, so
  # switching layers / original↔interpolated keeps the user's current zoom (proxy swaps in place)
  rx_fitted_sp <- reactiveVal(NULL)
  # current layer's fit extent, updated on each layer render; the "Zoom to layer extent" button reads it
  rx_fit_bbox <- reactiveVal(er_bbox)

  # url parameters ----
  observe({
    # only process URL params on initial load, not after updateQueryString
    if (rx_url_initialized()) return()

    query <- parseQueryString(session$clientData$url_search)
    # `?mdl_seq=` is the SAME parameter under v1-v7's name for it, and it is the form every
    # link published before v8 uses — the final report, the BOEM deliverables, issue #5. The
    # bundle already normalises a v1-v7 mdl_seq into `mdl_key`, so accepting the alias here is
    # all those links needed; without it they were silently answered with the default species.
    url_mdl_key <- query$mdl_key %||% query$mdl_seq
    if (!is.null(url_mdl_key)) {   # v8 mdl_key is a string (e.g. "ms_merge|WORMS:137209")

      # look up which species and layer this mdl_key belongs to
      lookup_row <- mdl_key_lookup |>
        filter(input_mdl_key == url_mdl_key)

      # resolve against ALL valid marine taxa (US + global); a non-US target shared via URL relaxes
      # the "Only species in US waters" checkbox so it can appear in the dropdown.
      us_keys  <- unlist(spp_choices_us,  use.names = FALSE)
      all_keys <- unlist(spp_choices_all, use.names = FALSE)
      select_target <- function(key) {
        is_us <- key %in% us_keys
        if (!is_us) updateCheckboxInput(session, 'us_only', value = FALSE)
        updateSelectizeInput(
          session, 'sel_sp',
          choices  = if (is_us) spp_choices_us else spp_choices_all,
          server   = T, selected = key)
      }

      # how people arrive: a shared/deep link naming a specific model. Tracked
      # with its resolution outcome, so dead links (a mdl_key retired by a
      # version bump) show up as a countable `not_found` rather than silently.
      # `resolution`, NOT `status`: status is a RESERVED name that ms_event()
      # hoists into its own Sheet column, which is meant for ok/error health.
      # Putting "input_model" there mixes a resolution KIND in with the
      # ok/error values ms_track_query() writes, spoiling that column for
      # filtering and charting.
      trk("deeplink_mdl_key",
          mdl_key    = url_mdl_key,
          resolution = if (nrow(lookup_row) > 0) "input_model"
                       else if (url_mdl_key %in% all_keys) "merged_model"
                       else "not_found")

      if (nrow(lookup_row) > 0) {
        # found: select the species (merged model) and store layer to apply later
        rx_ds_layer(lookup_row$ds_layer[1])
        select_target(lookup_row$merged_mdl_key[1])
      } else if (url_mdl_key %in% all_keys) {
        # valid merged model
        select_target(url_mdl_key)
      } else {
        # model not found — show disclaimer modal
        updateSelectizeInput(
          session,
          'sel_sp',
          choices  = spp_choices,
          server   = T,
          selected = sel_sp_default
        )
        showModal(modalDialog(
          title     = "Model not found",
          size      = "m",
          easyClose = TRUE,
          tags$div(
            style = "text-align: left;",
            tags$p(
              glue("The requested model (mdl_key={url_mdl_key}) is no longer ",
                   "available. It may have been modified or removed by a newer ",
                   "version of the Marine Sensitivity Toolkit.")),
            tags$p(
              "Please search for the species using the ",
              tags$strong("Species"), " dropdown above."),
            tags$p(
              "If the species is not listed, its expert range map (IUCN Red ",
              "List) falls entirely outside the US Exclusive Economic Zone, so ",
              "it has no modeled distribution in the study area.")),
          footer = modalButton("OK")
        ))
      }
    } else {
      updateSelectizeInput(
        session,
        'sel_sp',
        choices  = spp_choices,
        server   = T,
        selected = sel_sp_default
      )
    }

    # store invisible parameter (e.g., ?er_clr=test)
    if (!is.null(query$er_clr)) {
      rx_er_clr(query$er_clr)
    } else {
      rx_er_clr(NULL)
    }

    # mark URL initialization complete
    rx_url_initialized(TRUE)
  })

  # * us_only checkbox: swap the species dropdown between US-waters-only and all valid marine taxa,
  # keeping the current selection if it survives the filter (else fall back to the default).
  observeEvent(input$us_only, ignoreInit = TRUE, {
    ch   <- if (isTRUE(input$us_only)) spp_choices_us else spp_choices_all
    cur  <- isolate(input$sel_sp)
    keep <- if (!is.null(cur) && cur %in% unlist(ch, use.names = FALSE)) cur else sel_sp_default
    updateSelectizeInput(session, 'sel_sp', choices = ch, server = TRUE, selected = keep)
  })

  # * btn_zoom_extent: fit the map to the currently displayed layer's extent on demand ----
  observeEvent(input$btn_zoom_extent, {
    bb <- rx_fit_bbox()
    if (!is.null(bb)) maplibre_proxy("map") |> fit_bounds(bbox = bb, animate = TRUE)
  })

  # * sp_title ----
  # The selected taxon, rendered as ordinary selectable text with a copy button beside each
  # name (apps#10). Two buttons rather than one so the scientific and common names can be
  # taken independently; dragging across both still gets them together.
  output$sp_title <- renderUI({
    req(input$sel_sp)
    sp_row <- d_spp |> filter(mdl_key == input$sel_sp)
    req(nrow(sp_row) == 1)

    sci <- sp_row$scientific_name
    cmn <- sp_row$common_name
    has_cmn <- !is.na(cmn) && nzchar(cmn)

    copy_btn <- function(txt, what) tags$button(
      class = "sp-copy", type = "button",
      `data-copy` = txt, title = glue("copy {what} name"),
      `aria-label` = glue("copy {what} name"),
      icon("copy"))

    # no id here: uiOutput("sp_title") already puts that id on the wrapper this
    # renders into, and repeating it put TWO #sp_title in the document
    div(
      class = "sp-title",
      span(class = "sci", sci), copy_btn(sci, "scientific"),
      # a taxon with no common name gets the scientific name alone, not a dangling separator
      if (has_cmn) tagList(
        span(class = "sep", "·"),
        span(class = "cmn", cmn), copy_btn(cmn, "common")))
  })

  # * layer_bar ----
  output$layer_bar <- renderUI({
    req(input$sel_sp, input$ds_layer)

    sp_row        <- d_spp |> filter(mdl_key == input$sel_sp)
    current_layer <- input$ds_layer
    layer_name    <- layer_names[current_layer]
    n_inputs      <- n_inputs_of(sp_row$mdl_key)

    # determine available layers (mirrors observeEvent input$sel_sp logic)
    available <- c()
    if (!is.na(sp_row$mdl_key))
      available <- c(available, c("Merged Model" = "mdl_key"))
    for (dk in ds_keys) {
      if (dk %in% names(sp_row) && !is.na(sp_row[[dk]]))
        available <- c(available, setNames(dk, mdl_names[dk]))
    }

    is_merged <- (current_layer == "mdl_key")

    # build pill buttons for each available layer. An input that fed the merge but has no
    # published surface in this release is shown DISABLED rather than omitted — dropping it
    # made a registry gap look like a taxon with fewer inputs (see the d_edges note above).
    pills <- lapply(seq_along(available), function(i) {
      ds_val   <- available[[i]]
      ds_label <- names(available)[[i]]
      if (ds_val != "mdl_key" && !has_asset(sp_row[[ds_val]]))
        return(tags$span(
          class = "layer-pill unavailable",
          title = glue("{ds_label} feeds the merged model, but {ver} publishes no surface ",
                       "for it — nothing to draw"),
          ds_label))
      active   <- ifelse(ds_val == current_layer, " active", "")
      onclick_js <- sprintf(
        "var r=document.querySelector('#ds_layer_container input[value=\"%s\"]'); if(r) r.click();",
        ds_val)
      tags$a(
        class   = paste0("layer-pill", active),
        onclick = onclick_js,
        ds_label)
    })

    # build left content based on merged vs input state
    if (is_merged) {
      bar_class <- "layer-bar is-merged"
      left_content <- tagList(
        span(class = "layer-icon", "\u2713"),
        span(class = "layer-label", "Merged Model"),
        # counted from the normalised edges (n_inputs_of), NOT a stored n_ds column: v1-v7
        # count the ms_merge edge in n_ds and v8 does not, so any fixed offset is wrong on one
        # of them. This is the same quantity as the input pills beside it, by construction.
        if (n_inputs > 1) span(
          style = "opacity: 0.85;",
          glue(" (maximum of {n_inputs} inputs)")))
    } else {
      bar_class <- "layer-bar is-input"
      in_key  <- sp_row[[current_layer]]                 # this input's raw mdl_key
      reps    <- reps_for(in_key)
      cur_rep <- input$representation %||% "native"
      # Original (native source resolution) vs Interpolated (0.05\u00B0 scoring grid) \u2014 only when
      # the input publishes both (AquaMaps); vector ranges have just the native PMTiles.
      # For a dataset delivered ON the grid (dataset.on_grid, AquaX from v9) nothing is
      # interpolated: the pair is the band as DELIVERED vs the surface AS INGESTED (scaled to
      # 1-100, integer, pixels below the ingest threshold dropped -- what the merge consumes).
      on_grid  <- isTRUE(d_datasets$on_grid[match(current_layer, d_datasets$ds_key)])
      rep_lbl  <- if (on_grid) c(native = "Delivered", model = "As ingested") else c(native = "Original", model = "Interpolated")
      rep_ttl  <- if (on_grid) c(native = "the band exactly as delivered (already on the 0.05\u00B0 grid)",
                                 model  = "as ingested: rescaled to 1\u2013100 with the ingest threshold applied \u2014 what the merge uses")
                  else         c(native = "the source SDM at its native resolution",
                                 model  = "resampled to the 0.05\u00B0 scoring grid")
      rep_toggle <- if (length(reps) > 1) tagList(
        span(" \u2014 "),
        tags$a(
          class   = paste0("layer-pill rep-pill", if (cur_rep == "native") " active" else ""),
          title   = rep_ttl[["native"]],
          onclick = "var r=document.querySelector('#representation_container input[value=\"native\"]'); if(r) r.click();",
          rep_lbl[["native"]]),
        tags$a(
          class   = paste0("layer-pill rep-pill", if (cur_rep == "model") " active" else ""),
          title   = rep_ttl[["model"]],
          onclick = "var r=document.querySelector('#representation_container input[value=\"model\"]'); if(r) r.click();",
          rep_lbl[["model"]])) else NULL
      left_content <- tagList(
        span(class = "layer-icon", "\u25B6"),
        span(class = "layer-label",
          glue("Viewing input: {layer_name}")),
        rep_toggle,
        span(" \u2014 "),
        tags$a(
          class   = "merged-link",
          onclick = "var r=document.querySelector('#ds_layer_container input[value=\"mdl_key\"]'); if(r) r.click();",
          "show Merged Model"))
    }

    div(
      class = bar_class,
      left_content,
      # phones only (CSS): the pills stacked seven rows deep at 400px, so there they
      # sit behind this toggle and the bar is one line until tapped. Client-side
      # class flip, so no round trip; a re-render (layer change) collapses it again.
      tags$a(
        class   = "layer-pill layer-toggle",
        onclick = "this.closest('.layer-bar').classList.toggle('expanded');",
        glue("{length(pills)} layers")),
      div(class = "layer-links", pills))
  })

  # * species_info ----
  output$species_info <- renderUI({
    req(input$sel_sp, input$ds_layer)

    mdl_key <- input$sel_sp
    d_sp <- d_spp |>
      filter(mdl_key == !!mdl_key)

    # determine which models are present
    has_iucn <- "rng_iucn" %in% names(d_sp) && !is.na(d_sp$rng_iucn)
    n_ds     <- n_inputs_of(d_sp$mdl_key)

    # current layer being displayed
    current_layer <- input$ds_layer

    # helper to create model link (bold if currently displayed, in-page switch)
    make_link <- function(ds_key, type = "value") {
      if (!ds_key %in% names(d_sp)) return(NULL)
      ds_mdl_key <- d_sp[[ds_key]]
      if (is.na(ds_mdl_key)) return(NULL)
      # an input the merge used but this release publishes no surface for: still listed (it
      # contributed to the value shown), but not a link to a layer that cannot be drawn
      if (!has_asset(ds_mdl_key))
        return(HTML(glue("{mdl_names[ds_key]} <em class='text-muted'>(no published surface)</em>")))
      str_info <- ifelse(
        type == "value" && !is.na(mdl_info[ds_key]),
        glue("<br><em>({mdl_info[ds_key]})</em>"),
        "")
      is_active <- (ds_key == current_layer)
      link_text <- if (is_active) {
        glue("<b>{mdl_names[ds_key]}</b>")
      } else {
        mdl_names[ds_key]
      }
      onclick_js <- sprintf(
        "var r=document.querySelector('#ds_layer_container input[value=&quot;%s&quot;]'); if(r) r.click(); return false;",
        ds_key)
      HTML(glue('<a href="?mdl_key={ds_mdl_key}" onclick="{onclick_js}">{link_text}</a>{str_info}'))
    }

    # value models (all non-merge datasets present for this species)
    value_models <- ds_keys[
      sapply(ds_keys, function(k) k %in% names(d_sp) && !is.na(d_sp[[k]]))]

    # mask models (only relevant when has_iucn)
    mask_models <- ds_keys_mask[
      sapply(ds_keys_mask, function(k) k %in% names(d_sp) && !is.na(d_sp[[k]]))]

    # build values section
    if (length(value_models) == 1 && !has_iucn) {
      # single model, no merge needed
      values_ui <- tags$ul(tags$li(make_link(value_models[1])))
    } else {
      # merged model with sub-items
      merge_base  <- if (has_iucn) "Merged Model (IUCN masked)" else "Merged Model"
      merge_label <- if (current_layer == "mdl_key") glue("<b>{merge_base}</b>") else merge_base
      sub_items   <- lapply(value_models, function(k) tags$li(make_link(k)))
      values_ui   <- tags$ul(
        tags$li(
          HTML(glue('<a href="?mdl_key={d_sp$mdl_key}" onclick="var r=document.querySelector(\'#ds_layer_container input[value=&quot;mdl_key&quot;]\'); if(r) r.click(); return false;">{merge_label}</a><br><em>(maximum of):</em>')),
          tags$ul(sub_items)
        )
      )
    }

    # build mask section (only if has_iucn)
    mask_ui <- if (has_iucn) {
      mask_items <- lapply(mask_models, function(k) {
        suffix <- if (k == "rng_iucn") em(" (required)") else ""
        tags$li(make_link(k, type = "mask"), suffix)
      })
      tagList(
        span(strong("Mask"), br(), em("(to constrain extent)"),":"),
        tags$ul(mask_items)
      )
    } else NULL

    esa_str <- ifelse(
      is.na(d_sp$esa_code),
      "NA",
      glue("{d_sp$esa_code} ({d_sp$esa_source |> str_replace('ch_','') |> str_to_upper()})"))

    # protection flags (only show if TRUE)
    prot_items <- tagList(
      if (isTRUE(d_sp$is_mmpa)) tags$li("MMPA: Protected (20)"),
      if (isTRUE(d_sp$is_mbta)) tags$li("MBTA: Protected (10)")
    )

    tagList(
      h5(d_sp$scientific_name),
      tags$ul(
        tags$li(glue("Common name: {d_sp$common_name}")),
        tags$li(glue("Category: {d_sp$sp_cat}")),
        tags$li(glue("ESA Listing: {esa_str}")),
        tags$li(glue("IUCN RedList: {d_sp$redlist_code}")),
        tags$li(HTML(glue("WoRMS: {d_sp$worms_url}"))),
        prot_items
      ),
      span(strong("Values"),":"),
      values_ui,
      mask_ui
    )
  })

  # * sel_layer: the mdl_key for the selected species + layer (NULL if unavailable) ----
  # v8 renders via titiler-v8 XYZ tiles (not an in-R terra raster), so this returns just the
  # merged model's mdl_key; the SELECT is built + served in the render observer below.
  sel_layer <- reactive({
    req(input$sel_sp, input$ds_layer)
    sp_row        <- d_spp |> filter(mdl_key == input$sel_sp)
    layer_mdl_key <- sp_row[[input$ds_layer]]
    if (is.null(layer_mdl_key) || length(layer_mdl_key) == 0 || is.na(layer_mdl_key)) return(NULL)
    layer_mdl_key
  })

  # geographic bbox (lon/lat) of a model's cells, for fit_bounds
  #
  # ANTIMERIDIAN (apps#9): min(lon)/max(lon) describes a North Pacific range as the whole globe —
  # Least Auklet has cells at 160-180 E and 180-150 W, so the naive box is -179.975..179.975 and
  # fitBounds centres it at longitude 0, off Iceland. Both branches below therefore hand their
  # longitudes to msens::lon_span*(), which measures the span in the -180..180 AND 0..360 frames
  # and keeps the narrower. The xmax it returns may exceed 180; pass it through UNWRAPPED —
  # MapLibre reads [160, 48, 210, 66] as crossing the dateline (verified in-browser), while
  # wrapping 210 back to -150 would put west east of east and fit the complement.
  #
  # v1-v7 and v8 answer this from different tables, and the v1-v7 half used to answer it not at
  # all: the v8 SQL names `c.lon` and `model_cell`, and a v1-v7 release has neither (its `cell`
  # carries no lon/lat, and the table is `cell_model` keyed by mdl_seq). The tryCatch swallowed
  # that, so EVERY species on the default release fitted the same whole-study-area extent.
  mdl_bbox <- function(mdl_key) {
    # a read can still fail (model_cell is a view over Parquet the app container may not be able
    # to glob) — return NULL so callers fall back to the US study-area extent rather than crashing.
    b <- tryCatch(
      if (is_mdl_seq) {
        # cell_id -> row/col is pure arithmetic on the grid, so the extent needs no lon/lat
        # column: usa05's own frame runs 141.10 E EASTWARD across the dateline, which is already
        # the contiguous frame an Aleutian extent wants (cell_lonlat(wrap = FALSE)).
        rc <- dbGetQuery(con_sdm, glue(
          "SELECT min(((cell_id - 1) %  {grid$nc}) + 1) c0,
                  max(((cell_id - 1) %  {grid$nc}) + 1) c1,
                  min(((cell_id - 1) // {grid$nc}) + 1) r0,
                  max(((cell_id - 1) // {grid$nc}) + 1) r1
             FROM cell_model WHERE mdl_seq = {as.integer(mdl_key)}"))
        if (is.na(rc$c0)) NULL else data.frame(
          x0 = grid$xmin + (rc$c0 - 1) * grid$resx,   # cell EDGES, not centres
          x1 = grid$xmin +  rc$c1      * grid$resx,
          y0 = grid$ymax -  rc$r1      * grid$resy,
          y1 = grid$ymax - (rc$r0 - 1) * grid$resy)
      } else {
        dbGetQuery(con_sdm, glue(
          "SELECT min(c.lon) x0, min(c.lat) y0, max(c.lon) x1, max(c.lat) y1,
                  min(CASE WHEN c.lon < 0 THEN c.lon + 360 ELSE c.lon END) w0,
                  max(CASE WHEN c.lon < 0 THEN c.lon + 360 ELSE c.lon END) w1
             FROM model_cell mc JOIN cell c USING (cell_id)
            WHERE mc.mdl_id = (SELECT mdl_id FROM model WHERE mdl_key = '{mdl_key}')"))
      },
      error = function(e) NULL)
    if (is.null(b) || is.na(b$x0)) return(NULL)
    # on usa05 the frame is already contiguous, so the 0-360 pair IS the -180..180 pair and
    # lon_span_agg is a no-op; on global05 it is what moves the box off the prime meridian
    lon <- if (is.null(b$w0)) msens::lon_span_agg(b$x0, b$x1, b$x0, b$x1) else
                              msens::lon_span_agg(b$x0, b$x1, b$w0, b$w1)
    c(lon[1], b$y0, lon[2], b$y1)
  }

  # * get_name ----
  get_name <- reactive({
    d_spp |>
      filter(mdl_key == input$sel_sp) |>
      pull(scientific_name)
  })

  # * map ----
  output$map <- renderMaplibre({
    # input <- list(tgl_sphere = T)

    maplibre(
      style = carto_style("dark-matter"),
      projection = ifelse(input$tgl_sphere, "globe", "mercator")
    ) |>
      fit_bounds(er_bbox) |>
      msens::add_pmline(Filter(Negate(is.null), list(
        if (!is.null(pa <- ztile("programarea", tbl_pra_pm)))
          c(pa, list(id = "pra_ln", source_id = "pra_src",
                     line_color = "white", line_width = 1)),
        if (!is.null(er <- ztile("ecoregion", tbl_er)))
          c(er, list(id = "er_ln", source_id = "er_src",
                     line_color = "black", line_width = 3, before_id = "pra_ln"))))) |>
      add_fill_layer(
        id           = "pra_hover",
        source       = "pra_src",
        source_layer = pra_src_layer,
        fill_opacity = 0.01,
        fill_color   = "white",
        tooltip      = get_column("programarea_name"),
        before_id    = "pra_ln") |>
      msens::add_pmlabel(list(
        list(source     = pra_pts,
             text_field = "programarea_key",
             id         = "pra_lbl"))) |>
      add_fullscreen_control() |>
      add_navigation_control() |>
      add_scale_control() |>
      add_geocoder_control() |>
      add_globe_minimap(position = "bottom-left") |>
      add_layers_control(
        layers = list(
          "Program Area outlines" = "pra_ln",
          "Program Area labels"   = "pra_lbl",
          "Ecoregions outlines"   = "er_ln"))
  })

  # * parse_er_clr helper ----
  parse_er_clr <- function(er_clr_str) {
    # Parse "CAC:red;EGOA:green;NECS:blue" into lists of values and colors
    if (is.null(er_clr_str) || er_clr_str == "") {
      return(NULL)
    }

    pairs <- strsplit(er_clr_str, ";")[[1]]
    pairs <- trimws(pairs)
    pairs <- pairs[pairs != ""]

    values <- character(0)
    colors <- character(0)

    for (pair in pairs) {
      parts <- strsplit(pair, ":")[[1]]
      if (length(parts) == 2) {
        values <- c(values, trimws(parts[1]))
        colors <- c(colors, trimws(parts[2]))
      }
    }

    if (length(values) == 0) {
      return(NULL)
    }
    list(values = values, colors = colors)
  }

  # * add_er_fill_layer helper ----
  add_er_fill_layer <- function(map_proxy, er_clr_str) {
    parsed <- parse_er_clr(er_clr_str)
    if (is.null(parsed)) {
      return(map_proxy)
    }

    map_proxy |>
      clear_layer("er_ply") |>
      add_fill_layer(
        id = "er_ply",
        source = "er_src",
        source_layer = er_src_layer,
        fill_color = match_expr(
          column = "ecoregion_key",
          values = parsed$values,
          stops = parsed$colors,
          default = "#cccccc"
        ),
        fill_opacity = 0.5,
        before_id = "pra_ln"
      )
  }

  # * input$sel_sp -> update layer choices ----
  observeEvent(input$sel_sp, {
    req(input$sel_sp)

    sp_row <- d_spp |> filter(mdl_key == input$sel_sp)

    # determine which layers are available
    available <- c()
    if (!is.na(sp_row$mdl_key)) available <- c(available, "Merged Model" = "mdl_key")
    for (dk in ds_keys) {
      if (dk %in% names(sp_row) && !is.na(sp_row[[dk]])) {
        available <- c(available, setNames(dk, d_datasets$name_display[d_datasets$ds_key == dk]))
      }
    }

    # check if URL specified a layer to select. keep rx_ds_layer set (cleared later, in the
    # render observer) so it can skip the stale default-layer render — avoids the
    # merged->input "jigger" when deep-linking straight to a non-merged input.
    url_layer <- rx_ds_layer()
    selected_layer <- if (!is.null(url_layer) && url_layer %in% available) url_layer else "mdl_key"

    updateRadioButtons(session, "ds_layer", choices = available, selected = selected_layer, inline = TRUE)
  })

  # * input$sel_sp or ds_layer -> update map ----
  observeEvent(list(input$sel_sp, input$ds_layer, input$sel_mask, input$representation), {
    req(input$sel_sp, input$ds_layer)

    # deep-link jigger fix: while a URL-specified layer is pending (rx_ds_layer), skip any
    # render whose ds_layer hasn't caught up to it (don't flash the merged model first);
    # once ds_layer matches the target, clear the pending flag and render it directly.
    pend <- rx_ds_layer()
    if (!is.null(pend)) {
      if (!identical(input$ds_layer, pend)) return()
      rx_ds_layer(NULL)
    }

    if (verbose) {
      message("observeEvent(input$sel_sp/ds_layer): ", input$sel_sp, " / ", input$ds_layer)
    }

    map_proxy <- maplibre_proxy("map")

    # clear existing marker when species/layer changes
    map_proxy |> clear_markers()

    layer_mdl_key <- sel_layer()

    # handle case when the layer isn't available for this species
    if (is.null(layer_mdl_key)) {
      map_proxy |> clear_layer("r_lyr") |> clear_layer("r_src") |> clear_legend()
      showNotification("No data to display \u2014 this species lacks this layer", type = "warning")
      return()
    }

    n_cols <- 11
    cols_r <- rev(RColorBrewer::brewer.pal(n_cols, "Spectral"))
    # rng_r  <- minmax(r) |> as.numeric() |> signif(digits = 3)
    rng_r  <- c(1,100)

    # get species info for legend and browser title
    sp_row        <- d_spp |> filter(mdl_key == input$sel_sp)
    sp_name       <- sp_row$scientific_name
    layer_name    <- layer_names[input$ds_layer]
    layer_mdl_key <- sp_row[[input$ds_layer]]
    title_str     <- glue("{sp_name} - {layer_name}")

    # update browser title and URL
    sp_cat_cmn <- ifelse(
      is.na(sp_row$common_name) || sp_row$common_name == "",
      glue("{sp_row$sp_cat}"),
      glue("{sp_row$sp_cat}: {sp_row$common_name}"))
    browser_title <- glue(
      "{sp_name} distribution ({sp_cat_cmn}; mdl_key: {layer_mdl_key}) from {layer_name} | BOEM Marine Sensitivity")
    session$sendCustomMessage("updateTitle", browser_title)
    # on the preview host the version is the PATH (/v9/species/), so repeating it
    # as ?ver= would only make every shared deep link say it twice
    updateQueryString(
      if (msens::atlas_is_preview()) glue("?mdl_key={layer_mdl_key}")
      else glue("?ver={ver}&mdl_key={layer_mdl_key}"),
      mode = "replace", session = session)

    # v8 Phase 4b: the merged model + AquaMaps inputs are raster surfaces (titiler XYZ tiles on
    # "r_lyr"); vector-range inputs are PMTiles polygons (client-filtered by mdl_key on "r_pm").
    # Merged serves the US surface via cell-SQL; native COG/PMTiles inputs show the whole global
    # range. Only one is shown at a time, so clear both first.
    is_merged <- input$ds_layer == "mdl_key"
    # pick the selected representation (Original=native / Interpolated=model); falls back to
    # native when the input has only one (vector ranges). The merged surface is always the
    # interpolated view, so no rep applies there.
    asset     <- if (!is_merged) pick_asset(layer_mdl_key, input$representation %||% "native") else NULL

    # fit target: merged -> the model's own data extent; input -> its bbox from native_asset,
    # falling back to the taxon's merged extent, and to the study area only if both are unusable.
    #
    # An asset bbox is taken only if it FRAMES something (apps#9). native_asset records the
    # asset's extent, and for a range that crosses the antimeridian that extent is honestly
    # -180..180 — the COG really is a global raster with data at both edges. True of the asset,
    # useless as a camera target: obeying it put every Bering Sea species off Iceland. A
    # whole-world box is therefore treated exactly like a missing one, and the fallback is the
    # merged model's DATA extent, which mdl_bbox() now measures the short way round.
    asset_bbox <- if (!is.null(asset) && !is.na(asset$xmin))
      c(asset$xmin, asset$ymin, asset$xmax, asset$ymax) else NULL
    fit_bbox <- if (is_merged) {
      bb <- mdl_bbox(layer_mdl_key); if (is.null(bb)) er_bbox else bb
    } else if (!msens::bbox_spans_globe(asset_bbox)) {
      asset_bbox
    } else { bb <- mdl_bbox(sp_row$mdl_key); if (is.null(bb)) er_bbox else bb }
    rx_fit_bbox(fit_bbox)   # remember this layer's extent for the "Zoom to layer extent" button

    # clear BOTH the previous raster (r_lyr/r_src) and pmtiles (r_pm/pm_src) layer+source;
    # clear_layer removes a source of that id too, so re-adding a source doesn't collide
    # client-side (the bug that left the stale merged surface when switching to an input).
    map_proxy <- map_proxy |>
      clear_layer("r_lyr") |> clear_layer("r_src") |>
      clear_layer("r_pm")  |> clear_layer("pm_src") |> clear_legend()

    # merged layer: prefer the whole-range merged COG (anonymous /cog, and the ONLY surface for a
    # non-US species that has no model_cell); am-only taxa lack a merged COG -> model_cell fallback.
    merged_cog_url <- NA_character_
    if (is_merged) {
      # the merged model's OWN asset row, selected by key rather than by ds_key == "ms_merge".
      # On v8 those are the same row; on v1-v7 a taxon whose only model is AquaMaps has its
      # merged surface registered under ds_key "am", so keying on the label found nothing and
      # dropped through to the model_cell branch below -- which would have asked titiler for a
      # v8 partition while claiming to draw v1.
      mc <- rows_for(layer_mdl_key)
      if (nrow(mc)) merged_cog_url <- mc$asset_url[1]
    }
    if (is_merged || (!is.null(asset) && asset$asset_type == "cog")) {
      tile_url <- if (is_merged && !is.na(merged_cog_url)) {
        msens::cog_tile_url(merged_cog_url, colormap = "spectral_r", rescale = c(1, 100), base = tile_base_url)
      } else if (is_merged && has_model_cell) {
        # model_cell fallback (am-only taxa): titiler resolves the stable mdl_key -> serve
        # partition. ONLY for a release that publishes model_cell (v8) -- the tile endpoint is
        # bound to one release's serving DB, so using it elsewhere draws the wrong data.
        msens::cell_tile_url(mdl_key = layer_mdl_key, colormap = "spectral_r", rescale = c(1, 100),
                             mtime = db_mtime, base = tile_base_url)
      } else if (is_merged) {
        NULL                                    # nothing published for this taxon in this release
      } else {
        msens::cog_tile_url(asset$asset_url,
                            colormap = coalesce(asset$colormap, "spectral_r"),
                            rescale  = c(coalesce(asset$rescale_min, 1L), coalesce(asset$rescale_max, 100L)),
                            base = tile_base_url)
      }
      if (is.null(tile_url)) {
        showNotification(glue("No surface published for this taxon in {ver}"), type = "warning")
        active_lyr <- character(0)
      } else {
        map_proxy  <- map_proxy |>
          msens::add_cell_tiles(tile_url, id = "r_lyr", raster_opacity = 0.8, before_id = "er_ln") |>
          add_legend(title_str, values = rng_r, colors = cols_r, position = "bottom-right")
        active_lyr <- c("Raster cell values" = "r_lyr")
      }
    } else if (!is.null(asset) && asset$asset_type == "pmtiles") {
      pm_col     <- "#3388ff"
      map_proxy  <- map_proxy |>
        add_pmtiles_source(id = "pm_src", url = asset$asset_url) |>
        add_fill_layer(
          id = "r_pm", source = "pm_src", source_layer = asset$source_layer,
          filter = list("==", list("get", "mdl_key"), layer_mdl_key),
          fill_color = pm_col, fill_opacity = 0.5, before_id = "er_ln") |>
        add_categorical_legend(
          legend_title = title_str, values = "range (presence)", colors = pm_col,
          position = "bottom-right")
      active_lyr <- c("Range (presence)" = "r_pm")
    } else {
      showNotification("No native surface available for this input", type = "warning")
      active_lyr <- character(0)
    }

    # record what's shown so a map click can sample its value (merged -> cell-SQL by cell_id;
    # cog -> titiler /cog/point; pmtiles -> presence only)
    rx_shown(list(
      mdl_key = layer_mdl_key, name = sp_name,
      type    = if (is_merged && !is.na(merged_cog_url)) "cog"
                else if (is_merged) "merged" else (asset$asset_type %||% "pmtiles"),
      url     = if (is_merged && !is.na(merged_cog_url)) merged_cog_url
                else if (!is_merged && identical(asset$asset_type, "cog")) asset$asset_url else NA_character_))

    # outline overlay: show Program Areas, Ecoregions, or None (the "Outlines:" selector)
    pa_vis <- if (input$sel_mask == "programarea_key") "visible" else "none"
    er_vis <- if (input$sel_mask == "ecoregion_key")   "visible" else "none"
    # recenter ONLY when the species changed; switching layers/representations for the same
    # species swaps the source in place (map proxy) without bouncing the user's view.
    if (!identical(input$sel_sp, rx_fitted_sp())) {
      map_proxy |> fit_bounds(bbox = fit_bbox, animate = TRUE)
      rx_fitted_sp(input$sel_sp)
    }
    map_proxy |>
      set_layout_property("pra_ln",  "visibility", pa_vis) |>
      set_layout_property("pra_lbl", "visibility", pa_vis) |>
      set_layout_property("er_ln",   "visibility", er_vis) |>
      clear_controls("layers") |>
      add_layers_control(layers = c(list(
        "Program Area outlines" = "pra_ln",
        "Program Area labels"   = "pra_lbl",
        "Ecoregion outlines"    = "er_ln"), as.list(active_lyr)))

    # add ecoregion fill layer if er_clr parameter was provided
    er_clr <- rx_er_clr()
    if (!is.null(er_clr)) {
      map_proxy |>
        add_er_fill_layer(er_clr)
    }
  })

  # * OBIS occurrences overlay (h3t) ----
  # per-species OBIS occurrence hexagons (record counts) from the h3t service, reusing the apps/h3-db
  # layer (works now the map is MapLibre). Fires on the toggle and species change: tear the layer down
  # first, then re-add it when on and the taxon carries a WoRMS AphiaID (the h3t store filters by it).
  observeEvent(list(input$tgl_obis, input$sel_sp), ignoreInit = TRUE, {
    if (!has_obis) return()
    map_proxy <- maplibre_proxy("map")
    map_proxy |> clear_layer(c("obis_occ_fill", "obis_occ"))   # layer first, then its source
    if (!isTRUE(input$tgl_obis)) return()
    req(input$sel_sp)

    sp_row  <- d_spp |> filter(mdl_key == input$sel_sp)
    if (nrow(sp_row) != 1) return()
    aphiaid <- if (identical(sp_row$taxon_authority, "worms") && !is.na(sp_row$taxon_id))
                 suppressWarnings(as.integer(sp_row$taxon_id)) else NA_integer_
    if (is.na(aphiaid)) {
      showNotification(
        "OBIS occurrences need a WoRMS AphiaID — not available for this taxon.", type = "warning")
      return()
    }

    # indicator "n" = number of OBIS occurrence records per hexagon, filtered to this species' subtree
    sql   <- obis_h3t_sql(indicator = "n", aphiaid = aphiaid, res_max = 7L)
    stats <- h3t_stats(sql)
    if (!is.null(stats$error) || (stats$n %||% 0) < 1) {
      showNotification(glue("No OBIS occurrences found for {sp_row$scientific_name}."), type = "message")
      return()
    }

    # color ramp over the robust p02–p98 record-count range (fallback to min–max)
    lo <- stats$p02 %||% stats$min %||% 0
    hi <- stats$p98 %||% stats$max %||% 1
    if (!is.finite(lo) || !is.finite(hi) || lo >= hi) { lo <- 0; hi <- max(hi, 1, na.rm = TRUE) }
    brks  <- seq(lo, hi, length.out = length(H3T_VIRIDIS5))
    tiles <- obis_h3t_url(base_url = H3T_TILES_BASE, sql = sql, release = H3T_RELEASE)

    map_proxy |>
      add_h3t_source(id = "obis_occ", tiles = tiles) |>
      add_fill_layer(
        id = "obis_occ_fill", source = "obis_occ", source_layer = "obis_occ",
        fill_color   = interpolate(column = "value", values = brks, stops = H3T_VIRIDIS5),
        fill_opacity = 0.6,
        tooltip      = get_column("value")) |>
      add_legend(
        glue("OBIS occurrences: {sp_row$scientific_name}"),
        values = round(c(lo, hi), 0), colors = H3T_VIRIDIS5, position = "bottom-left")
  })

  # map_click ----
  observeEvent(input$map_click, {
    click <- input$map_click

    # skip if marker was clicked (allow popup to show)
    if (rx_marker_clicked()) {
      rx_marker_clicked(FALSE)
      return()
    }

    shown <- rx_shown()
    if (is.null(shown)) return()
    lng <- click$lng; lat <- click$lat

    # global 0.05° cell_id (row-major, top-left origin) — matches the grid the surfaces use
    col <- floor((lng + 180) / 0.05); row <- floor((90 - lat) / 0.05)
    if (col < 0 || col >= 7200 || row < 0 || row >= 3600) return()
    cell_id <- as.integer(row) * 7200L + as.integer(col) + 1L

    # sample the value of the shown layer at this cell
    val <- if (shown$type == "merged") {
      v <- tryCatch(dbGetQuery(con_sdm, glue(
        "SELECT val FROM model_cell
         WHERE mdl_id = (SELECT mdl_id FROM model WHERE mdl_key = '{shown$mdl_key}') AND cell_id = {cell_id}")),
        error = function(e) NULL)
      if (!is.null(v) && nrow(v)) as.numeric(v$val[1]) else NA_real_
    } else if (identical(shown$type, "cog") && !is.na(shown$url)) {
      pv <- tryCatch(
        httr2::request(glue("{tile_base_url}/cog/point/{lng},{lat}")) |>
          httr2::req_url_query(url = shown$url) |> httr2::req_perform() |>
          httr2::resp_body_json(),
        error = function(e) NULL)
      if (!is.null(pv) && length(pv$values)) as.numeric(pv$values[[1]]) else NA_real_
    } else NA_real_   # pmtiles = presence; no per-cell value

    if (verbose) message("map_click: cell_id=", cell_id, ", val=", val)

    if (is.na(val)) {
      maplibre_proxy("map") |> clear_markers() |>
        add_markers(data = c(lng, lat), marker_id = "click_marker")
      session$sendCustomMessage("clickPopup", list(map = "map", lng = lng, lat = lat,
        html = glue('<div style="padding:6px;color:black;">Cell {cell_id}<br>',
                    'Lon {round(lng,3)}, Lat {round(lat,3)}<br><i>no value here</i></div>')))
      return()
    }

    # calculate background color based on value
    n_cols <- 11
    cols_r <- rev(RColorBrewer::brewer.pal(n_cols, "Spectral"))
    # rng_r  <- minmax(r) |> as.numeric()
    rng_r  <- c(1,100)

    # scale value to color index
    if (rng_r[2] > rng_r[1]) {
      val_scaled <- (val - rng_r[1]) / (rng_r[2] - rng_r[1])
    } else {
      val_scaled <- 0.5
    }
    val_scaled <- max(0, min(1, val_scaled))  # clamp to [0, 1]
    col_idx    <- round(val_scaled * (n_cols - 1)) + 1
    bg_color   <- cols_r[col_idx]

    # calculate text color for contrast (luminance-based)
    rgb_vals  <- col2rgb(bg_color)
    luminance <- (0.299 * rgb_vals[1] + 0.587 * rgb_vals[2] + 0.114 * rgb_vals[3]) / 255
    txt_color <- ifelse(luminance > 0.5, "black", "white")

    sp_name <- get_name()

    popup_html <- glue(
      '<div style="background-color: {bg_color}; color: {txt_color}; ',
      'padding: 8px; border-radius: 4px;">',
      '<b>{sp_name}</b><br>',
      'Cell ID: {cell_id}<br>',
      'Lon: {round(click$lng, 3)}<br>',
      'Lat: {round(click$lat, 3)}<br>',
      'Value: {round(val, 3)}',
      '</div>'
    )

    # drop the pin AND open the popup immediately (single click) — mapgl's marker popup
    # otherwise needs a second click on the marker to open.
    maplibre_proxy("map") |>
      clear_markers() |>
      add_markers(data = c(click$lng, click$lat), marker_id = "click_marker", color = bg_color)
    session$sendCustomMessage("clickPopup",
      list(map = "map", lng = click$lng, lat = click$lat, html = popup_html))
  })

  # marker_click: set flag so map_click doesn't recreate marker ----

  observeEvent(input$map_marker_click_marker, {
    # browser()
    rx_marker_clicked(TRUE)
  })
}


# `ver` is a URL parameter, not a fork of this app. Both the UI and the server are evaluated
# with their enclosing environment set to the requested version's bundle.

ui <- function(req) {
  b <- bundle(ver_of_req(req))
  f <- ui_impl; environment(f) <- b
  f(req)
}

server <- function(input, output, session) {
  b <- bundle(ver_of_session(input, session))
  f <- server_impl; environment(f) <- b
  f(input, output, session)
}

shinyApp(ui, server)

