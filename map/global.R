# libraries ----
librarian::shelf(
  dplyr, DT, geojsonio, glue, here,
  #  leaflet,
  # micahwilhelm/leaflet.extras, # addDrawToolbar()
  MarineSensitivity/msens,
  yogevherz/plotme,  # count_to_treemap()
  plotly,
  rdeck,
  shiny, shinydashboard, sf, tibble)

# TODO:
#  - [ ] copy duckdb to server
#  - [ ] migrate functions.R to aquamapsduckdb R package
#  - [ ] swap out shinydashboard for bslib
#        https://rstudio.github.io/bslib/articles/dashboards/

source(here("map/functions.R"))

# con_am start/stop ----
message("connecting to AquaMaps database")
con_am <- dbConnect(
  duckdb(
    dbdir     = path_am,
    read_only = T))

onStop(function() {
  message("shutting down AquaMaps database")
  dbDisconnect(con_am, shutdown = TRUE)
})

nspp_tif      <- here("map/data/am_nspp.tif")
nspp_3857_tif <- here("map/data/am_nspp_3857.tif")

if (!file.exists(nspp_3857_tif)){

  r <- am_rast_nspp()

  writeRaster(
    r,
    nspp_tif,
    overwrite = T,
    datatype  = "INT2U",
    gdal      = c(
      "TILED=YES",
      "COMPRESS=DEFLATE"))

  # trim since leaflet can't display at the poles
  e <- ext(r)
  e$ymin <- max(e$ymin, -89)  # -85
  e$ymax <- min(e$ymax,  89)  #  85
  r <- crop(r, e)
  r_nspp_3857 <- project(r, "epsg:3857")

  writeRaster(
    r_nspp_3857,
    nspp_3857_tif,
    overwrite = T,
    datatype  = "INT2U",
    gdal      = c(
      "TILED=YES",
      "COMPRESS=DEFLATE"))
}
r_nspp_3857 <- rast(nspp_3857_tif)

ply_rgns_s05 <- ply_rgns_s05 |>
  sf::st_wrap_dateline()
  #arrange(rgn_key)

lst_rgns <- ply_rgns_s05 |>
  st_drop_geometry() |>
  arrange(shlf_name, rgn_name) |>
  group_by(shlf_name) |>
  summarize(
    rgns = list(setNames(rgn_key, rgn_name))) |>
  deframe()
lst_rgns <- c(list(Overview = setNames(NA, "Overview")),
  lst_rgns)

# global defaults ----
ply_g <- ext(-180, 180, -90, 90) |>
  st_bbox() |>
  st_as_sfc() |>
  st_as_sf(crs = 4326)

d_spp_g <- am_spp_in_ply()
