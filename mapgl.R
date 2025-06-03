librarian::shelf(
  glue, httr2, mapgl, purrr, stringr)

sp_key <- "Fis-114088"
# src    <- "derived/aquamaps_0.05dd"
src    <- "raw/aquamaps"
cog_url <- glue("https://file.marinesensitivity.org/cog/sdm/{src}/{sp_key}.tif")

titiler_endpoint = "https://titilecache.marinesensitivity.org"
# titiler_endpoint = "https://tilecache.marinesensitivity.org"
# cog_method     = "bilinear"
cog_method     = "nearest"
cog_palette    = "spectral_r"
lgnd_palette   = "Spectral"
lgnd_palette_r = T
bidx           = 1

cog_bb <- request(titiler_endpoint) |>
  req_url_path_append("cog", "bounds") |>
  req_url_query(
    url = cog_url,
    bidx = bidx) |>
  req_perform() |>
  resp_body_json() |>
  unlist() |>
  as.numeric()
cog_bb

# * get range of values via titiler ----
cog_rng <- request(titiler_endpoint) |>
  req_url_path_append("cog", "statistics") |>
  req_url_query(
    url  = cog_url,
    bidx = bidx) |>
  req_perform() |>
  resp_body_json() |>
  pluck(glue("b{bidx}")) |>
  keep_at(~ .x %in% c("min", "max")) |>
  as.numeric()
cog_rng # 0 254
cog_rng = c(1, 100)

# set cog_rng manually
# cog_rng <- range(values(r, na.rm=T))

tile_opts <- glue(
  "bidx={bidx}&expression=b{bidx}&
  unscale=false&
  resampling={cog_method}&reproject={cog_method}&return_mask=true&
  rescale={paste(cog_rng, collapse=',')}&
  colormap_name={cog_palette}") |> str_replace_all("\\s+", "")
sdm_tile_url  <- glue(
  "{titiler_endpoint}/cog/tiles/WebMercatorQuad/{{z}}/{{x}}/{{y}}@2x.png?url={cog_url}&{tile_opts}")

# https://tile.marinesensitivity.org/public.ply_planareas.json
# pa_url <- "https://tile.marinesensitivity.org/public.ply_planareas/{z}/{x}/{y}.pbf"
# pa_url <- "https://tile.marinesensitivity.org/public.ply_planareas.json"
# pa_url <- "https://tilecache.marinesensitivity.org/public.sdm_geometries.json"

# Sys.setenv(MAPTILER_API_KEY = "5raPGHaCnQ8fXTv0whrb")
# usethis::edit_r_environ(scope = c("user"))

# maplibre(
#   style = maptiler_style("ocean"),
#   center = c(-43.23412, -22.91370),
#   zoom = 14) |>
#   add_fullscreen_control(position = "top-left") |>
#   add_navigation_control()

mapboxgl(
  style = mapbox_style("dark"),
  # maplibre(
  #   style = maptiler_style("toner"), # ocean
  zoom = 5,
  # center = c(-119.6982, 34.4208)) |>
  center = c(-89.11, 24.45)) |>
  add_raster_source(
    "rast-src",
    tiles = sdm_tile_url) |>
  add_raster_layer(
    id        = 'rast-lyr',
    source    = 'rast-src',
    # before_id = "admin-1-boundary-bg",
    # before_id = "landuse",
    raster_opacity = 0.6) |>
  move_layer(
    layer_id  = "rast-lyr",
    before_id = "landuse")

  # add_vector_source(
  #   id  = "vect_src",
  #   # url = 'https://file.marinesensitivity.org/_public.ply_planareas_2025.json') |>
  #   url = 'https://api.marinesensitivity.org/tilejson?table=public.ply_planareas_2025') |>
  # add_fill_layer(
  #   id           = "vect_lyr",
  #   source       = "vect_src",
  #   source_layer = "public.ply_planareas_2025",
  #   # fill_outline_color = "gray")
  #   # fill_color = '#0080ff',
  #   # fill_color   = 'transparent',
  #   fill_color   = mapgl::interpolate(
  #     column   = "area_km2",
  #     values   = c(21575.9, 1066334.4),
  #     stops    = c("lightblue", "darkblue"),
  #     na_color = "lightgrey"),
  #   popup        = "area_km2",
  #   tooltip      = "planarea_name",
  #   # fill_outline_color = 'white',
  #   fill_opacity = 0.9) |>
  mapgl::add_legend(
    "Suitability",
    values = c(1, 100),
    colors = rev(RColorBrewer::brewer.pal(11, "Spectral")))
