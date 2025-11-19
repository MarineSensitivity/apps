function(input, output, session) {

  # map ----
  output$map <- renderMapboxgl({
    mapboxgl(
      style      = mapbox_style("dark"),
      scrollZoom = F) |>
      add_vector_source(
        id  = "er_src",
        url = "https://api.marinesensitivity.org/tilejson?table=public.ply_ecoregions_2025") |>
      add_vector_source(
        id  = "pa_src",
        url = "https://api.marinesensitivity.org/tilejson?table=public.ply_planareas_2025") |>
      add_image_source(
        id     = "r_src",
        data   = r0,
        colors = cols_r) |>
      add_navigation_control() |>
      add_scale_control() # |>
    # add_fullscreen_control() |>
    # add_layers_control() |>
      # add_geocoder_control(placeholder = "Go to location")
  })

  # on: scores_cell ----
  on_section("map", "scores_cell", {

    map_proxy <- mapboxgl_proxy("map")

    # remove layers if exist
    map_proxy |>
      clear_layer("pa_lyr") |>
      clear_layer("r_lyr") |>
      clear_legend()

    map_proxy |>
      fit_bounds(bbox0, animate = T) |>
      add_raster_layer(
        id                = "r_lyr",
        source            = "r_src",
        raster_opacity    = 1,
        raster_resampling = "nearest") |>
      mapgl::add_legend(
        "score",
        values   = rng_r0,
        colors   = cols_r,
        position = "bottom-right")
  })

  # on: scores_pa ----
  on_section("map", "scores_pa", {

    lyr <- "score_extriskspcat_primprod_ecoregionrescaled_equalweights"

    # get range of planning area values
    rng_pa <- tbl(con_sdm, "zone") |>
      filter(
        fld    == "planarea_key") |>
      select(planarea_key = value, zone_seq) |>
      inner_join(tbl(con_sdm, "zone_metric"), by = join_by(zone_seq)) |>
      inner_join(
        tbl(con_sdm, "metric") |>
          filter(metric_key == !!lyr), by = join_by(metric_seq)) |>
      pull(value) |>
      range()

    # colors
    cols_pa  <- colorRampPalette(cols_r, space = "Lab")(n_cols)
    brks_pa <- seq(rng_pa[1], rng_pa[2], length.out = n_cols)

    map_proxy <- mapboxgl_proxy("map")

    # remove raster layer if exists
    map_proxy |>
      clear_layer("r_lyr") |>
      clear_layer("pa_lyr") |>
      clear_legend()

    # add planning area fill layer
    map_proxy |>
      add_fill_layer(
        id                 = "pa_lyr",
        source             = "pa_src",
        source_layer       = "public.ply_planareas_2025",
        fill_color         = mapgl::interpolate(
          column       = lyr,
          values       = brks_pa,
          stops        = cols_pa,
          na_color     = "lightgrey"),
        fill_opacity       = 1,
        fill_outline_color = "white",
        tooltip            = concat("Value: ", get_column(lyr)),
        hover_options = list(
          fill_color   = "purple",
          fill_opacity = 1 )) |>
      mapgl::add_legend(
        "score",
        values   = round(rng_pa, 1),
        colors   = cols_pa,
        position = "bottom-right") |>
      fit_bounds(bbox0, animate = T)
  })

}
