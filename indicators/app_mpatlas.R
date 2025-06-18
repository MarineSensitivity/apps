# libraries ----
librarian::shelf(
  bslib, DBI, dplyr, duckdb, glue, here, purrr, sf, shiny, stringr,
  terra, tibble, tidyr, RPostgres)

# TODO:
# - [ ] page_navbar(); https://rstudio.github.io/bslib/reference/navset.html#page-navbar-
# - [ ] maplibre() with pmtiles from map.navigatormap.org; check at pmtiles.io
# - [ ] improve tooltip and legend; https://walker-data.com/mapgl/reference/concat.html
# - [ ] mapboxgl() with wdpar; https://prioritizr.github.io/wdpar/articles/wdpar.html

# variables, paths ----
title          <- "Indicators"
verbose        <- T
is_server      <-  Sys.info()[["sysname"]] == "Linux"
dir_private    <- ifelse(is_server, "/share/private", "~/My Drive/private")
db_pass_txt    <- glue("{dir_private}/msens-db_admin-pass.txt")
mapbox_tkn_txt <- glue("{dir_private}/mapbox_token_bdbest.txt")

# load mapgl with mapbox token ----
Sys.setenv(MAPBOX_PUBLIC_TOKEN=readLines(mapbox_tkn_txt))
# devtools::install_github("bbest/mapgl")
librarian::shelf(
  mapgl)

# database connection ----
stopifnot(file.exists(db_pass_txt))
con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname   = "msens",
  host     = ifelse(is_server, "postgis", "localhost"),
  port     = 5432,
  user     = "admin",
  password = readLines(db_pass_txt),
  options  ="-c search_path=oceanmetrics,public")

# database connection ----
d_lyrs <- tbl(con, "ds_indicators_lyrs") |>
  collect()

lyr_choices <- d_lyrs |>
  arrange(group, description) |>
  group_by(group) |>
  summarise(
    layer = list(setNames(key, description)),
    .groups = "drop") |>
  deframe()

light <- bs_theme()
dark  <- bs_theme()
ui <- page_sidebar(
  tags$head(tags$style(HTML(
    ".mapboxgl-popup-content{color:black;}" ))),
  title   = title,
  sidebar = sidebar(
    selectInput(
      "sel_lyr",
      "Layer",
      choices = lyr_choices),
    # input_switch(
    #   "tgl_sphere", "Sphere", T ),
    input_dark_mode(
      id = "tgl_dark", mode = "dark")),
  card(
    full_screen = TRUE,
    mapboxglOutput("map") ) )
    # maplibreOutput("map") ) )

server <- function(input, output, session) {

  # TODO: update proxymap if lyr changes
  # lyr <- input$sel_lyr  # lyr_val = selected lyr value

  output$map <- renderMapboxgl({
  # output$map <- renderMaplibre({

    # req(input$sel_lyr, input$tgl_sphere)
    req(input$sel_lyr)

    # input <- list(tgl_sphere = F, sel_lyr = "al_er_su_wr")
    d_lyr <- d_lyrs |>
      filter(key == input$sel_lyr)

    n_cols <- 11
    cols   <- rev(RColorBrewer::brewer.pal(n_cols, "Spectral"))
    rng    <- d_lyr |> select(value_min, value_max) |> as.numeric()
    brks   <- seq(rng[1], rng[2], length.out = n_cols)

    # maplibre(
    #   style      = carto_style("voyager"),
    mapboxgl(
      style      = mapbox_style("dark"),
      # projection = ifelse(input$tgl_sphere, "globe", "mercator")) |>
      projection = "globe") |>
      fit_bounds(c(-97.86628, 11.76369, -43.88875, 56.52494)) |>
      add_vector_source(
        id         = "vect_src",
        url        = "https://api.marinesensitivity.org/tilejson?table=oceanmetrics.ds_indicators",
        promoteId  = "id") |>
      add_vector_source(
        id         = "mpa_src",
        url        = "https://mpatlas.org/services/mpas/tiles/{z}/{x}/{y}.pbf",
        promoteId  = "id") |>
      add_fill_layer(
        id           = "vect_ply",
        source       = "vect_src",
        source_layer = "oceanmetrics.ds_indicators",
        fill_color = interpolate(
          column = input$sel_lyr,
          values = brks,
          stops  = cols ),
        fill_opacity = 0.7,
        tooltip = input$sel_lyr,
        popup = concat(
          "layer key: ", input$sel_lyr    , "<br>
           feature id: ", get_column("id"), "<br>
           value: ", get_column(input$sel_lyr)),
        hover_options = list(
          fill_color   = "cyan",
          fill_opacity = 1 )) |>
      add_fill_layer(
        id           = "mpa_ply",
        source       = "mpa_src",
        source_layer = "mpa_poly",
        fill_color = "purple",
        fill_opacity = 0.7,
        tooltip = "id",
        # popup = concat(
        #   "layer key: ", input$sel_lyr    , "<br>
        #    feature id: ", get_column("id"), "<br>
        #    value: ", get_column(input$sel_lyr)),
        hover_options = list(
          fill_color   = "cyan",
          fill_opacity = 1 )) |>
      mapgl::add_legend(
        d_lyr$description,
        values   = rng |> signif(digits = 2),
        colors   = cols,
        position = "bottom-right") |>
      add_fullscreen_control(
        position = "top-left") |>
      add_navigation_control() |>
      add_scale_control()

  })

  observeEvent(input$map_click, {
    # mapboxgl_proxy("map")
    if (verbose){
      message(": input$map_click", str(input$map_click))
      message(": input$map_center", str(input$map_center))
      message(": input$map_zoom", str(input$map_zoom))
      message(": input$map_bbox", str(input$map_bbox))
    }
  })
}

shinyApp(ui, server)
