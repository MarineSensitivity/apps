# packages ----
librarian::shelf(
  bslib, DBI, dplyr, duckdb, glue, here, purrr, sf, shiny, stringr,
  terra, tibble, tidyr)

# variables ----
verbose        <- F
is_server      <-  Sys.info()[["sysname"]] == "Linux"
dir_private    <- ifelse(
  is_server,
  "/share/private",
  "~/My Drive/private")
dir_data       <- ifelse(
  is_server,
  "/share/data",
  "~/My Drive/projects/msens/data")
mapbox_tkn_txt <- glue("{dir_private}/mapbox_token_bdbest.txt")
cell_tif       <- glue("{dir_data}/derived/r_bio-oracle_planarea.tif")
sdm_dd         <- glue("{dir_data}/derived/sdm.duckdb")

Sys.setenv(MAPBOX_PUBLIC_TOKEN=readLines(mapbox_tkn_txt))
librarian::shelf(
  mapgl)

# database ----
source(here("../workflows/libs/db.R")) # con
con_sdm <- dbConnect(duckdb(), dbdir = sdm_dd, read_only = T)
# dbDisconnect(con, shutdown = T) # TODO: disconnect when Shiny closes
# duckdb_shutdown(duckdb())

# data prep ----
r_cell <- terra::rast(cell_tif)

# * lyrs ----
sp_cats   <- tbl(con_sdm, "species") |> distinct(sp_cat) |> pull(sp_cat) |> sort()
sp_cats_u <- sp_cats |> str_replace(" ", "_")

d_lyrs <- bind_rows(
  tibble(
    order    = 1,
    category = "Overall",
    source   = "db_metric",
    layer    = "score",
    lyr      = "score_extriskspcat_primprod_ecoregionrescaled_equalweights"),
  tibble(
    order    = 2,
    category = "Species, rescaled by Ecoregion",
    layer    = glue("{sp_cats}: ext. risk, ecorgn"),
    lyr      = glue("extrisk_{sp_cats_u}_ecoregion_rescaled")),
  tibble(
    order    = 3,
    category = "Primary Productivity, rescaled by Ecoregion",
    layer    = glue("{sp_cats}: ext. risk, ecorgn"),
    lyr      = glue("extrisk_{sp_cats_u}_ecoregion_rescaled")),
  tibble(
    order    = 4,
    category = "Species, raw Extinction Risk",
    layer    = glue("{sp_cats}: ext. risk"),
    lyr      = glue("extrisk_{sp_cats_u}")),
  tibble(
    order    = 5,
    category = "Primary Productivity, raw Phytoplankton",
    lyr      = "primprod",
    layer    = "primary productivity (mmol/m^3)" ) )

# confirm all layers available for both planareas and cell metrics
lyrs_pa   <- dbListFields(con, "ply_planareas_2025")
lyrs_cell <- tbl(con_sdm, "metric") |>
  semi_join(
    tbl(con_sdm, "cell_metric") |>
      distinct(metric_seq),
    by = "metric_seq") |>
  pull(metric_key)
stopifnot(all(d_lyrs$lyr %in% lyrs_pa))
stopifnot(all(d_lyrs$lyr %in% lyrs_cell))

lyr_choices <- d_lyrs |>
  group_by(order, category) |>
  summarise(
    layer = list(setNames(lyr, layer)),
    .groups = "drop") |>
  arrange(order, layer) |>
  select(-order) |>
  deframe()

# ui ----
light <- bs_theme()
# dark <- bs_theme(bg = "black", fg = "white", primary = "purple")
dark <- bs_theme()
ui <- page_sidebar(
  tags$head(tags$style(HTML(
    ".mapboxgl-popup-content{color:black;}" ))),
  title = "BOEM Marine Sensitivity",
  sidebar = sidebar(
    selectInput(
      "sel_unit",
      "Spatial units",
      choices = c(
        "Raster cells (0.05°)" = "cell",
        "Planning areas"       = "pa")),
    selectInput(
      "sel_lyr",
      "Layer",
      choices = lyr_choices),
    input_switch(
      "tgl_sphere", "Sphere", T ),
    input_dark_mode(
      id = "tgl_dark", mode = "dark")),
  card(
    full_screen = TRUE,
    mapboxglOutput("map") ) )

# server ----
server <- function(input, output, session) {

  # observe(session$setCurrentTheme(
  #   if (isTRUE(input$tgl_dark)) dark else light
  # ))

  # * get_rast ----
  get_rast <- reactive({
    req(input$sel_unit, input$sel_lyr)

    m_key <- input$sel_lyr

    d <- dbGetQuery(con_sdm, glue("
        SELECT
          cm.cell_id,
          cm.value
        FROM cell_metric cm
        WHERE cm.metric_seq = (
          SELECT metric_seq
          FROM metric
          WHERE metric_key = '{m_key}' )" ))
    stopifnot(sum(duplicated(d$cell_id)) == 0)

    r <- init(r_cell[[1]], NA)
    r[d$cell_id] <- d$value

    r
  })

  # * map ----
  output$map <- renderMapboxgl({

    r <- get_rast()
    n_cols <- 11
    cols_r <- rev(RColorBrewer::brewer.pal(n_cols, "Spectral"))
    rng_r <- minmax(r) |> as.numeric() |> signif(digits = 3)

    var_pa <- "score_extriskspcat_primprod_ecoregionrescaled_equalweights"
    rng_pa <- tbl(con, "ply_planareas_2025") |>
      pull({{ var_pa }}) |>
      range() # |> signif(digits = 3)
    cols_pa  <- colorRampPalette(cols_r, space = "Lab")(n_cols)
    brks_pa <- seq(rng_pa[1], rng_pa[2], length.out = n_cols)

    mapboxgl(
      style  = mapbox_style("dark"),
      projection = ifelse(input$tgl_sphere, "globe", "mercator"),
      zoom   = 3.5,
      center = c(-106, 40.1)) |>
      add_vector_source(
        id  = "pa_src",
        url = "https://api.marinesensitivity.org/tilejson?table=public.ply_planareas_2025") |>
      add_vector_source(
        id  = "er_src",
        url = "https://api.marinesensitivity.org/tilejson?table=public.ply_ecoregions_2025") |>
      add_image_source(
        id     = "r_src",
        data   = r,
        colors = cols_r) |>
      add_raster_layer(
        id                = "r_lyr",
        source            = "r_src",
        raster_opacity    = 0.6,
        raster_resampling = "nearest") |>
      add_line_layer(
        id           = "er_ln",
        source       = "er_src",
        source_layer = "public.ply_ecoregions_2025",
        line_color   = "black",
        line_opacity = 0.8,
        line_width   = 0.5) |>
      add_line_layer(
        id           = "pa_ln",
        source       = "pa_src",
        source_layer = "public.ply_planareas_2025",
        line_color   = "white",
        line_opacity = 0.8,
        line_width   = 0.5) |>
      add_fill_layer(
        id                 = "pa_ply",
        source             = "pa_src",
        source_layer       = "public.ply_planareas_2025",
        fill_color         = interpolate(
          column   = var_pa,
          values   = brks_pa,
          stops    = cols_pa,
          na_color = "lightgrey"),
        fill_outline_color = "white",
        tooltip            = concat("Value: ", get_column(var_pa)),
        hover_options = list(
          fill_color = "purple",
          fill_opacity = 1 ) ) |>
      mapgl::add_legend(
        "Colorscale",
        values   = rng_r,
        colors   = cols_r,
        position = "bottom-right") |>
      add_fullscreen_control() |>
        # position = "top-left") |>
      add_navigation_control() |>
      add_scale_control() |>
      add_layers_control() |>
      # add_globe_control() # only for MapLibre maps
      add_geocoder_control() |>
      add_draw_control(position = "top-right")

  })

  # * map_click ----
  observeEvent(input$map_click, {
    if (verbose){
      message(": input$map_click", str(input$map_click))
      message(": input$map_center", str(input$map_center))
      message(": input$map_zoom", str(input$map_zoom))
      message(": input$map_bbox", str(input$map_bbox))
    }
  })
}

shinyApp(ui, server)
