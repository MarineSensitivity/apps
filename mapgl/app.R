librarian::shelf(
  bslib, DBI, dplyr, duckdb, glue, here, mapgl, purrr, sf, shiny, stringr,
  terra, tibble, tidyr)

verbose   <- T
is_server <-  Sys.info()[["sysname"]] == "Linux"
dir_data  <- ifelse(
  is_server,
  "/share/data",
  "~/My Drive/projects/msens/data")
cell_tif  <- glue("{dir_data}/derived/r_bio-oracle_planarea.tif")
sdm_dd    <- glue("{dir_data}/derived/sdm.duckdb")

con_sdm <- dbConnect(duckdb(), dbdir = sdm_dd, read_only = T)

r_cell <- terra::rast(cell_tif) # |>  # |> rotate()
  # subset("prim_prod_mean")     # "depth_mean"
# plot(r) # plet(r)

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
    source   = "db_metric",
    layer    = glue("{sp_cats}: ext. risk, ecorgn"),
    lyr      = glue("extrisk_{sp_cats_u}_ecoregion_rescaled")),
  tibble(
    order    = 3,
    category = "Species, raw Extinction Risk",
    source   = "db_metric",
    layer    = glue("{sp_cats}: ext. risk"),
    lyr      = glue("extrisk_{sp_cats_u}")),
  tibble(
    order    = 4,
    category = "Environment",
    source   = "r_cell",
    lyr      = names(r_cell) |> setdiff("cell_id"),
    layer    = names(r_cell) |> setdiff("cell_id") ) )
# View(d_lyrs)

lyr_choices <- d_lyrs |>
  group_by(order, category) |>
  summarise(
    layer = list(setNames(lyr, layer)), 
    .groups = "drop") |>
  arrange(order, layer) |>
  select(-order) |>
  deframe()
View(d_lyrs)

light <- bs_theme()
dark <- bs_theme(bg = "black", fg = "white", primary = "purple")
ui <- page_sidebar(
  title = "BOEM Marine Sensitivity",
  sidebar = sidebar(
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

server <- function(input, output, session) {

  # observe(session$setCurrentTheme(
  #   if (isTRUE(input$tgl_dark)) dark else light
  # ))

  get_rast <- reactive({
    req(input$sel_lyr)

    lyr_val <- input$sel_lyr  # lyr_val = selected lyr value
    src <- d_lyrs |>
      filter(lyr == !!lyr_val) |>
      pull(source)

    if (verbose)
      message("Source: ", src, " for lyr: ", lyr_val)
    if (src == "r_cell") {
      r <- r_cell[[lyr_val]]
    } else {  # src == "db_metric"

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
    }

    r
  })

  output$map <- renderMapboxgl({

    r <- get_rast()
    cols_r <- rev(RColorBrewer::brewer.pal(11, "Spectral"))
    rng_r <- minmax(r) |> as.numeric() |> signif(digits = 3)

    mapboxgl(
      style  = mapbox_style("dark"),
      projection = ifelse(input$tgl_sphere, "globe", "mercator"),
      zoom   = 3.5,
      center = c(-106, 40.1)) |>
      add_vector_source(
        id  = "vect_src",
        url = 'https://api.marinesensitivity.org/tilejson?table=public.ply_planareas_2025') |>
      add_fill_layer(
        id                 = "vect_lyr",
        source             = "vect_src",
        source_layer       = "public.ply_planareas_2025",
        fill_color         = "transparent",
        fill_outline_color = "white",
        tooltip            = "planarea_name") |>
      add_image_source(
        id     = "r_src",
        data   = r,
        colors = cols_r) |>
      add_raster_layer(
        id                = 'r_lyr',
        source            = 'r_src',
        raster_opacity    = 0.6,
        raster_resampling = "nearest") |>
      mapgl::add_legend(
        "Colorscale",
        values   = rng_r,
        colors   = cols_r,
        position = "bottom-right") |>
      add_fullscreen_control(
        position = "top-left") |>
      add_navigation_control() |>
      add_scale_control()

  })

  observeEvent(input$map_click, {
    # mapboxgl_proxy("map")
    # browser()
    if (verbose){
      message(": input$map_click", str(input$map_click))
      message(": input$map_center", str(input$map_center))
      message(": input$map_zoom", str(input$map_zoom))
      message(": input$map_bbox", str(input$map_bbox))
    }
  })
}

shinyApp(ui, server)
