# packages ----
librarian::shelf(
  bslib, DBI, dplyr, duckdb, glue, here, RColorBrewer, sf,
  shiny, stringr, terra, tibble, tidyr)

# variables ----
verbose        <- T
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

# data prep ----
r_cell <- rast(cell_tif)

d_spp <- tbl(con_sdm, "model") |>
  select(mdl_seq, taxa) |>
  left_join(
    tbl(con_sdm, "species") |>
      select(taxa, sp_cat, sp_key, scientific_name_dataset, common_name_dataset, worms_id, gbif_id, redlist_code),
    by = "taxa") |>
  collect()

spp_choices <- d_spp |>
  arrange(sp_cat, scientific_name_dataset) |>
  group_by(sp_cat) |>
  summarise(
    layer = list(setNames(sp_key, scientific_name_dataset)),
    .groups = "drop") |>
  deframe()

# ui ----
ui <- page_sidebar(
  tags$head(tags$style(HTML(
    ".mapboxgl-popup-content{color:black;}" ))),
  titlePanel("BOEM Marine Sensitivity - Species Viewer"),

  sidebar = sidebar(
    selectizeInput(
      "sel_sp",
      "Select Species",
      choices = spp_choices), # TODO: show comparison of old to new species map
    input_switch(
      "tgl_sphere", "Sphere", T ),
    input_dark_mode(
      id = "tgl_dark", mode = "dark") ),
  card(
    mapboxglOutput("map") ) )

# server ----
server <- function(input, output, session) {
  # updateSelectizeInput(session, 'sel_sp', choices = spp_choices, server = T)
  # TODO: server side is faster but not allowing for URL update?

  # parse URL parameters ----
  # http://127.0.0.1:3763/?sp_key=ITS-Mam-180528
  # input <- list(sel_sp = "ITS-Mam-180528") # DEBUG
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query$sp_key)) {
      browser()
      updateSelectizeInput(session, "sel_sp", selected = query$sp_key)
    }
  })

  # * species_info ----
  output$species_info <- renderUI({
    req(input$sel_sp)

    with(
      d_spp |>
        filter(sp_key == input$sel_sp), {
      tagList(
        h5(scientific_name_dataset),
        p(glue("Common name: {common_name_dataset}")),
        p(glue("Extinction risk (IUCN RedList): {redlist_code}"))) })
  })

  # * get_rast ----
  get_rast <- reactive({
    req(input$sel_sp)

    # debug: input$sel_sp = "ITS-Mam-180528" # blue whale

    d <- tbl(con_sdm, "species") |>
      select(sp_key, taxa) |>
      filter(sp_key == !!input$sel_sp) |>
      left_join(
        tbl(con_sdm, "model") |>
          select(taxa, mdl_seq),
        by = "taxa") |>
      left_join(
        tbl(con_sdm, "model_cell") |>
          select(mdl_seq, cell_id, value),
        by = "mdl_seq") |>
      select(cell_id, value) |>
      collect()

    r <- init(r_cell[[1]], NA)
    r[d$cell_id] <- d$value

    r
  })

  # * map ----
  output$map <- renderMapboxgl({
    # req(rx$sp_key)

    # r      <- get_rast()
    # n_cols <- 11
    # cols_r <- rev(RColorBrewer::brewer.pal(n_cols, "Spectral"))
    # rng_r  <- minmax(r) |> as.numeric() |> signif(digits = 3)

    mapboxgl(
      style  = mapbox_style("dark"),
      projection = ifelse(input$tgl_sphere, "globe", "mercator"),
      zoom   = 3.5,
      center = c(-106, 40.1)) |>
      add_vector_source(
        id  = "er_src",
        url = "https://api.marinesensitivity.org/tilejson?table=public.ply_ecoregions_2025") |>
      add_vector_source(
        id  = "pa_src",
        url = "https://api.marinesensitivity.org/tilejson?table=public.ply_planareas_2025") |>
      # add_image_source(
      #   id     = "r_src",
      #   data   = r,
      #   colors = cols_r) |>
      add_line_layer(
        id           = "pa_ln",
        source       = "pa_src",
        source_layer = "public.ply_planareas_2025",
        line_color   = "white",
        line_opacity = 1,
        line_width   = 1) |>
      add_line_layer(
        id           = "er_ln",
        source       = "er_src",
        source_layer = "public.ply_ecoregions_2025",
        line_color   = "black",
        line_opacity = 1,
        line_width   = 3,
        before_id    = "pa_ln") |>
      # add_raster_layer(
      #   id                = "r_lyr",
      #   source            = "r_src",
      #   raster_opacity    = 0.8,
      #   raster_resampling = "nearest",
      #   before_id         = "er_ln") |>
      # mapgl::add_legend(
      #   rx$layer,
      #   values   = rng_r,
      #   colors   = cols_r,
      #   position = "bottom-right") |>
      add_fullscreen_control() |>
      add_navigation_control() |>
      add_scale_control() |>
      add_geocoder_control() |>
      add_globe_minimap(position = "top-left")
  })

  # * input$sel_sp -> update map ----
  observeEvent(input$sel_sp, {
    req(input$sel_sp)

    if (verbose)
      message("observeEvent(input$sel_sp): ", input$sel_sp)

    map_proxy <- mapboxgl_proxy("map")

    r <- get_rast()

    n_cols <- 11
    cols_r <- rev(RColorBrewer::brewer.pal(n_cols, "Spectral"))
    rng_r  <- minmax(r) |> as.numeric() |> signif(digits = 3)

    # get species name for legend
    sp_name <- d_spp |>
      filter(sp_key == input$sel_sp) |>
      pull(scientific_name_dataset) |>
      first()

    # update raster
    map_proxy |>
      clear_layer("r_src") |>
      clear_layer("r_lyr") |>
      clear_legend() |>
      add_image_source(
        id     = "r_src",
        data   = r,
        colors = cols_r) |>
      add_raster_layer(
        id                = "r_lyr",
        source            = "r_src",
        raster_opacity    = 0.8,
        raster_resampling = "nearest",
        before_id         = "er_ln") |>
      add_legend(
        sp_name,
        values   = rng_r,
        colors   = cols_r,
        position = "bottom-right") |>
      fit_bounds(
        bbox = trim(r) |> st_bbox() |> as.numeric(),
        animate = T)
  })


  # map_click ----
  observeEvent(input$map_click, {
    click <- input$map_click

      # extract cell value at clicked location
      pt <- vect(
        data.frame(x = click$lng, y = click$lat),
        geom = c("x", "y"),
        crs = "EPSG:4326") |>
        st_as_sf() |>
        st_shift_longitude()

      r <- get_rast()
      if (!is.null(r)) {
        val <- terra::extract(r, pt)[1,1]
        if (!is.na(val)) {
          showNotification(
            glue("{input$sel_sp}: {round(val, 3)}"),
            duration = 3,
            type = "message" ) } } })
}

shinyApp(ui, server)
