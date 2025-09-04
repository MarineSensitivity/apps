# packages ----
librarian::shelf(
  bslib, DBI, dplyr, duckdb, glue, here, htmltools, RColorBrewer,
  scales, sf, shiny, stringr, terra, tibble, tidyr)

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
# source(here("../workflows/libs/db.R")) # con
con_sdm <- dbConnect(duckdb(), dbdir = sdm_dd, read_only = T)

# data prep ----
r_cell <- rast(cell_tif)

d_spp <- tbl(con_sdm, "taxon") |>
  filter(is_ok) |>
  collect() |>
  mutate(
    common_name = case_match(
      scientific_name,
      "Eubalaena glacialis"    ~ "North Atlantic right whale",  # OLD: black right whale
      "Megaptera novaeangliae" ~ "humpback whale",              # OLD: hump
      .default = common_name),
    lbl_cmn = ifelse(
      !is.na(common_name), glue(" ({common_name})", .trim = F), ""),
    label = glue("{sp_cat}: {scientific_name}{lbl_cmn}"),
  #   key_url   = glue('<a href="https://shiny.marinesensitivity.org/mapsp/?sp_key={sp_key}" target="_blank">{sp_key}</a>'),
    worms_url = ifelse(
      is.na(worms_id), NA,
      glue('<a href="https://www.marinespecies.org/aphia.php?p=taxdetails&id={worms_id}" target="_blank">{worms_id}</a>'))) #,
  #   gbif_url  = ifelse(
  #     is.na(gbif_id), NA,
  #     glue('<a href="https://www.gbif.org/species/{gbif_id}" target="_blank">{gbif_id}</a>')))

spp_choices <- d_spp |>
  arrange(sp_cat, label) |>
  group_by(sp_cat) |>
  summarise(
    layer = list(setNames(mdl_seq, label)),
    .groups = "drop") |>
  deframe()

sel_sp_default <- d_spp |>
  filter(scientific_name == "Balaenoptera ricei") |> pull(mdl_seq)

# ui ----
ui <- page_sidebar(
  tags$head(tags$style(HTML(
    ".mapboxgl-popup-content{color:black;}" ))),
  titlePanel("BOEM Marine Sensitivity - Species Distribution"),

  sidebar = sidebar(
    open = F,
    # selectizeInput(
    #   "sel_sp",
    #   "Select Species",
    #   choices = NULL),
      # choices = spp_choices,
      # selected = sel_sp_default),
      # TODO: show comparison of old to new species map
    input_switch(
      "tgl_sphere", "Sphere", T ),
    input_dark_mode(
      id = "tgl_dark", mode = "dark"),
    uiOutput("species_info")),
  selectizeInput(
    "sel_sp",
    "Select Species",
    choices = NULL,
    width   = "100%"),
  card(
    mapboxglOutput("map") ) )

# server ----
server <- function(input, output, session) {

  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query$mdl_seq)) {
      updateSelectizeInput(session, 'sel_sp', choices = spp_choices, server = T, selected = query$mdl_seq)
    } else {
      # updateSelectizeInput(session, 'sel_sp', choices = spp_choices, server = T, selected = NULL)
      updateSelectizeInput(session, 'sel_sp', choices = spp_choices, server = T, selected = sel_sp_default)
    }
  })

  # * species_info ----
  output$species_info <- renderUI({
    req(input$sel_sp)

    # sp_key <- input$sel_sp
    mdl_seq <- input$sel_sp
    d_sp   <- d_spp |>
      filter(mdl_seq == !!mdl_seq)

    with(
      d_sp,
      {tagList(
        h5(scientific_name),
        tags$ul(
          tags$li(HTML(glue("Scientific name: {scientific_name}"))),
          tags$li(glue("Common name: {common_name}")),
          tags$li(glue("Category: {sp_cat}")),
          tags$li(glue("Extinction risk (IUCN RedList): {redlist_code}")),
          tags$li(HTML(glue("MarineSpecies.org: {worms_url}"))) #,
          # tags$li(HTML(glue("GBIF.org: {gbif_url}")))
          ) ) } )
    })

  # * get_rast ----
  get_rast <- reactive({
    req(input$sel_sp)

    mdl_seq <- input$sel_sp
    # mdl_seq = 18232 # Balaenoptera ricei

    d <- tbl(con_sdm, "model_cell") |>
      filter(mdl_seq == !!mdl_seq) |>
      select(cell_id, value) |>
      collect()

    r <- init(r_cell[[1]], NA)
    r[d$cell_id] <- d$value
    names(r) <- "value"

    r
  })

  # * get_name ----
  get_name <- reactive({
    d_spp |>
      filter(mdl_seq == input$sel_sp) |>
      pull(scientific_name)
  })

  # * map ----
  output$map <- renderMapboxgl({

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
    sp_name <- get_name()

    # update raster
    map_proxy |>
      clear_layer("r_lyr") |>
      clear_layer("r_src") |>
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
        sp_name <- get_name()

        val <- terra::extract(r, pt) |> pull(value)
        if (!is.na(val)) {
          showNotification(
            glue("{sp_name} ({round(click$lng, 3)}, {round(click$lat, 3)}): {round(val, 3)}"),
            duration = 3,
            type = "message" ) } } })
}

shinyApp(ui, server)
