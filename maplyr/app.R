# packages ----
librarian::shelf(
  bslib, DBI, dplyr, duckdb, glue, here, RColorBrewer, sf,
  shiny, stringr, terra, tibble, tidyr)

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

# data prep ----
r_cell <- terra::rast(cell_tif)

# WRONG: parse query string ----
# query <- isolate(parseQueryString(session$clientData$url_search))
# selected_layer <- query$layer

# ui ----
ui <- page_fluid(
  tags$head(tags$style(HTML(
    ".mapboxgl-popup-content{color:black;}" ))),
  titlePanel("BOEM Marine Sensitivity - Layer Viewer"),

  layout_sidebar(
    sidebar = sidebar(
      h4("Layer Information"),
      uiOutput("layer_info"),
      br(),
      selectInput(
        "sel_scale",
        "Scale Type",
        choices = c(
          "Ecoregion Rescaled" = "rescaled",
          "Raw Values" = "raw"),
        selected = "rescaled"),
      radioButtons(
        "sel_unit",
        "Spatial Units",
        choices = c(
          "Raster cells (0.05°)" = "cell",
          "Planning areas" = "pa"),
        selected = "cell"),
      input_switch(
        "tgl_sphere", "Sphere", T )),

    card(
      full_screen = TRUE,
      mapboxglOutput("map", height = "700px")
    )
  )
)

# server ----
server <- function(input, output, session) {

  # reactive values ----
  rx <- reactiveValues(
    layer = NULL,
    metric_key = NULL)

  # parse URL parameters ----
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query$layer)) {
      rx$layer <- query$layer
    } else {
      rx$layer <- "primprod"  # default
    }
  })

  # * layer_info ----
  output$layer_info <- renderUI({
    req(rx$layer)

    if (rx$layer == "primprod") {
      tagList(
        h5("Primary Productivity"),
        p("Net primary productivity from phytoplankton"),
        p("Units: mmol/m³"),
        p("Source: Copernicus Marine Service")
      )
    } else {
      # get species category info
      sp_cat <- rx$layer |> str_replace("_", " ")

      # get species count
      n_spp <- dbGetQuery(con_sdm, glue("
        SELECT COUNT(*) as n
        FROM species
        WHERE sp_cat = '{sp_cat}'"))[1,1]

      tagList(
        h5(str_to_title(sp_cat)),
        p(glue("Extinction risk for {n_spp} species")),
        p("Based on IUCN Red List categories"),
        p("Values: 0 (low risk) to 100 (high risk)")
      )
    }
  })

  # * get_metric_key ----
  get_metric_key <- reactive({
    req(rx$layer, input$sel_scale)

    if (rx$layer == "primprod") {
      if (input$sel_scale == "rescaled") {
        "primprod_ecoregion_rescaled"
      } else {
        "primprod"
      }
    } else {
      # convert layer to metric key
      sp_cat_u <- rx$layer
      if (input$sel_scale == "rescaled") {
        glue("extrisk_{sp_cat_u}_ecoregion_rescaled")
      } else {
        glue("extrisk_{sp_cat_u}")
      }
    }
  })

  # * get_rast ----
  get_rast <- reactive({
    req(get_metric_key())

    m_key <- get_metric_key()

    d <- dbGetQuery(con_sdm, glue("
        SELECT
          cm.cell_id,
          cm.value
        FROM cell_metric cm
        WHERE cm.metric_seq = (
          SELECT metric_seq
          FROM metric
          WHERE metric_key = '{m_key}' )" ))

    if (nrow(d) == 0) {
      showNotification(
        glue("No data found for metric: {m_key}"),
        type = "error")
      return(NULL)
    }

    stopifnot(sum(duplicated(d$cell_id)) == 0)

    r <- init(r_cell[[1]], NA)
    r[d$cell_id] <- d$value

    r
  })

  # * map ----
  output$map <- renderMapboxgl({
    req(get_rast())

    r      <- get_rast()
    n_cols <- 11
    cols_r <- rev(RColorBrewer::brewer.pal(n_cols, "Spectral"))
    rng_r  <- minmax(r) |> as.numeric() |> signif(digits = 3)

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
      add_image_source(
        id     = "r_src",
        data   = r,
        colors = cols_r) |>
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
      add_raster_layer(
        id                = "r_lyr",
        source            = "r_src",
        raster_opacity    = 0.8,
        raster_resampling = "nearest",
        before_id         = "er_ln") |>
      mapgl::add_legend(
        rx$layer,
        values   = rng_r,
        colors   = cols_r,
        position = "bottom-right") |>
      add_fullscreen_control() |>
      add_navigation_control() |>
      add_scale_control() |>
      add_geocoder_control()
  })

  # * update map based on selections ----
  observeEvent(list(input$sel_unit, input$sel_scale), {
    req(input$sel_unit, get_metric_key())

    map_proxy <- mapboxgl_proxy("map")

    if (input$sel_unit == "cell") {
      # show raster layer
      r <- get_rast()
      if (!is.null(r)) {
        n_cols <- 11
        cols_r <- rev(RColorBrewer::brewer.pal(n_cols, "Spectral"))
        rng_r  <- minmax(r) |> as.numeric() |> signif(digits = 3)

        # remove planning area fill layer if exists
        map_proxy |>
          clear_layer("pa_lyr")

        # update raster
        map_proxy |>
          clear_layer("r_src") |>
          clear_layer("r_lyr") |>
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
          mapgl::add_legend(
            rx$layer,
            values   = rng_r,
            colors   = cols_r,
            position = "bottom-right")
      }

    } else {
      # show planning area layer
      n_cols <- 11
      cols_r <- rev(RColorBrewer::brewer.pal(n_cols, "Spectral"))
      var_pa <- get_metric_key()

      # check if column exists in planning areas
      pa_cols <- dbListFields(con, "ply_planareas_2025")
      if (!(var_pa %in% pa_cols)) {
        showNotification(
          glue("Metric '{var_pa}' not available for planning areas"),
          type = "warning")
        return()
      }

      rng_pa <- tbl(con, "ply_planareas_2025") |>
        pull({{ var_pa }}) |>
        range(na.rm = TRUE)
      cols_pa  <- colorRampPalette(cols_r, space = "Lab")(n_cols)
      brks_pa <- seq(rng_pa[1], rng_pa[2], length.out = n_cols)

      # remove raster layer if exists
      map_proxy |>
        clear_layer("r_lyr")

      # add planning area fill layer
      map_proxy |>
        clear_layer("pa_lyr") |>
        add_fill_layer(
          id                 = "pa_lyr",
          source             = "pa_src",
          source_layer       = "public.ply_planareas_2025",
          fill_color         = interpolate(
            column   = var_pa,
            values   = brks_pa,
            stops    = cols_pa,
            na_color = "lightgrey"),
          fill_outline_color = "white",
          tooltip            = concat(get_column("planarea_name"), ": ", get_column(var_pa)),
          hover_options = list(
            fill_color = "purple",
            fill_opacity = 1 ),
          before_id = "pa_ln" ) |>
        mapgl::add_legend(
          rx$layer,
          values   = rng_pa,
          colors   = cols_pa,
          position = "bottom-right")
    }
  }, ignoreInit = FALSE)

  # handle clicks for additional info
  observeEvent(input$map_click, {
    click <- input$map_click

    if (input$sel_unit == "cell") {
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
            glue("{rx$layer}: {round(val, 3)}"),
            duration = 3,
            type = "message"
          )
        }
      }
    }
  })
}

shinyApp(ui, server)
