# packages ----
librarian::shelf(
  bslib, DBI, dplyr, duckdb, DT, ggiraph, ggplot2, glue, here, purrr,
  RColorBrewer, readr, sf, shiny, stringr, terra, tibble, tidyr)
options(readr.show_col_types = F)

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
spp_global_csv <- here("mapgl/spp_global_cache.csv")

Sys.setenv(MAPBOX_PUBLIC_TOKEN=readLines(mapbox_tkn_txt))
librarian::shelf(
  mapgl)

# database ----
source(here("../workflows/libs/db.R")) # con
con_sdm <- dbConnect(duckdb(), dbdir = sdm_dd, read_only = T)
# dbDisconnect(con, shutdown = T) # TODO: disconnect when Shiny closes
# duckdb_shutdown(duckdb())

# flower plot function ----
plot_flower <- function(
    data,
    fld_category,
    fld_height,
    fld_width,
    tooltip_expr = NULL,
    score        = NULL,
    title        = NULL,
    colors       = "Set2"){

  stopifnot(is.numeric(data |> pull({{ fld_height }})))
  stopifnot(is.numeric(data |> pull({{ fld_width }})))

  if (is.null(score)){
    score <- data |>
      # ensure both are not just integer (weighted.mean goes to 0)
      mutate(
        "{{fld_height}}" := as.double({{ fld_height }}),
        "{{fld_width}}"  := as.double({{ fld_width  }}) ) |>
      summarize(
        score = weighted.mean({{ fld_height }}, {{ fld_width }}, na.rm = T)) |>
      pull(score)
  }

  # Calculate positions
  d <- data |>
    arrange({{ fld_category }}) |>
    mutate(across(!where(is.character), as.double)) |>
    mutate(
      # Calculate angles for plotting
      ymax    = cumsum({{ fld_width }}),
      ymin    = lag(ymax, default=0), # ,  c(0, head(ymax, n=-1)),
      xmax    = {{ fld_height }},
      xmin    = 0)

  sym_category <- ensym(fld_category)
  sym_height   <- ensym(fld_height)
  sym_width    <- ensym(fld_width)

  if (!is.null(tooltip_expr)){
    d <- d |>
      mutate(
        tooltip = glue(tooltip_expr))
  } else {
    d <- d |>
      mutate(
        tooltip = glue("{!!fld_category}"))
  }

  g <- ggplot(d) +
    geom_rect_interactive(aes(
      xmin    = xmin,
      xmax    = xmax,
      ymin    = ymin,
      ymax    = ymax,
      fill    = {{ fld_category }},
      color   = "white",
      data_id = {{ fld_category }},
      tooltip = tooltip),
      color = "white",
      alpha = 0.5) +
    coord_polar(theta = "y") +
    # Create donut hole
    xlim(c(-10, max(data |> pull({{ fld_height }})))) +
    # Add center score
    annotate(
      "text", x = -10, y = 0,
      label = round(score),
      size = 8,
      fontface = "bold") +
    # scale_fill_brewer(
    #   palette = colors) +
    # scale_fill_brewer() +
    theme_minimal() +
    # theme_void() +
    theme(
      legend.position = "bottom",
      plot.margin = unit(c(20, 20, 20, 20), "pt"))

  if (!is.null(title))
    g <- g +
      ggtitle(title)

  girafe(
    ggobj = g,
    options = list(
      opts_sizing(rescale = TRUE, width = 1),
      opts_tooltip(css = "background-color:white;color:black;padding:5px;border-radius:3px;")))
}

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

# d_lyrs |>
#   group_by(order, category) |>
#   summarize(
#     n = n(),
#     lyrs = paste(layer, collapse = "; "),
#     .groups = "drop")

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

# * d_spp_global ----
if (file.exists(spp_global_csv)) {
  d_spp_global <- read_csv(spp_global_csv)
} else {
  message("Generating spp_global_csv from database...")
  tbl(con_sdm, "model_cell") |>
    select(mdl_seq, cell_id, suitability = value) |>
    left_join(
      tbl(con_sdm, "model") |>
        select(mdl_seq, taxa),
      by = "mdl_seq") |>
    group_by(taxa) |>
    summarize(
      suitability = mean(suitability, na.rm = TRUE),
      .groups = "drop") |>
    left_join(
      tbl(con_sdm, "species") |>
        select(taxa, sp_key, worms_id, gbif_id, sp_cat, scientific_name_dataset, common_name_dataset, redlist_code),
      by = "taxa") |>    # TODO: later manage multiple species per model; assume 1 taxa to 1 species for now
    mutate(
      rl_score  = case_match(
        redlist_code,
        "CR" ~ 1.0,      # Critically Endangered
        "EN" ~ 0.8,    # Endangered
        "VU" ~ 0.6,    # Vulnerable
        "NT" ~ 0.4,    # Near Threatened
        "LC" ~ 0.2),   # Least Concern
      suit_rl   = suitability * rl_score,
      pct_total = suit_rl / sum(suit_rl, na.rm = T),
      is_pct_na = is.na(pct_total)) |>
    arrange(is_pct_na, desc(pct_total), scientific_name_dataset) |>
    collect() |>
    select(
      sp_cat, sp_key, scientific_name_dataset, common_name_dataset, worms_id, gbif_id,
      redlist_code, rl_score, suitability, suit_rl, pct_total) |>
    write_csv(spp_global_csv)
}

# ui ----
light <- bs_theme()
# dark <- bs_theme(bg = "black", fg = "white", primary = "purple")
dark <- bs_theme()
ui <- page_sidebar(
  tags$head(tags$style(HTML(
    ".mapboxgl-popup-content{color:black;}
     .bslib-full-screen .girafe_container_std {
       height: calc(100vh - 120px) !important;
       width: 100% !important;
     }
     .bslib-full-screen .card-body {
       height: calc(100vh - 120px) !important;
       display: flex;
       flex-direction: column;
     }
     .bslib-full-screen #plot_flower {
       height: 100% !important;
       flex: 1;
     }
     #plot_flower {
       height: 300px;
     }" ))),
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

  navset_card_tab(
    full_screen = TRUE,
    nav_panel(
      "Map",
      mapboxglOutput("map"),
      conditionalPanel(
        absolutePanel(
          id          = "flower_panel",
          top         = 80,
          left        = 30,
          # width       = 350,
          height      = "auto",
          draggable   = T,
          accordion(accordion_panel(
            "Plot",
            card(
            # style       = "resize:vertical;",
            full_screen = T,
            # card_header(class = "bg-dark", "Score"),
            card_body(
              # girafeOutput("plot_flower", height = "300px") ) ) ),
              girafeOutput("plot_flower", height = "100%") ) ) ) ) ),
        condition = 'output.flower_status')),
    nav_panel(
      "Species",
      card(
        card_header(
          textOutput("species_table_header", inline = T),
          class = "d-flex justify-content-between align-items-center",
          downloadButton("download_data", "Download CSV", class = "btn-sm")),
        card_body(
          DTOutput("species_table")))) ) )

# server ----
server <- function(input, output, session) {

  # observe(session$setCurrentTheme(
  #   if (isTRUE(input$tgl_dark)) dark else light
  # ))

  # reactive values ----
  rx <- reactiveValues(
    clicked_pa             = NULL,
    clicked_cell           = NULL,
    species_table          = NULL,
    species_table_header   = NULL,
    species_table_filename = NULL)

  output$flower_status <- reactive({
    if (!is.null(rx$clicked_pa) || !is.null(rx$clicked_cell))
      return(T)
    F
  })
  outputOptions(output, "flower_status", suspendWhenHidden = FALSE)

  # * get_rast ----
  get_rast <- reactive({
    req(input$sel_lyr)

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

    # default to show raster score
    r      <- get_rast()
    n_cols <- 11
    cols_r <- rev(RColorBrewer::brewer.pal(n_cols, "Spectral"))
    rng_r  <- minmax(r) |> as.numeric() |> signif(digits = 3)

    # input = list(tgl_sphere = T)
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
        raster_opacity    = 0.6,
        raster_resampling = "nearest",
        before_id         = "er_ln") |>
      mapgl::add_legend(
        "Value",
        values   = rng_r,
        colors   = cols_r,
        position = "bottom-right") |>
      add_fullscreen_control() |>
      add_navigation_control() |>
      add_scale_control() |>
      # add_layers_control() |>
      # add_draw_control(position = "top-right") |>
      add_geocoder_control(placeholder = "Go to location")

  })

  # * update map based on unit selection ----
  observeEvent(list(input$sel_unit, input$sel_lyr), {
    req(input$sel_unit, input$sel_lyr)

    map_proxy <- mapboxgl_proxy("map")

    if (input$sel_unit == "cell") {
      if (verbose)
        message(glue("update map cell"))

      # show raster layer
      r      <- get_rast()
      n_cols <- 11
      cols_r <- rev(RColorBrewer::brewer.pal(n_cols, "Spectral"))
      rng_r  <- minmax(r) |> as.numeric() |> signif(digits = 3)

      # remove planning area fill layer if exists
      map_proxy |>
        clear_layer("pa_lyr")

      # update or add raster source and layer
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
          raster_opacity    = 0.6,
          raster_resampling = "nearest",
          before_id         = "er_ln") |>
        mapgl::add_legend(
          "Colorscale",
          values   = rng_r,
          colors   = cols_r,
          position = "bottom-right")

    } else {
      if (verbose)
        message(glue("update map pa"))

      # show planning area layer
      n_cols <- 11
      cols_r <- rev(RColorBrewer::brewer.pal(n_cols, "Spectral"))
      var_pa <- input$sel_lyr

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
          tooltip            = concat("Value: ", get_column(var_pa)),
          hover_options = list(
            fill_color = "purple",
            fill_opacity = 1 ),
          before_id = "pa_ln" ) |>
        mapgl::add_legend(
          "Colorscale",
          values   = rng_pa,
          colors   = cols_pa,
          position = "bottom-right")
    }
  }, ignoreInit = FALSE)

  # * map_click ----
  observeEvent(input$map_click, {
    req(input$map_click)

    click <- input$map_click

    if (input$sel_unit == "cell") {
      # handle raster click
      lng <- click$lng
      lat <- click$lat

      # extract cell value at clicked location
      pt <- vect(data.frame(x = lng, y = lat), geom = c("x", "y"), crs = "EPSG:4326") |>
        st_as_sf() |>
        st_shift_longitude()  # [-180,180] -> [0,360]
      cell_id <- terra::extract(r_cell$cell_id , pt) |> pull(cell_id)

      if (!is.na(cell_id)) {
        rx$clicked_cell <- list(
          lng      = lng,
          lat      = lat,
          cell_id  = cell_id,
          lyr      = input$sel_lyr)
      }

    } else {
      # handle planning area click
      if (!is.null(input$map_feature_click)) {
        rx$clicked_pa <- list(
          id         = input$map_feature_click$id,
          properties = input$map_feature_click$properties)
      }
    }
  })

  # * plot_flower ----
  output$plot_flower <- renderGirafe({

    # set height based on container size
    height <- "100%"

    if (input$sel_unit == "cell" && !is.null(rx$clicked_cell)) {
      # get data for cell
      cell_id <- rx$clicked_cell$cell_id
      lng     <- rx$clicked_cell$lng |> round(3)
      lat     <- rx$clicked_cell$lat |> round(3)

      # get species group scores for the cell
      d_fl <- tbl(con_sdm, "metric") |>
        filter(str_detect(metric_key, ".*_ecoregion_rescaled$")) |>
        left_join(
          tbl(con_sdm, "cell_metric"),
          by = "metric_seq") |>
        filter(cell_id == !!cell_id) |>
        select(metric_key, score = value) |>
        mutate(
          component = metric_key |>
            str_replace("extrisk_","") |>
            str_replace("_ecoregion_rescaled","") |>
            str_replace("_", " "),
          even = 1) |>
        filter(component != "all") |>
        collect()

      if (nrow(d_fl) > 0) {
        d_fl |>
          plot_flower(
            fld_category = component,
            fld_height   = score,
            fld_width    = even,
            tooltip_expr = "{component}: {round(score, 2)}",
            title        = glue("Cell ID: {cell_id} (x: {lng}, y: {lat})"))
      }

    } else if (input$sel_unit == "pa" && !is.null(rx$clicked_pa)) {

      # get data for planning area
      pa_name <- rx$clicked_pa$properties$planarea_name
      pa_key  <- rx$clicked_pa$properties$planarea_key


      l <- rx$clicked_pa$properties
      l <- l[str_detect(names(l), "_ecoregion_rescaled$")]

      d_fl <- tibble(
        metric_key = names(l),
        score      = unlist(l)) |>
        mutate(
          component = metric_key |>
            str_replace("extrisk_","") |>
            str_replace("_ecoregion_rescaled","") |>
            str_replace("_", " "),
          even = 1) |>
        filter(component != "all")

      if (nrow(d_fl) > 0) {
        d_fl |>
          plot_flower(
            fld_category = component,
            fld_height   = score,
            fld_width    = even,
            tooltip_expr = "{component}: {round(score, 2)}",
            title        = pa_name)
      }
    }
  })

  # * click_info ----
  output$click_info <- renderPrint({
    if (input$sel_unit == "cell" && !is.null(rx$clicked_cell)) {
      cat("Location:", round(rx$clicked_cell$lng, 4), ",", round(rx$clicked_cell$lat, 4))
    } else if (input$sel_unit == "pa" && !is.null(rx$clicked_pa)) {
      cat("Planning Area:", rx$clicked_pa$feature$properties$planarea_name)
    }
  })

  # * species_table_header ----

  output$species_table_header <- renderText({
    req(rx$species_table_header)
    rx$species_table_header
  })

  # * get_species_table ----
  get_species_table <- reactive({

    # ** global ----
    if (is.null(rx$clicked_cell) && is.null(rx$clicked_pa)){
      rx$species_table_header   <- "Species across USA"
      rx$species_table_filename <- "species_usa"
      d_spp <- d_spp_global
    }

    # ** cell ----
    if (input$sel_unit == "cell" && !is.null(rx$clicked_cell)) {
      # see original: https://github.com/MarineSensitivity/workflows/blob/76d711aed5ea319bde44158efade00a02c1031e4/ingest_aquamaps_to_sdm_duckdb.qmd#L1817-L1954

      cell_id <- rx$clicked_cell$cell_id
      rx$species_table_header   <- glue("Species for Cell ID: {cell_id}")
      rx$species_table_filename <- glue("species_cellid-{cell_id}")

      d_spp <- tbl(con_sdm, "model_cell") |>
        select(mdl_seq, cell_id, suitability = value) |>
        filter(cell_id == !!cell_id) |>
        left_join(
          tbl(con_sdm, "model") |>
            select(mdl_seq, taxa),
          by = "mdl_seq") |>
        left_join(
          tbl(con_sdm, "species") |>
            select(taxa, sp_key, worms_id, gbif_id, sp_cat, scientific_name_dataset, common_name_dataset, redlist_code),
          by = "taxa") |>    # TODO: later manage multiple species per model; assume 1 taxa to 1 species for now
        mutate(
          rl_score  = case_match(
            redlist_code,
            "CR" ~ 1.0,      # Critically Endangered
            "EN" ~ 0.8,    # Endangered
            "VU" ~ 0.6,    # Vulnerable
            "NT" ~ 0.4,    # Near Threatened
            "LC" ~ 0.2),   # Least Concern
          suit_rl   = suitability * rl_score,
          pct_total = suit_rl / sum(suit_rl, na.rm = T),
          is_pct_na = is.na(pct_total)) |>
        arrange(is_pct_na, desc(pct_total), scientific_name_dataset) |>
        collect() |>
        select(
          sp_cat, sp_key, scientific_name_dataset, common_name_dataset, worms_id, gbif_id,
          redlist_code, rl_score, suitability, suit_rl, pct_total)
    }

    # ** pa ----
    if (input$sel_unit == "pa" && !is.null(rx$clicked_pa)) {

      pa_key  <- rx$clicked_pa$properties$planarea_key
      pa_name <- rx$clicked_pa$properties$planarea_name
      # pa_key <- "ALA" # DEBUG
      rx$species_table_header   <- glue("Species for Planning Area: {pa_name}")
      rx$species_table_filename <- glue("species_planarea-{str_replace(pa_name, ' ', '-') |> str_to_lower()}")

      d_spp <- tbl(con_sdm, "zone") |>
        filter(
          tbl   == "ply_planareas_2025",
          value == !!pa_key) |>
        select(zone_seq) |>
        left_join(
          tbl(con_sdm, "zone_cell") |>
            select(zone_seq, cell_id, pct_covered),
          by = "zone_seq") |>
        left_join(
          tbl(con_sdm, "model_cell") |>
          select(mdl_seq, cell_id, suitability = value),
          by = "cell_id") |>
        left_join(
          tbl(con_sdm, "model") |>
            select(mdl_seq, taxa),
          by = "mdl_seq") |>
        group_by(taxa) |>
        summarize(
          # suitability = (suitability * pct_covered) / sum(pct_covered, na.rm = T),
          suitability = weighted_avg(suitability, pct_covered),
          .groups = "drop") |>
        left_join(
          tbl(con_sdm, "species") |>
            select(taxa, sp_key, worms_id, gbif_id, sp_cat, scientific_name_dataset, common_name_dataset, redlist_code),
          by = "taxa") |>    # TODO: later manage multiple species per model; assume 1 taxa to 1 species for now
        mutate(
          rl_score  = case_match(
            redlist_code,
            "CR" ~ 1.0,      # Critically Endangered
            "EN" ~ 0.8,    # Endangered
            "VU" ~ 0.6,    # Vulnerable
            "NT" ~ 0.4,    # Near Threatened
            "LC" ~ 0.2),   # Least Concern
          suit_rl   = suitability * rl_score,
          pct_total = suit_rl / sum(suit_rl, na.rm = T),
          is_pct_na = is.na(pct_total)) |>
        arrange(is_pct_na, desc(pct_total), scientific_name_dataset) |>
        collect() |>
        select(
          sp_cat, sp_key, scientific_name_dataset, common_name_dataset, worms_id, gbif_id,
          redlist_code, rl_score, suitability, suit_rl, pct_total)
    }

    # rename columns
    d_spp |>
      select(
        category   = sp_cat,
        key        = sp_key,
        scientific = scientific_name_dataset,
        common     = common_name_dataset,
        worms      = worms_id,
        gbif       = gbif_id,
        rl_code    = redlist_code,
        rl_score,
        suit       = suitability,
        suit_rl,
        pct_all    = pct_total)
  })

  # * species_table ----
  output$species_table <- renderDT({
    d <- get_species_table()
    # d <- d_spp_global # DEBUG

    d <- d |>
      mutate(
        key   = glue('<a href="https://shiny.marinesensitivity.org/mapsp/?sp_key={key}" target="_blank">{key}</a>'),
        worms = ifelse(
          is.na(worms),
          NA,
          glue('<a href="https://www.marinespecies.org/aphia.php?p=taxdetails&id={worms}" target="_blank">{worms}</a>')),
        gbif  = ifelse(
          is.na(gbif),
          NA,
          glue('<a href="https://www.gbif.org/species/{gbif}" target="_blank">{gbif}</a>')))
        # suitability = round(suitability, 1),
        # suit_rl     = round(suit_rl, 1)
        # pct_total   = round(pct_total * 100, 3) ) |>
    # TODO: add n_cells and avg_suitability for PlanAreas (or drawn areas)

    # store for download
    rx$species_table <- d

    # select columns to display
    # if (input$sel_unit == "cell") {
    #   display_cols <- c("cell_id", "component_link", "metric_name", "value", "scale_type")
    #   col_names <- c("Cell ID", "Component", "Metric", "Value", "Scale")
    # } else {
    #   display_cols <- c("zone_name", "component_link", "metric_name", "value", "scale_type")
    #   col_names <- c("Planning Area", "Component", "Metric", "Value", "Scale")
    # }

    # browser()
    # TODO: rename columns and add explanation with info popups
    datatable(
      # d[, display_cols],
      # colnames = col_names,
      # d |> slice(1:100), # DEBUG
      d,
      escape     = F,  # allow HTML in component_link column
      # filter     = "top",
      class      = "display compact",
      # extensions = c("Buttons", "ColReorder", "KeyTable", "Responsive"),
      extensions = c("ColReorder", "KeyTable", "Responsive"),
      options    = list(
        # buttons    = c("copy", "csv", "excel", "pdf", "print"),
        colReorder = T, # ColReorder
        keys       = T, # KeyTable
        pageLength = 5,
        lengthMenu = c(5, 50, 100),
        # scrollX    = TRUE,
        # scrollY    = "600px",
        # dom        = 'Bfrtip',
        dom        = 'lfrtip') ) |>
        # columnDefs = list(
        #   list(className = 'dt-right', targets = which(display_cols == "value") - 1) ))  |>
      formatPercentage(c("pct_all"), 2) |>
      formatRound(c("suit","suit_rl"), 1)
  }, server = T)

  # * download_data ----
  output$download_data <- downloadHandler(
    filename = function() {
      rx$species_table_filename |>
        paste0("_", Sys.Date(), ".csv") },
    content = function(file) {
      req(rx$species_table_header, rx$species_table)
      write_csv(rx$species_table, file) } )
}

shinyApp(ui, server)
