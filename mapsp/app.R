# packages ----
librarian::shelf(
  bslib,
  DBI,
  dplyr,
  duckdb,
  glue,
  here,
  htmltools,
  RColorBrewer,
  scales,
  sf,
  shiny,
  stringr,
  terra,
  tibble,
  tidyr,
  quiet = T
)

options(
  shiny.minified = T,
  sass.cache = F,
  bslib.precompiled = T,
  bslib.color_contrast_warnings = F,
  shiny.autoreload = T,
  shiny.autoreload.legacy_warning = F # Warning: Using legacy autoreload file watching. Please install watcher for a more performant autoreload file watcher.
)

# variables ----
verbose <- T
is_server <- Sys.info()[["sysname"]] == "Linux"
dir_private <- ifelse(
  is_server,
  "/share/private",
  "~/My Drive/private"
)
dir_data <- ifelse(
  is_server,
  "/share/data",
  "~/My Drive/projects/msens/data"
)
mapbox_tkn_txt <- glue("{dir_private}/mapbox_token_bdbest.txt")
cell_tif <- glue("{dir_data}/derived/r_bio-oracle_planarea.tif")
sdm_db <- glue("{dir_data}/derived/sdm_2026.duckdb")

Sys.setenv(MAPBOX_PUBLIC_TOKEN = readLines(mapbox_tkn_txt))
librarian::shelf(
  mapgl,
  quiet = T
)

# database ----
# source(here("../workflows/libs/db.R")) # con
con_sdm <- dbConnect(duckdb(), dbdir = sdm_db, read_only = T)

# data prep ----
r_cell <- rast(cell_tif)

# query taxon table with all dataset columns
d_spp <- tbl(con_sdm, "taxon") |>
  filter(is_ok, !is.na(mdl_seq)) |>
  select(
    taxon_id, scientific_name, common_name, sp_cat, n_ds, mdl_seq,
    `am_0.05`, ch_nmfs, ch_fws, rng_fws, bl, rng_iucn,
    worms_id, redlist_code) |>
  collect() |>
  mutate(
    common_name = case_match(
      # TODO: update common names in DB
      scientific_name,
      "Eubalaena glacialis"    ~ "North Atlantic right whale", # OLD: black right whale
      "Megaptera novaeangliae" ~ "humpback whale",             # OLD: hump
      "Balaena mysticetus"     ~ "bowhead whale",              # OLD: Arctic right whale
      .default = common_name
    ),
    lbl_cmn = ifelse(
      !is.na(common_name),
      glue(" ({common_name})", .trim = F),
      ""
    ),
    label = glue("{sp_cat}: {scientific_name}{lbl_cmn}"),
    #   key_url   = glue('<a href="https://shiny.marinesensitivity.org/mapsp/?sp_key={sp_key}" target="_blank">{sp_key}</a>'),
    worms_url = ifelse(
      is.na(worms_id),
      NA,
      glue(
        '<a href="https://www.marinespecies.org/aphia.php?p=taxdetails&id={worms_id}" target="_blank">{worms_id}</a>'
      )
    )
  ) #,
#   gbif_url  = ifelse(
#     is.na(gbif_id), NA,
#     glue('<a href="https://www.gbif.org/species/{gbif_id}" target="_blank">{gbif_id}</a>')))

spp_choices <- d_spp |>
  arrange(sp_cat, label) |>
  group_by(sp_cat) |>
  summarise(
    layer = list(setNames(mdl_seq, label)),
    .groups = "drop"
  ) |>
  deframe()

sel_sp_default <- d_spp |>
  filter(scientific_name == "Balaenoptera ricei") |>
  pull(mdl_seq)

# build reverse lookup: any mdl_seq -> (merged mdl_seq, ds_layer)
ds_cols <- c("am_0.05", "ch_nmfs", "ch_fws", "rng_fws", "bl", "rng_iucn")
mdl_seq_lookup <- d_spp |>
  select(merged_mdl_seq = mdl_seq, all_of(ds_cols)) |>
  pivot_longer(
    cols      = all_of(ds_cols),
    names_to  = "ds_layer",
    values_to = "input_mdl_seq") |>
  filter(!is.na(input_mdl_seq)) |>
  # add merged model itself as a lookup entry
  bind_rows(
    d_spp |>
      select(merged_mdl_seq = mdl_seq) |>
      mutate(ds_layer = "mdl_seq", input_mdl_seq = merged_mdl_seq) )

# ui ----
ui <- page_sidebar(
  tags$head(tags$style(HTML(
    ".mapboxgl-popup-content{color:black;}"
  ))),
  titlePanel("BOEM Marine Sensitivity - Species Distribution (v2)"),

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
      "tgl_sphere",
      "Sphere",
      T
    ),
    input_dark_mode(
      id = "tgl_dark",
      mode = "dark"
    ),
    uiOutput("species_info")
  ),
  selectizeInput(
    "sel_sp",
    "Select Species",
    choices = NULL,
    width = "100%"
  ),
  radioButtons(
    "ds_layer",
    "Display Layer",
    choices  = c(
      "Merged Model"    = "mdl_seq",
      "AquaMaps SDM"    = "am_0.05",
      "IUCN Range"      = "rng_iucn",
      "NMFS Crit. Hab." = "ch_nmfs",
      "FWS Crit. Hab."  = "ch_fws",
      "FWS Range"       = "rng_fws",
      "BirdLife"        = "bl"),
    selected = "mdl_seq",
    inline   = TRUE
  ),
  card(
    mapboxglOutput("map")
  )
)

# server ----
server <- function(input, output, session) {
  # rx_er_clr ----
  rx_er_clr <- reactiveVal(NULL)
  # rx_ds_layer: store ds_layer from URL to apply after species loads

  rx_ds_layer <- reactiveVal(NULL)

  # url parameters ----
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query$mdl_seq)) {
      url_mdl_seq <- as.integer(query$mdl_seq)

      # look up which species and layer this mdl_seq belongs to
      lookup_row <- mdl_seq_lookup |>
        filter(input_mdl_seq == url_mdl_seq)

      if (nrow(lookup_row) > 0) {
        # found: select the species (merged model) and store layer to apply later
        merged_seq <- lookup_row$merged_mdl_seq[1]
        ds_layer   <- lookup_row$ds_layer[1]

        updateSelectizeInput(
          session,
          'sel_sp',
          choices  = spp_choices,
          server   = T,
          selected = merged_seq
        )
        rx_ds_layer(ds_layer)
      } else {
        # not found in lookup, try as merged model directly
        updateSelectizeInput(
          session,
          'sel_sp',
          choices  = spp_choices,
          server   = T,
          selected = url_mdl_seq
        )
      }
    } else {
      updateSelectizeInput(
        session,
        'sel_sp',
        choices  = spp_choices,
        server   = T,
        selected = sel_sp_default
      )
    }

    # store invisible parameter (e.g., ?er_clr=test)
    if (!is.null(query$er_clr)) {
      rx_er_clr(query$er_clr)
    } else {
      rx_er_clr(NULL)
    }
  })

  # * species_info ----
  output$species_info <- renderUI({
    req(input$sel_sp)

    mdl_seq <- input$sel_sp
    d_sp <- d_spp |>
      filter(mdl_seq == !!mdl_seq)

    # model display names
    mdl_names <- c(
      "am_0.05"  = "AquaMaps SDM",
      "ch_nmfs"  = "NMFS Critical Habitat",
      "ch_fws"   = "FWS Critical Habitat",
      "rng_fws"  = "FWS Range",
      "bl"       = "BirdLife",
      "rng_iucn" = "IUCN Range")

    # determine which models are present
    has_iucn <- !is.na(d_sp$rng_iucn)
    n_ds     <- d_sp$n_ds

    # helper to create model link
    make_link <- function(ds_key) {
      ds_mdl_seq <- d_sp[[ds_key]]
      if (is.na(ds_mdl_seq)) return(NULL)
      HTML(glue('<a href="?mdl_seq={ds_mdl_seq}">{mdl_names[ds_key]}</a>'))
    }

    # value models (all except rng_iucn which is mask-only)
    ds_keys_values <- c("am_0.05", "ch_nmfs", "ch_fws", "rng_fws", "bl")
    value_models <- ds_keys_values[!is.na(sapply(ds_keys_values, function(k) d_sp[[k]]))]

    # mask models (only relevant when has_iucn)
    ds_keys_mask <- c("rng_iucn", "ch_nmfs", "ch_fws", "rng_fws")
    mask_models  <- ds_keys_mask[!is.na(sapply(ds_keys_mask, function(k) d_sp[[k]]))]

    # build values section
    if (length(value_models) == 1 && !has_iucn) {
      # single model, no merge needed
      values_ui <- tags$ul(tags$li(make_link(value_models[1])))
    } else {
      # merged model with sub-items
      merge_label <- if (has_iucn) "Merged Model (IUCN masked)" else "Merged Model"
      # merge_label <- if (has_iucn) "Merged Model"
      sub_items <- lapply(value_models, function(k) tags$li(make_link(k)))
      values_ui <- tags$ul(
        tags$li(
          HTML(glue('<a href="?mdl_seq={d_sp$mdl_seq}"><b>{merge_label}</b></a><br><em>(maximum of):</em>')),
          tags$ul(sub_items)
        )
      )
    }

    # build mask section (only if has_iucn)
    mask_ui <- if (has_iucn) {
      mask_items <- lapply(mask_models, function(k) {
        suffix <- if (k == "rng_iucn") em(" (required)") else ""
        tags$li(make_link(k), suffix)
      })
      tagList(
        span(strong("Mask"), br(), em("(to constrain extent)"),":"),
        tags$ul(mask_items)
      )
    } else NULL

    tagList(
      h5(d_sp$scientific_name),
      tags$ul(
        tags$li(glue("Common name: {d_sp$common_name}")),
        tags$li(glue("Category: {d_sp$sp_cat}")),
        tags$li(glue("IUCN RedList: {d_sp$redlist_code}")),
        tags$li(HTML(glue("WoRMS: {d_sp$worms_url}")))
      ),
      span(strong("Values"),":"),
      values_ui,
      mask_ui
    )
  })

  # * get_rast ----
  get_rast <- reactive({
    req(input$sel_sp, input$ds_layer)

    # mdl_seq = 18232 # Balaenoptera ricei
    # mdl_seq = 18513 # Haliotis cracherodii (rng_iucn)

    # get selected taxon row
    sp_row <- d_spp |> filter(mdl_seq == input$sel_sp)

    # get mdl_seq for selected layer
    layer_col     <- input$ds_layer
    layer_mdl_seq <- sp_row[[layer_col]]

    if (is.na(layer_mdl_seq)) {
      # layer not available for this species
      return(NULL)
    }

    d <- tbl(con_sdm, "model_cell") |>
      filter(mdl_seq == !!layer_mdl_seq) |>
      select(cell_id, value) |>
      collect()

    if (nrow(d) == 0) return(NULL)

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
    # input <- list(tgl_sphere = T)

    mapboxgl(
      style = mapbox_style("dark"),
      projection = ifelse(input$tgl_sphere, "globe", "mercator"),
      # projection = "globe",
      zoom = 3.5,
      center = c(-106, 40.1)
    ) |>
      add_vector_source(
        id = "er_src",
        url = "https://api.marinesensitivity.org/tilejson?table=public.ply_ecoregions_2025"
      ) |>
      # add_vector_source(
      #   id = "pa_src",
      #   url = "https://api.marinesensitivity.org/tilejson?table=public.ply_planareas_2025"
      # ) |>
      add_vector_source(
        id = "pra_src",
        url = "https://api.marinesensitivity.org/tilejson?table=public.ply_programareas_2026"
      ) |>
      # add_line_layer(
      #   id = "pa_ln",
      #   source = "pa_src",
      #   source_layer = "public.ply_planareas_2025",
      #   line_color = "white",
      #   line_opacity = 1,
      #   line_width = 1
      # ) |>
      add_line_layer(
        id = "pra_ln",
        source = "pra_src",
        source_layer = "public.ply_programareas_2026",
        line_color = "white",
        line_opacity = 1,
        line_width = 1
      ) |>
      add_line_layer(
        id = "er_ln",
        source = "er_src",
        source_layer = "public.ply_ecoregions_2025",
        line_color = "black",
        line_opacity = 1,
        line_width = 3,
        before_id = "pra_ln"
      ) |>
      add_fullscreen_control() |>
      add_navigation_control() |>
      add_scale_control() |>
      add_geocoder_control() |>
      add_globe_minimap(position = "bottom-left") |>
      add_layers_control(
        layers = list(
          "Program Area outlines" = "pra_ln",
          "Ecoregions outlines"   = "er_ln",
          "Raster cell values"    = "r_lyr"))
    # add_fill_layer(
    #   id           = "er_ply",
    #   source       = "er_src",
    #   source_layer = "public.ply_ecoregions_2025",
    #   fill_color = match_expr(
    #     column  = "ecoregion_key",
    #     values  = c("CAC", "EGOA", "NECS"),
    #     stops   = c("#ff0000", "#00ff00", "#0000ff"),
    #     default = "#cccccc"),
    #   fill_opacity = 0.5)
  })

  # * parse_er_clr helper ----
  parse_er_clr <- function(er_clr_str) {
    # Parse "CAC:red;EGOA:green;NECS:blue" into lists of values and colors
    if (is.null(er_clr_str) || er_clr_str == "") {
      return(NULL)
    }

    pairs <- strsplit(er_clr_str, ";")[[1]]
    pairs <- trimws(pairs)
    pairs <- pairs[pairs != ""]

    values <- character(0)
    colors <- character(0)

    for (pair in pairs) {
      parts <- strsplit(pair, ":")[[1]]
      if (length(parts) == 2) {
        values <- c(values, trimws(parts[1]))
        colors <- c(colors, trimws(parts[2]))
      }
    }

    if (length(values) == 0) {
      return(NULL)
    }
    list(values = values, colors = colors)
  }

  # * add_er_fill_layer helper ----
  add_er_fill_layer <- function(map_proxy, er_clr_str) {
    parsed <- parse_er_clr(er_clr_str)
    if (is.null(parsed)) {
      return(map_proxy)
    }

    map_proxy |>
      clear_layer("er_ply") |>
      add_fill_layer(
        id = "er_ply",
        source = "er_src",
        source_layer = "public.ply_ecoregions_2025",
        fill_color = match_expr(
          column = "ecoregion_key",
          values = parsed$values,
          stops = parsed$colors,
          default = "#cccccc"
        ),
        fill_opacity = 0.5,
        before_id = "pra_ln"
      )
  }

  # layer names for display
  layer_names <- c(
    "mdl_seq"  = "Merged Model",
    "am_0.05"  = "AquaMaps SDM",
    "rng_iucn" = "IUCN Range (Mask)",
    "ch_nmfs"  = "NMFS Critical Habitat (Mask)",
    "ch_fws"   = "FWS Critical Habitat (Mask)",
    "rng_fws"  = "FWS Range (Mask)",
    "bl"       = "BirdLife Range")

  # * input$sel_sp -> update layer choices ----
  observeEvent(input$sel_sp, {
    req(input$sel_sp)

    sp_row <- d_spp |> filter(mdl_seq == input$sel_sp)

    # determine which layers are available
    available <- c()
    if (!is.na(sp_row$mdl_seq))     available <- c(available, "Merged Model"    = "mdl_seq")
    if (!is.na(sp_row$`am_0.05`))   available <- c(available, "AquaMaps SDM"    = "am_0.05")
    if (!is.na(sp_row$rng_iucn))    available <- c(available, "IUCN Range"      = "rng_iucn")
    if (!is.na(sp_row$ch_nmfs))     available <- c(available, "NMFS Crit. Hab." = "ch_nmfs")
    if (!is.na(sp_row$ch_fws))      available <- c(available, "FWS Crit. Hab."  = "ch_fws")
    if (!is.na(sp_row$rng_fws))     available <- c(available, "FWS Range"       = "rng_fws")
    if (!is.na(sp_row$bl))          available <- c(available, "BirdLife"        = "bl")

    # check if URL specified a layer to select
    url_layer <- rx_ds_layer()
    if (!is.null(url_layer) && url_layer %in% available) {
      selected_layer <- url_layer
      rx_ds_layer(NULL)  # clear after use
    } else {
      selected_layer <- "mdl_seq"
    }

    updateRadioButtons(session, "ds_layer", choices = available, selected = selected_layer, inline = TRUE)
  })

  # * input$sel_sp or ds_layer -> update map ----
  observeEvent(list(input$sel_sp, input$ds_layer), {
    req(input$sel_sp, input$ds_layer)

    if (verbose) {
      message("observeEvent(input$sel_sp/ds_layer): ", input$sel_sp, " / ", input$ds_layer)
    }

    map_proxy <- mapboxgl_proxy("map")

    r <- get_rast()

    # handle case when layer not available
    if (is.null(r)) {
      map_proxy |>
        clear_layer("r_lyr") |>
        clear_layer("r_src") |>
        clear_legend()
      showNotification("Selected layer not available for this species", type = "warning")
      return()
    }

    n_cols <- 11
    cols_r <- rev(RColorBrewer::brewer.pal(n_cols, "Spectral"))
    rng_r  <- minmax(r) |> as.numeric() |> signif(digits = 3)

    # get species name and layer name for legend
    sp_name    <- get_name()
    layer_name <- layer_names[input$ds_layer]
    title_str  <- glue("{sp_name} - {layer_name}")

    # update raster
    map_proxy |>
      clear_layer("r_lyr") |>
      clear_layer("r_src") |>
      clear_legend() |>
      add_image_source(
        id     = "r_src",
        data   = r,
        colors = cols_r
      ) |>
      add_raster_layer(
        id               = "r_lyr",
        source           = "r_src",
        raster_opacity   = 0.8,
        raster_resampling = "nearest",
        before_id        = "er_ln"
      ) |>
      add_legend(
        title_str,
        values   = rng_r,
        colors   = cols_r,
        position = "bottom-right"
      ) |>
      fit_bounds(
        bbox    = trim(r) |> st_bbox() |> as.numeric(),
        animate = T
      ) |>
      clear_controls("layers") |>
      add_layers_control(
        layers = list(
          "Program Area outlines" = "pra_ln",
          "Ecoregion outlines"    = "er_ln",
          "Raster cell values"    = "r_lyr"))

    # add ecoregion fill layer if er_clr parameter was provided
    er_clr <- rx_er_clr()
    if (!is.null(er_clr)) {
      map_proxy |>
        add_er_fill_layer(er_clr)
    }
  })

  # map_click ----
  observeEvent(input$map_click, {
    click <- input$map_click

    # extract cell value at clicked location
    pt <- vect(
      data.frame(x = click$lng, y = click$lat),
      geom = c("x", "y"),
      crs = "EPSG:4326"
    ) |>
      st_as_sf() |>
      st_shift_longitude()

    r <- get_rast()
    if (!is.null(r)) {
      sp_name <- get_name()

      val <- terra::extract(r, pt) |> pull(value)
      if (!is.na(val)) {
        showNotification(
          glue(
            "{sp_name} ({round(click$lng, 3)}, {round(click$lat, 3)}): {round(val, 3)}"
          ),
          duration = 3,
          type = "message"
        )
      }
    }
  })
}

shinyApp(ui, server)
