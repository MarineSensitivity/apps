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

# version ----
v_int <- 3
v_sfx <- paste0("_v", v_int)
v_dir <- paste0("v", v_int)
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
dir_v   <- glue("{dir_data}/derived/{v_dir}")
dir_big <- ifelse(
  is_server,
  glue("/share/data/big/{v_dir}"),
  glue("~/_big/msens/derived/{v_dir}"))

mapbox_tkn_txt <- glue("{dir_private}/mapbox_token_bdbest.txt")
cell_tif <- glue("{dir_data}/derived/r_bio-oracle_planarea.tif")
mask_tif <- glue("{dir_v}/r_metrics{v_sfx}.tif")
sdm_db   <- glue("{dir_big}/sdm.duckdb")

Sys.setenv(MAPBOX_PUBLIC_TOKEN = readLines(mapbox_tkn_txt))
librarian::shelf(
  mapgl,
  quiet = T
)

# helper functions ----

#' Add raster with fixed color range to mapgl map
#'
#' Colors are drawn from a fixed scale but truncated to the data's actual range,
#' ensuring consistent color mapping across multiple rasters.
#'
#' @param map A mapgl map object
#' @param r A SpatRaster
#' @param id Layer ID
#' @param fixed_range Numeric vector c(min, max) defining the full color scale
#' @param colors Color palette vector spanning the full fixed_range
#' @param ... Additional arguments passed to add_raster_layer()
add_fixed_range_raster <- function(
  map,
  data,
  id,
  fixed_range = c(1, 100),
  colors = viridisLite::viridis(256),
  ...) {

  # Get raster data range, test if within fixed range
  dr <- terra::minmax(data) |> as.numeric()
  stopifnot(dr[1] >= fixed_range[1] & dr[2] <= fixed_range[2])

  # Expand colors to full fixed range, then truncate to data range
  n_clrs  <- diff(fixed_range) + 1
  clrs    <- colorRampPalette(colors)(n_clrs)
  clrs_dr <- clrs[seq.int(dr[1], dr[2])]

  map |>
    add_image_source(
      id     = paste0(id, "_src"),
      data   = terra::clamp(data, lower = fixed_range[1], upper = fixed_range[2], values = TRUE),
      colors = clrs_dr) |>
    add_raster_layer(
      id     = id,
      source = paste0(id, "_src"),
      ...)
}
# Usage
# maplibre() |>
#   add_fixed_range_raster(raster1, "layer1", range = c(1, 100),  colors = viridisLite::viridis(256), raster_opacity = 0.7)


# database ----
# source(here("../workflows/libs/db.R")) # con
con_sdm <- dbConnect(duckdb(), dbdir = sdm_db, read_only = T)

# data prep ----
r_cell <- rast(cell_tif)
r_masks <- list(
  programarea_key = rast(mask_tif, lyrs = "programarea_key"),
  ecoregion_key   = rast(mask_tif, lyrs = "ecoregion_key"))

# query dataset metadata once at startup
d_datasets <- tbl(con_sdm, "dataset") |>
  select(ds_key, name_display, value_info, is_mask, sort_order) |>
  collect() |>
  arrange(sort_order)

# derive what was previously hardcoded
ds_keys      <- d_datasets |> filter(!ds_key %in% c("ms_merge")) |> pull(ds_key)
layer_names  <- c(
  "mdl_seq" = "Merged Model",
  deframe(d_datasets |> filter(ds_key != "ms_merge") |> select(ds_key, name_display)))
mdl_names    <- deframe(d_datasets |> filter(ds_key != "ms_merge") |> select(ds_key, name_display))
mdl_info     <- deframe(d_datasets |> filter(!is.na(value_info)) |> select(ds_key, value_info))
ds_keys_mask <- d_datasets |> filter(is_mask) |> pull(ds_key)

# query taxon base data (no per-dataset columns)
d_spp <- tbl(con_sdm, "taxon") |>
  filter(is_ok, !is.na(mdl_seq)) |>
  select(
    taxon_id, scientific_name, common_name, sp_cat, n_ds, mdl_seq,
    worms_id, redlist_code, esa_code, esa_source,
    is_mmpa, is_mbta) |>
  collect()

# pivot taxon_model to wide format, join to taxon
d_tm <- tbl(con_sdm, "taxon_model") |>
  filter(taxon_id %in% !!d_spp$taxon_id) |>
  collect() |>
  pivot_wider(names_from = ds_key, values_from = mdl_seq)

spp_sci_cmn_fixes <- tribble(
  ~scientific_name,         ~common_name,
  "Eubalaena glacialis",    "North Atlantic right whale", # OLD: black right whale
  "Megaptera novaeangliae", "humpback whale",             # OLD: hump
  "Balaena mysticetus",     "bowhead whale"               # OLD: Arctic right whale
)

d_spp <- d_spp |>
  left_join(d_tm, by = "taxon_id") |>
  mutate(
    common_name = recode_values(
      # TODO: update common names in DB
      scientific_name,
      from = spp_sci_cmn_fixes$scientific_name,
      to   = spp_sci_cmn_fixes$common_name,
      default = common_name),
    lbl_cmn = ifelse(
      !is.na(common_name),
      glue(" ({common_name})", .trim = F),
      ""
    ),
    label = glue("{sp_cat}: {scientific_name}{lbl_cmn}"),
    worms_url = ifelse(
      is.na(worms_id),
      NA,
      glue(
        '<a href="https://www.marinespecies.org/aphia.php?p=taxdetails&id={worms_id}" target="_blank">{worms_id}</a>'
      )
    )
  )

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
mdl_seq_lookup <- d_spp |>
  select(merged_mdl_seq = mdl_seq, any_of(ds_keys)) |>
  pivot_longer(
    cols      = any_of(ds_keys),
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
  tags$head(
    tags$style(HTML("
      .mapboxgl-popup-content{color:black;}
      #ds_layer_container {display: none;}
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('updateTitle', function(title) {
        document.title = title;
      });
    "))
  ),
  titlePanel(
    glue("BOEM Marine Sensitivity (v{v_int}) species distribution")),

  sidebar = sidebar(
    open = F,
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
  fluidRow(
    column(9,
      selectizeInput(
        "sel_sp",
        "Species:",
        choices = NULL,
        width   = "100%")),
    column(3,
      selectInput(
        "sel_mask",
        "Mask:",
        choices  = c("Program Areas" = "programarea_key",
                     "Ecoregions"    = "ecoregion_key"),
        selected = "programarea_key",
        width    = "100%"))
  ),
  uiOutput("current_layer_info"),
  # hidden radioButtons to maintain ds_layer input
  div(
    id = "ds_layer_container",
    radioButtons(
      "ds_layer",
      "Display Layer",
      choices  = c(
        "Merged Model" = "mdl_seq",
        setNames(ds_keys, d_datasets$name_display[match(ds_keys, d_datasets$ds_key)])),
      selected = "mdl_seq",
      inline   = TRUE
    )
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
  # rx_marker_clicked: flag to prevent map_click from recreating marker when marker is clicked
  rx_marker_clicked <- reactiveVal(FALSE)
  # rx_url_initialized: flag to prevent re-processing URL after updateQueryString

  rx_url_initialized <- reactiveVal(FALSE)

  # url parameters ----
  observe({
    # only process URL params on initial load, not after updateQueryString
    if (rx_url_initialized()) return()

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

    # mark URL initialization complete
    rx_url_initialized(TRUE)
  })

  # * current_layer_info ----
  output$current_layer_info <- renderUI({
    req(input$sel_sp, input$ds_layer)

    sp_row     <- d_spp |> filter(mdl_seq == input$sel_sp)
    layer_name <- layer_names[input$ds_layer]
    mdl_seq    <- sp_row[[input$ds_layer]]

    tags$p(
      tags$em(glue("Displaying: {layer_name} (mdl_seq: {mdl_seq})")),
      style = "margin-top: -10px; margin-bottom: 10px; color: #888;"
    )
  })

  # * species_info ----
  output$species_info <- renderUI({
    req(input$sel_sp, input$ds_layer)

    mdl_seq <- input$sel_sp
    d_sp <- d_spp |>
      filter(mdl_seq == !!mdl_seq)

    # determine which models are present
    has_iucn <- "rng_iucn" %in% names(d_sp) && !is.na(d_sp$rng_iucn)
    n_ds     <- d_sp$n_ds

    # current layer being displayed
    current_layer <- input$ds_layer

    # helper to create model link (bold if currently displayed)
    make_link <- function(ds_key, type = "value") {
      if (!ds_key %in% names(d_sp)) return(NULL)
      ds_mdl_seq <- d_sp[[ds_key]]
      if (is.na(ds_mdl_seq)) return(NULL)
      str_info <- ifelse(
        type == "value" && !is.na(mdl_info[ds_key]),
        glue("<br><em>({mdl_info[ds_key]})</em>"),
        "")
      is_active <- (ds_key == current_layer)
      link_text <- if (is_active) {
        glue("<b>{mdl_names[ds_key]}</b>")
      } else {
        mdl_names[ds_key]
      }
      HTML(glue('<a href="?mdl_seq={ds_mdl_seq}">{link_text}</a>{str_info}'))
    }

    # value models (all non-merge datasets present for this species)
    value_models <- ds_keys[
      sapply(ds_keys, function(k) k %in% names(d_sp) && !is.na(d_sp[[k]]))]

    # mask models (only relevant when has_iucn)
    mask_models <- ds_keys_mask[
      sapply(ds_keys_mask, function(k) k %in% names(d_sp) && !is.na(d_sp[[k]]))]

    # build values section
    if (length(value_models) == 1 && !has_iucn) {
      # single model, no merge needed
      values_ui <- tags$ul(tags$li(make_link(value_models[1])))
    } else {
      # merged model with sub-items
      merge_base  <- if (has_iucn) "Merged Model (IUCN masked)" else "Merged Model"
      merge_label <- if (current_layer == "mdl_seq") glue("<b>{merge_base}</b>") else merge_base
      sub_items   <- lapply(value_models, function(k) tags$li(make_link(k)))
      values_ui   <- tags$ul(
        tags$li(
          HTML(glue('<a href="?mdl_seq={d_sp$mdl_seq}">{merge_label}</a><br><em>(maximum of):</em>')),
          tags$ul(sub_items)
        )
      )
    }

    # build mask section (only if has_iucn)
    mask_ui <- if (has_iucn) {
      mask_items <- lapply(mask_models, function(k) {
        suffix <- if (k == "rng_iucn") em(" (required)") else ""
        tags$li(make_link(k, type = "mask"), suffix)
      })
      tagList(
        span(strong("Mask"), br(), em("(to constrain extent)"),":"),
        tags$ul(mask_items)
      )
    } else NULL

    esa_str <- ifelse(
      is.na(d_sp$esa_code),
      "NA",
      glue("{d_sp$esa_code} ({d_sp$esa_source |> str_replace('ch_','') |> str_to_upper()})"))

    # protection flags (only show if TRUE)
    prot_items <- tagList(
      if (isTRUE(d_sp$is_mmpa)) tags$li("MMPA: Protected"),
      if (isTRUE(d_sp$is_mbta)) tags$li("MBTA: Protected")
    )

    tagList(
      h5(d_sp$scientific_name),
      tags$ul(
        tags$li(glue("Common name: {d_sp$common_name}")),
        tags$li(glue("Category: {d_sp$sp_cat}")),
        tags$li(glue("ESA Listing: {esa_str}")),
        tags$li(glue("IUCN RedList: {d_sp$redlist_code}")),
        tags$li(HTML(glue("WoRMS: {d_sp$worms_url}"))),
        prot_items
      ),
      span(strong("Values"),":"),
      values_ui,
      mask_ui
    )
  })

  # * get_rast ----
  get_rast <- reactive({
    req(input$sel_sp, input$ds_layer, input$sel_mask)

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
    r_mask <- r_masks[[input$sel_mask]]
    r <- mask(r, r_mask)

    # species range may fall entirely outside program areas
    if (all(is.na(values(r)))) return(NULL)

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
        url = glue("https://api.marinesensitivity.org/tilejson?table=public.ply_ecoregions_2025{v_sfx}")
      ) |>
      # add_vector_source(
      #   id = "pa_src",
      #   url = "https://api.marinesensitivity.org/tilejson?table=public.ply_planareas_2025"
      # ) |>
      add_vector_source(
        id = "pra_src",
        url = glue("https://api.marinesensitivity.org/tilejson?table=public.ply_programareas_2026{v_sfx}")
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
        source_layer = glue("public.ply_programareas_2026{v_sfx}"),
        line_color = "white",
        line_opacity = 1,
        line_width = 1
      ) |>
      add_line_layer(
        id = "er_ln",
        source = "er_src",
        source_layer = glue("public.ply_ecoregions_2025{v_sfx}"),
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
        source_layer = glue("public.ply_ecoregions_2025{v_sfx}"),
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

  # * input$sel_sp -> update layer choices ----
  observeEvent(input$sel_sp, {
    req(input$sel_sp)

    sp_row <- d_spp |> filter(mdl_seq == input$sel_sp)

    # determine which layers are available
    available <- c()
    if (!is.na(sp_row$mdl_seq)) available <- c(available, "Merged Model" = "mdl_seq")
    for (dk in ds_keys) {
      if (dk %in% names(sp_row) && !is.na(sp_row[[dk]])) {
        available <- c(available, setNames(dk, d_datasets$name_display[d_datasets$ds_key == dk]))
      }
    }

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
  observeEvent(list(input$sel_sp, input$ds_layer, input$sel_mask), {
    req(input$sel_sp, input$ds_layer)

    if (verbose) {
      message("observeEvent(input$sel_sp/ds_layer): ", input$sel_sp, " / ", input$ds_layer)
    }

    map_proxy <- mapboxgl_proxy("map")

    # clear existing marker when species/layer changes
    map_proxy |> clear_markers()

    r <- get_rast()

    # handle case when layer not available
    if (is.null(r)) {
      map_proxy |>
        clear_layer("r_lyr") |>
        clear_layer("r_lyr_src") |>
        clear_legend()
      showNotification(
        "No data to display \u2014 species may lack this layer or fall outside program areas",
        type = "warning")
      return()
    }

    n_cols <- 11
    cols_r <- rev(RColorBrewer::brewer.pal(n_cols, "Spectral"))
    # rng_r  <- minmax(r) |> as.numeric() |> signif(digits = 3)
    rng_r  <- c(1,100)

    # get species info for legend and browser title
    sp_row        <- d_spp |> filter(mdl_seq == input$sel_sp)
    sp_name       <- sp_row$scientific_name
    layer_name    <- layer_names[input$ds_layer]
    layer_mdl_seq <- sp_row[[input$ds_layer]]
    title_str     <- glue("{sp_name} - {layer_name}")

    # update browser title and URL
    sp_cat_cmn <- ifelse(
      is.na(sp_row$common_name) || sp_row$common_name == "",
      glue("{sp_row$sp_cat}"),
      glue("{sp_row$sp_cat}: {sp_row$common_name}"))
    browser_title <- glue(
      "{sp_name} distribution ({sp_cat_cmn}; mdl_seq: {layer_mdl_seq}) from {layer_name} | BOEM Marine Sensitivity")
    session$sendCustomMessage("updateTitle", browser_title)
    updateQueryString(glue("?mdl_seq={layer_mdl_seq}"), mode = "replace", session = session)

    # update raster
    map_proxy |>
      clear_layer("r_lyr") |>
      clear_layer("r_lyr_src") |>
      clear_legend() |>
      # add_image_source(
      #   id     = "r_src",
      #   data   = r,
      #   colors = cols_r
      # ) |>
      # add_raster_layer(
      #   id                = "r_lyr",
      #   source            = "r_src",
      #   raster_opacity    = 0.8,
      #   raster_resampling = "nearest",
      #   before_id         = "er_ln"
      # ) |>
      # add_fixed_range_raster() ----
      add_fixed_range_raster(
        data   = r,
        id     = "r_lyr",
        colors = cols_r,
        raster_opacity    = 0.8,
        raster_resampling = "nearest",
        before_id         = "er_ln") |>
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

    # skip if marker was clicked (allow popup to show)
    if (rx_marker_clicked()) {
      rx_marker_clicked(FALSE)
      return()
    }

    r <- get_rast()
    if (is.null(r)) return()

    # create point and handle longitude wrapping
    pt <- vect(
      data.frame(x = click$lng, y = click$lat),
      geom = c("x", "y"),
      crs = "EPSG:4326"
    ) |>
      st_as_sf() |>
      st_shift_longitude()

    # extract value and cell info
    extracted <- terra::extract(r, pt, cells = TRUE)
    if (nrow(extracted) == 0) return()

    cell_id <- extracted$cell
    val     <- extracted$value
    if (verbose)
      message("map_click: cell_id=", cell_id, ", val=", val)

    if (is.na(val) || is.na(cell_id)) return()

    # calculate background color based on value
    n_cols <- 11
    cols_r <- rev(RColorBrewer::brewer.pal(n_cols, "Spectral"))
    # rng_r  <- minmax(r) |> as.numeric()
    rng_r  <- c(1,100)

    # scale value to color index
    if (rng_r[2] > rng_r[1]) {
      val_scaled <- (val - rng_r[1]) / (rng_r[2] - rng_r[1])
    } else {
      val_scaled <- 0.5
    }
    val_scaled <- max(0, min(1, val_scaled))  # clamp to [0, 1]
    col_idx    <- round(val_scaled * (n_cols - 1)) + 1
    bg_color   <- cols_r[col_idx]

    # calculate text color for contrast (luminance-based)
    rgb_vals  <- col2rgb(bg_color)
    luminance <- (0.299 * rgb_vals[1] + 0.587 * rgb_vals[2] + 0.114 * rgb_vals[3]) / 255
    txt_color <- ifelse(luminance > 0.5, "black", "white")

    sp_name <- get_name()

    popup_html <- glue(
      '<div style="background-color: {bg_color}; color: {txt_color}; ',
      'padding: 8px; border-radius: 4px;">',
      '<b>{sp_name}</b><br>',
      'Cell ID: {cell_id}<br>',
      'Lon: {round(click$lng, 3)}<br>',
      'Lat: {round(click$lat, 3)}<br>',
      'Value: {round(val, 3)}',
      '</div>'
    )

    mapboxgl_proxy("map") |>
      clear_markers() |>
      add_markers(
        data      = c(click$lng, click$lat),
        popup     = popup_html,
        marker_id = "click_marker",
        color     = bg_color
      )
  })

  # marker_click: set flag so map_click doesn't recreate marker ----

  observeEvent(input$map_marker_click_marker, {
    # browser()
    rx_marker_clicked(TRUE)
  })
}

shinyApp(ui, server)
