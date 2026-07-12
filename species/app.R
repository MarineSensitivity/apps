# packages ----
librarian::shelf(
  bslib,
  etiennebacher/conductor,
  DBI,
  dplyr,
  duckdb,
  glue,
  here,
  htmltools,
  MarineSensitivity/msens,
  RColorBrewer,
  readr,
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
ver <- "v8"
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
dir_v   <- glue("{dir_data}/derived/{ver}")
dir_big <- ifelse(
  is_server,
  glue("/share/data/big/{ver}"),
  glue("~/_big/msens/derived/{ver}"))
is_prod <- Sys.getenv("MSENS_ENV") == "prod"
pmtiles_base_url <- ifelse(
  is_prod,
  "/pmtiles",
  "https://file.marinesensitivity.org/pmtiles")
tbl_er <- "ply_ecoregions_2025"
tbl_pra <- glue("ply_programareas_2026_{ver}")
tbl_pra_pm <- "ply_programareas_2026"

mapbox_tkn_txt <- glue("{dir_private}/mapbox_token_bdbest.txt")
cell_tif <- glue("{dir_data}/derived/r_cellid_global.tif")
mask_tif <- glue("{dir_v}/r_metrics_{ver}.tif")
pra_gpkg <- glue("{dir_v}/ply_programareas_2026_{ver}.gpkg")
sdm_db   <- { s <- glue("{dir_big}/serve.duckdb"); if (file.exists(s)) s else glue("{dir_big}/sdm.duckdb") }
# titiler-v8 serves the merged surfaces from the S3 view-DB; mtime cache-busts the tile URLs
tile_base_url <- "https://titiler-v8.marinesensitivity.org"
db_mtime <- format(file.info(sdm_db)$mtime, "%Y%m%dT%H%M%SZ", tz = "UTC")

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

# * pra_pts: program area label points (cached) ----
pra_pts_csv <- here("species/cache/pra_label_pts.csv")
if (!file.exists(pra_pts_csv)) {
  pra_pts <- read_sf(pra_gpkg) |>
    st_point_on_surface() |>   # v8 is [-180,180]; no st_shift_longitude
    select(programarea_key, programarea_name) |>
    mutate(
      lng = st_coordinates(geom)[, 1],
      lat = st_coordinates(geom)[, 2]) |>
    st_drop_geometry()
  write_csv(pra_pts, pra_pts_csv)
} else {
  pra_pts <- read_csv(pra_pts_csv)
}
pra_pts <- st_as_sf(pra_pts, coords = c("lng", "lat"), crs = 4326)

r_cell <- rast(cell_tif)
r_masks <- list(
  programarea_key = rast(mask_tif, lyrs = "programarea_key"),
  ecoregion_key   = rast(mask_tif, lyrs = "ecoregion_key"))

# * er_bbox: default extent = all ecoregions ----
# cached bbox (numeric xmin,ymin,xmax,ymax in the 0-360 longitude convention
# of r_cell / r_masks) used by the initial map render. trim() strips outer
# all-NA rows/cols so the bbox hugs actual ecoregion cells.
er_bbox_csv <- here("species/cache/ecoregions_bbox.csv")
if (!file.exists(er_bbox_csv)) {
  er_bbox <- r_masks[["ecoregion_key"]] |>
    trim() |>
    st_bbox() |>
    as.numeric()
  tibble(
    xmin = er_bbox[1], ymin = er_bbox[2],
    xmax = er_bbox[3], ymax = er_bbox[4]
  ) |>
    write_csv(er_bbox_csv)
} else {
  er_bbox <- read_csv(er_bbox_csv) |>
    as.numeric()
}

# query dataset metadata once at startup
d_datasets <- tbl(con_sdm, "dataset") |>
  select(ds_key, name_display, value_info, is_mask, sort_order) |>
  collect() |>
  arrange(sort_order)

# derive what was previously hardcoded
ds_keys      <- d_datasets |> filter(!ds_key %in% c("ms_merge")) |> pull(ds_key)
layer_names  <- c(
  "mdl_key" = "Merged Model",
  deframe(d_datasets |> filter(ds_key != "ms_merge", !is.na(name_display)) |> select(ds_key, name_display)))
mdl_names    <- deframe(d_datasets |> filter(ds_key != "ms_merge", !is.na(name_display)) |> select(ds_key, name_display))
mdl_info     <- deframe(d_datasets |> filter(!is.na(value_info)) |> select(ds_key, value_info))
ds_keys_mask <- d_datasets |> filter(is_mask) |> pull(ds_key)

# query taxon base data (v8 schema). Only the MERGED surface (`ms_merge_key`) is served via
# titiler-v8; per-dataset "original" layers await Phase 4b native publishing. Alias v8 columns
# to the names the UI/popup downstream expect.
d_spp <- tbl(con_sdm, "taxon") |>
  filter(is_valid_usa, is_marine, !is.na(ms_merge_key),
         !sp_cat %in% c("reptile", "amphibian")) |>   # not scored -> not in the picker
  select(
    taxon_id, taxon_authority, scientific_name, common_name, sp_cat,
    n_ds = n_datasets, mdl_key = ms_merge_key,
    redlist_code = iucn_code, esa_code = extrisk_code, er_score,
    rarity, is_mmpa, is_mbta) |>
  collect() |>
  mutate(
    esa_source  = NA_character_,
    lbl_cmn = ifelse(!is.na(common_name) & common_name != "",
                     glue(" ({common_name})", .trim = F), ""),
    label = glue("{sp_cat}: {scientific_name}{lbl_cmn}"),
    worms_url = ifelse(
      taxon_authority == "worms" & !is.na(taxon_id),
      glue('<a href="https://www.marinespecies.org/aphia.php?p=taxdetails&id={taxon_id}" target="_blank">{taxon_id}</a>'),
      NA_character_))

spp_choices <- d_spp |>
  arrange(sp_cat, label) |>
  group_by(sp_cat) |>
  summarise(layer = list(setNames(mdl_key, label)), .groups = "drop") |>
  deframe()

sel_sp_default <- d_spp |> filter(scientific_name == "Dermochelys coriacea") |> pull(mdl_key)
if (length(sel_sp_default) == 0) sel_sp_default <- d_spp$mdl_key[1]

# * native_asset: per-taxon input surfaces (Phase 4b) ----
# each raw input model a taxon is built from, published in a pyramided native format the
# species app overlays as its whole (often global) range: AquaMaps -> COG (titiler /cog),
# vector ranges -> PMTiles (client-side filter by mdl_key). Present once publish_native +
# release have run; absent -> graceful merged-only.
native_asset <- tryCatch(
  tbl(con_sdm, "native_asset") |> collect(),
  error = function(e) tibble(
    ms_merge_key = character(), mdl_key = character(), ds_key = character(),
    asset_type = character(), asset_url = character(), rescale_min = integer(),
    rescale_max = integer(), colormap = character(), source_layer = character(),
    xmin = double(), ymin = double(), xmax = double(), ymax = double()))

# wide per-taxon input columns: one column per ds_key holding that input's raw mdl_key (or NA),
# so `sp_row[[ds_key]]` gives the input to render for the selected taxon (v7 mapsp pattern).
d_inputs <- native_asset |>
  distinct(ms_merge_key, ds_key, mdl_key) |>
  pivot_wider(names_from = ds_key, values_from = mdl_key, values_fn = dplyr::first)
d_spp <- d_spp |> left_join(d_inputs, by = c("mdl_key" = "ms_merge_key"))
input_ds_keys <- intersect(ds_keys, names(d_inputs))   # ds_keys offered as input layers

# render lookup: raw input mdl_key -> how to draw it (asset_type/url/rescale/colormap/bbox)
native_by_key <- split(
  native_asset |> distinct(mdl_key, .keep_all = TRUE),
  (native_asset |> distinct(mdl_key, .keep_all = TRUE))$mdl_key)

# URL routing: a ?mdl_key=<key> may name the merged model OR any raw input -> resolve to
# (merged model, ds_layer) so the picker opens on that layer.
mdl_key_lookup <- bind_rows(
  d_spp   |> transmute(merged_mdl_key = mdl_key, ds_layer = "mdl_key", input_mdl_key = mdl_key),
  native_asset |> distinct(ms_merge_key, ds_key, mdl_key) |>
    transmute(merged_mdl_key = ms_merge_key, ds_layer = ds_key, input_mdl_key = mdl_key))

# ui ----
ui <- page_sidebar(
  tags$head(
    tags$link(rel = "icon", type = "image/x-icon", href = "favicon.ico"),
    tags$style(HTML("
      .mapboxgl-popup-content{color:black;}
      #ds_layer_container {display: none;}
      .header-right { margin-left: auto; display: flex; align-items: center; gap: 12px; }
      .header-right .action-button { background: none; border: none; color: inherit; cursor: pointer; text-decoration: underline; font-size: 0.9em; padding: 0; }
      .modal-footer { flex-wrap: wrap; justify-content: center; }
      .modal-footer .form-group { width: 100%; margin-bottom: 0.5rem; }

      /* layer status bar */
      .layer-bar {
        display: flex; align-items: center; gap: 6px;
        padding: 6px 12px; border-radius: 6px;
        margin-top: -8px; margin-bottom: 8px; font-size: 0.9em;
      }
      .layer-bar.is-merged { background-color: #198754; color: white; }
      .layer-bar.is-input  { background-color: #fd7e14; color: white; }
      .layer-bar .layer-icon  { font-size: 1.1em; margin-right: 2px; }
      .layer-bar .layer-label { font-weight: 600; }
      .layer-bar .layer-links {
        margin-left: auto; display: flex; gap: 4px;
        flex-wrap: wrap; align-items: center;
      }
      .layer-bar .layer-pill {
        display: inline-block; padding: 2px 10px; border-radius: 12px;
        cursor: pointer; font-size: 0.85em;
        border: 1.5px solid rgba(255,255,255,0.5);
        background: rgba(255,255,255,0.15); color: inherit;
        text-decoration: none; transition: background 0.15s, border-color 0.15s;
      }
      .layer-bar .layer-pill:hover {
        background: rgba(255,255,255,0.3); border-color: rgba(255,255,255,0.8);
      }
      .layer-bar .layer-pill.active {
        background: rgba(255,255,255,0.35); border-color: white; font-weight: 700;
      }
      .layer-bar .merged-link {
        text-decoration: underline; cursor: pointer; color: inherit; font-weight: 600;
      }
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('updateTitle', function(title) {
        document.title = title;
      });
      $(document).on('shiny:connected', function() {
        var params = new URLSearchParams(window.location.search);
        if (params.get('splash') === 'false') {
          Shiny.setInputValue('show_splash_pref', 'false');
        } else {
          var show = localStorage.getItem('msens_mapsp_show_splash');
          Shiny.setInputValue('show_splash_pref', show === null ? 'true' : show);
        }
      });
      Shiny.addCustomMessageHandler('saveSplashPref', function(val) {
        localStorage.setItem('msens_mapsp_show_splash', val);
      });
    "))
  ),
  useConductor(),
  title = div(
    style = "display: flex; align-items: center; width: 100%;",
    span(glue("BOEM Marine Sensitivity ({ver}) species distribution")),
    div(
      class = "header-right",
      actionLink("btn_about", "About"),
      input_dark_mode(id = "tgl_dark", mode = "dark")
    )
  ),

  sidebar = sidebar(
    open = F,
    input_switch(
      "tgl_sphere",
      "Sphere",
      T
    ),
    uiOutput("species_info")
  ),
  fluidRow(
    column(9,
      tags$div(
        id = "tour_sp",
        selectizeInput(
          "sel_sp",
          "Species:",
          choices = NULL,
          width   = "100%"))),
    column(3,
      tags$div(
        id = "tour_mask",
        selectInput(
          "sel_mask",
          "Outlines:",
          choices  = c("Program Areas (white)" = "programarea_key",
                       "Ecoregions (black)"    = "ecoregion_key",
                       "None"                  = "none"),
          selected = "ecoregion_key",
          width    = "100%")))
  ),
  uiOutput("layer_bar"),
  # hidden radioButtons to maintain ds_layer input
  div(
    id = "ds_layer_container",
    radioButtons(
      "ds_layer",
      "Display Layer",
      choices  = c("Merged Model" = "mdl_key"),   # v8: only the merged surface is served
      selected = "mdl_key",
      inline   = TRUE
    )
  ),
  card(
    mapboxglOutput("map")
  )
)

# server ----
server <- function(input, output, session) {
  # show_welcome helper ----
  show_welcome <- function() {
    showModal(modalDialog(
      title     = "Welcome to BOEM Marine Sensitivity",
      size      = "m",
      easyClose = TRUE,
      tags$div(
        style = "text-align: left;",
        tags$img(
          src   = "https://marinesensitivity.org/docs/figures/overview-methods.svg",
          style = "max-width: 80%; height: auto; max-height: 300px; margin-bottom: 10px;",
          alt   = "Marine Sensitivity Methods Overview"),
        tags$p(
          "Explore individual species distribution models across US waters.",
          "Search by common or scientific name and view merged model or any input distribution (range or suitability).",
          "Also see:"),
        tags$ul(
          tags$li(tags$a(
            href   = "../scores/",
            target = "_blank",
            "Composite Scores app"), " for aggregated sensitivity maps"),
          tags$li(tags$a(
            href   = "https://marinesensitivity.org/docs/",
            target = "_blank",
            "Documentation"), " for methods and data sources"))
      ),
      footer = tagList(
        checkboxInput(
          "chk_show_splash",
          tagList(
            "Show this welcome screen on startup", br(),
            helpText("Click About (upper right) to revisit this screen later")),
          value = TRUE),
        actionButton("btn_tour", "Take a Tour", icon = icon("route")),
        modalButton("Explore"))
    ))
  }

  # welcome modal on startup ----
  observe({
    if (isTRUE(input$show_splash_pref == "true"))
      show_welcome()
  }) |> bindEvent(input$show_splash_pref, once = TRUE)

  # about link ----
  observe({ show_welcome() }) |> bindEvent(input$btn_about)

  # save splash preference ----
  observe({
    session$sendCustomMessage(
      "saveSplashPref",
      tolower(as.character(input$chk_show_splash)))
  }) |> bindEvent(input$chk_show_splash)

  # conductor tour ----
  tour <- Conductor$new()$
    step(
      title    = "Select a Species",
      text     = "Search by common or scientific name. Results are grouped by category (bird, fish, mammal, etc.).",
      el       = "#tour_sp",
      position = "bottom"
    )$
    step(
      title    = "Mask Selection",
      text     = "Choose whether to overlay BOEM Program Area or Ecoregion boundaries on the map.",
      el       = "#tour_mask",
      position = "bottom"
    )$
    step(
      title    = "Layer Selector",
      text     = "The colored bar shows which model layer is displayed. Green means the final Merged Model; orange means you are viewing a single input. Click the pills to switch layers.",
      el       = ".layer-bar",
      position = "bottom"
    )$
    step(
      title    = "Species Map",
      text     = "The map shows the species distribution model. Cell values range from 1 (low sensitivity) to 100 (high sensitivity). Click cells for details.",
      el       = "#map",
      position = "top"
    )$
    step(
      title    = "Species Info",
      text     = "Open the sidebar to see ESA listing, IUCN status, MMPA/MBTA flags, and extinction risk score for the selected species.",
      el       = ".collapse-toggle",
      position = "right"
    )
  tour$init()
  if (verbose) message("conductor tour initialized")

  observe({
    if (verbose) message("starting conductor tour")
    removeModal()
    session$onFlushed(function() {
      tour$start()
      if (verbose) message("conductor tour started")
    }, once = TRUE)
  }) |>
    bindEvent(input$btn_tour)

  # rx_er_clr ----
  rx_er_clr <- reactiveVal(NULL)
  # rx_ds_layer: store ds_layer from URL to apply after species loads
  rx_ds_layer <- reactiveVal(NULL)
  # rx_marker_clicked: flag to prevent map_click from recreating marker when marker is clicked
  rx_marker_clicked <- reactiveVal(FALSE)
  # rx_url_initialized: flag to prevent re-processing URL after updateQueryString

  rx_url_initialized <- reactiveVal(FALSE)

  # v8 click-to-inspect: surfaces are titiler tiles / PMTiles, not an in-R raster, so record
  # what's currently shown (mdl_key + how it's served) and sample the value on click by
  # cell_id (merged cell-SQL) or a titiler /cog/point query (COG input).
  rx_shown <- reactiveVal(NULL)

  # url parameters ----
  observe({
    # only process URL params on initial load, not after updateQueryString
    if (rx_url_initialized()) return()

    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query$mdl_key)) {
      url_mdl_key <- query$mdl_key   # v8 mdl_key is a string (e.g. "ms_merge|WORMS:137209")

      # look up which species and layer this mdl_key belongs to
      lookup_row <- mdl_key_lookup |>
        filter(input_mdl_key == url_mdl_key)

      # check if mdl_key is a valid merged model in spp_choices
      all_mdl_keys <- unlist(spp_choices, use.names = FALSE)

      if (nrow(lookup_row) > 0) {
        # found: select the species (merged model) and store layer to apply later
        merged_seq <- lookup_row$merged_mdl_key[1]
        ds_layer   <- lookup_row$ds_layer[1]

        updateSelectizeInput(
          session,
          'sel_sp',
          choices  = spp_choices,
          server   = T,
          selected = merged_seq
        )
        rx_ds_layer(ds_layer)
      } else if (url_mdl_key %in% all_mdl_keys) {
        # valid merged model
        updateSelectizeInput(
          session,
          'sel_sp',
          choices  = spp_choices,
          server   = T,
          selected = url_mdl_key
        )
      } else {
        # model not found — show disclaimer modal
        updateSelectizeInput(
          session,
          'sel_sp',
          choices  = spp_choices,
          server   = T,
          selected = sel_sp_default
        )
        showModal(modalDialog(
          title     = "Model not found",
          size      = "m",
          easyClose = TRUE,
          tags$div(
            style = "text-align: left;",
            tags$p(
              glue("The requested model (mdl_key={url_mdl_key}) is no longer ",
                   "available. It may have been modified or removed by a newer ",
                   "version of the Marine Sensitivity Toolkit.")),
            tags$p(
              "Please search for the species using the ",
              tags$strong("Species"), " dropdown above."),
            tags$p(
              "If the species is not listed, its expert range map (IUCN Red ",
              "List) falls entirely outside the US Exclusive Economic Zone, so ",
              "it has no modeled distribution in the study area.")),
          footer = modalButton("OK")
        ))
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

  # * layer_bar ----
  output$layer_bar <- renderUI({
    req(input$sel_sp, input$ds_layer)

    sp_row        <- d_spp |> filter(mdl_key == input$sel_sp)
    current_layer <- input$ds_layer
    layer_name    <- layer_names[current_layer]
    n_ds          <- sp_row$n_ds

    # determine available layers (mirrors observeEvent input$sel_sp logic)
    available <- c()
    if (!is.na(sp_row$mdl_key))
      available <- c(available, c("Merged Model" = "mdl_key"))
    for (dk in ds_keys) {
      if (dk %in% names(sp_row) && !is.na(sp_row[[dk]]))
        available <- c(available, setNames(dk, mdl_names[dk]))
    }

    is_merged <- (current_layer == "mdl_key")

    # build pill buttons for each available layer
    pills <- lapply(seq_along(available), function(i) {
      ds_val   <- available[[i]]
      ds_label <- names(available)[[i]]
      active   <- ifelse(ds_val == current_layer, " active", "")
      onclick_js <- sprintf(
        "var r=document.querySelector('#ds_layer_container input[value=\"%s\"]'); if(r) r.click();",
        ds_val)
      tags$a(
        class   = paste0("layer-pill", active),
        onclick = onclick_js,
        ds_label)
    })

    # build left content based on merged vs input state
    if (is_merged) {
      bar_class <- "layer-bar is-merged"
      left_content <- tagList(
        span(class = "layer-icon", "\u2713"),
        span(class = "layer-label", "Merged Model"),
        if (n_ds > 1) span(
          style = "opacity: 0.85;",
          glue(" (maximum of {n_ds - 1} inputs)"))) # assume 1 merged model + n_ds-1 inputs
    } else {
      bar_class <- "layer-bar is-input"
      left_content <- tagList(
        span(class = "layer-icon", "\u25B6"),
        span(class = "layer-label",
          glue("Viewing input: {layer_name}")),
        span(" \u2014 "),
        tags$a(
          class   = "merged-link",
          onclick = "var r=document.querySelector('#ds_layer_container input[value=\"mdl_key\"]'); if(r) r.click();",
          "show Merged Model"))
    }

    div(
      class = bar_class,
      left_content,
      div(class = "layer-links", pills))
  })

  # * species_info ----
  output$species_info <- renderUI({
    req(input$sel_sp, input$ds_layer)

    mdl_key <- input$sel_sp
    d_sp <- d_spp |>
      filter(mdl_key == !!mdl_key)

    # determine which models are present
    has_iucn <- "rng_iucn" %in% names(d_sp) && !is.na(d_sp$rng_iucn)
    n_ds     <- d_sp$n_ds

    # current layer being displayed
    current_layer <- input$ds_layer

    # helper to create model link (bold if currently displayed, in-page switch)
    make_link <- function(ds_key, type = "value") {
      if (!ds_key %in% names(d_sp)) return(NULL)
      ds_mdl_key <- d_sp[[ds_key]]
      if (is.na(ds_mdl_key)) return(NULL)
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
      onclick_js <- sprintf(
        "var r=document.querySelector('#ds_layer_container input[value=&quot;%s&quot;]'); if(r) r.click(); return false;",
        ds_key)
      HTML(glue('<a href="?mdl_key={ds_mdl_key}" onclick="{onclick_js}">{link_text}</a>{str_info}'))
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
      merge_label <- if (current_layer == "mdl_key") glue("<b>{merge_base}</b>") else merge_base
      sub_items   <- lapply(value_models, function(k) tags$li(make_link(k)))
      values_ui   <- tags$ul(
        tags$li(
          HTML(glue('<a href="?mdl_key={d_sp$mdl_key}" onclick="var r=document.querySelector(\'#ds_layer_container input[value=&quot;mdl_key&quot;]\'); if(r) r.click(); return false;">{merge_label}</a><br><em>(maximum of):</em>')),
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
      if (isTRUE(d_sp$is_mmpa)) tags$li("MMPA: Protected (20)"),
      if (isTRUE(d_sp$is_mbta)) tags$li("MBTA: Protected (10)")
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

  # * sel_layer: the mdl_key for the selected species + layer (NULL if unavailable) ----
  # v8 renders via titiler-v8 XYZ tiles (not an in-R terra raster), so this returns just the
  # merged model's mdl_key; the SELECT is built + served in the render observer below.
  sel_layer <- reactive({
    req(input$sel_sp, input$ds_layer)
    sp_row        <- d_spp |> filter(mdl_key == input$sel_sp)
    layer_mdl_key <- sp_row[[input$ds_layer]]
    if (is.null(layer_mdl_key) || length(layer_mdl_key) == 0 || is.na(layer_mdl_key)) return(NULL)
    layer_mdl_key
  })

  # geographic bbox (lon/lat) of a model's cells, for fit_bounds
  mdl_bbox <- function(mdl_key) {
    b <- dbGetQuery(con_sdm, glue(
      "SELECT min(c.lon) x0, min(c.lat) y0, max(c.lon) x1, max(c.lat) y1
       FROM model_cell mc JOIN cell c USING (cell_id) WHERE mc.mdl_key = '{mdl_key}'"))
    if (is.na(b$x0)) NULL else c(b$x0, b$y0, b$x1, b$y1)
  }

  # * get_name ----
  get_name <- reactive({
    d_spp |>
      filter(mdl_key == input$sel_sp) |>
      pull(scientific_name)
  })

  # * map ----
  output$map <- renderMapboxgl({
    # input <- list(tgl_sphere = T)

    mapboxgl(
      style = mapbox_style("dark"),
      projection = ifelse(input$tgl_sphere, "globe", "mercator")
    ) |>
      fit_bounds(er_bbox) |>
      msens::add_pmline(list(
        list(url = glue("{pmtiles_base_url}/{tbl_pra_pm}.pmtiles"),
             source_layer = tbl_pra_pm, id = "pra_ln", source_id = "pra_src",
             line_color = "white", line_width = 1),
        list(url = glue("{pmtiles_base_url}/{tbl_er}.pmtiles"),
             source_layer = tbl_er, id = "er_ln", source_id = "er_src",
             line_color = "black", line_width = 3, before_id = "pra_ln"))) |>
      add_fill_layer(
        id           = "pra_hover",
        source       = "pra_src",
        source_layer = tbl_pra_pm,
        fill_opacity = 0.01,
        fill_color   = "white",
        tooltip      = get_column("programarea_name"),
        before_id    = "pra_ln") |>
      msens::add_pmlabel(list(
        list(source     = pra_pts,
             text_field = "programarea_key",
             id         = "pra_lbl"))) |>
      add_fullscreen_control() |>
      add_navigation_control() |>
      add_scale_control() |>
      add_geocoder_control() |>
      add_globe_minimap(position = "bottom-left") |>
      add_layers_control(
        layers = list(
          "Program Area outlines" = "pra_ln",
          "Program Area labels"   = "pra_lbl",
          "Ecoregions outlines"   = "er_ln"))
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
        source_layer = tbl_er,
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

    sp_row <- d_spp |> filter(mdl_key == input$sel_sp)

    # determine which layers are available
    available <- c()
    if (!is.na(sp_row$mdl_key)) available <- c(available, "Merged Model" = "mdl_key")
    for (dk in ds_keys) {
      if (dk %in% names(sp_row) && !is.na(sp_row[[dk]])) {
        available <- c(available, setNames(dk, d_datasets$name_display[d_datasets$ds_key == dk]))
      }
    }

    # check if URL specified a layer to select. keep rx_ds_layer set (cleared later, in the
    # render observer) so it can skip the stale default-layer render — avoids the
    # merged->input "jigger" when deep-linking straight to a non-merged input.
    url_layer <- rx_ds_layer()
    selected_layer <- if (!is.null(url_layer) && url_layer %in% available) url_layer else "mdl_key"

    updateRadioButtons(session, "ds_layer", choices = available, selected = selected_layer, inline = TRUE)
  })

  # * input$sel_sp or ds_layer -> update map ----
  observeEvent(list(input$sel_sp, input$ds_layer, input$sel_mask), {
    req(input$sel_sp, input$ds_layer)

    # deep-link jigger fix: while a URL-specified layer is pending (rx_ds_layer), skip any
    # render whose ds_layer hasn't caught up to it (don't flash the merged model first);
    # once ds_layer matches the target, clear the pending flag and render it directly.
    pend <- rx_ds_layer()
    if (!is.null(pend)) {
      if (!identical(input$ds_layer, pend)) return()
      rx_ds_layer(NULL)
    }

    if (verbose) {
      message("observeEvent(input$sel_sp/ds_layer): ", input$sel_sp, " / ", input$ds_layer)
    }

    map_proxy <- mapboxgl_proxy("map")

    # clear existing marker when species/layer changes
    map_proxy |> clear_markers()

    layer_mdl_key <- sel_layer()

    # handle case when the layer isn't available for this species
    if (is.null(layer_mdl_key)) {
      map_proxy |> clear_layer("r_lyr") |> clear_layer("r_src") |> clear_legend()
      showNotification("No data to display \u2014 this species lacks this layer", type = "warning")
      return()
    }

    n_cols <- 11
    cols_r <- rev(RColorBrewer::brewer.pal(n_cols, "Spectral"))
    # rng_r  <- minmax(r) |> as.numeric() |> signif(digits = 3)
    rng_r  <- c(1,100)

    # get species info for legend and browser title
    sp_row        <- d_spp |> filter(mdl_key == input$sel_sp)
    sp_name       <- sp_row$scientific_name
    layer_name    <- layer_names[input$ds_layer]
    layer_mdl_key <- sp_row[[input$ds_layer]]
    title_str     <- glue("{sp_name} - {layer_name}")

    # update browser title and URL
    sp_cat_cmn <- ifelse(
      is.na(sp_row$common_name) || sp_row$common_name == "",
      glue("{sp_row$sp_cat}"),
      glue("{sp_row$sp_cat}: {sp_row$common_name}"))
    browser_title <- glue(
      "{sp_name} distribution ({sp_cat_cmn}; mdl_key: {layer_mdl_key}) from {layer_name} | BOEM Marine Sensitivity")
    session$sendCustomMessage("updateTitle", browser_title)
    updateQueryString(glue("?mdl_key={layer_mdl_key}"), mode = "replace", session = session)

    # v8 Phase 4b: the merged model + AquaMaps inputs are raster surfaces (titiler XYZ tiles on
    # "r_lyr"); vector-range inputs are PMTiles polygons (client-filtered by mdl_key on "r_pm").
    # Merged serves the US surface via cell-SQL; native COG/PMTiles inputs show the whole global
    # range. Only one is shown at a time, so clear both first.
    is_merged <- input$ds_layer == "mdl_key"
    asset     <- if (!is_merged) native_by_key[[layer_mdl_key]] else NULL

    # fit target: merged -> US model extent; input -> its own (often global) bbox from
    # native_asset; a pmtiles input without a per-model bbox falls back to the taxon's
    # merged US extent (never the whole world, which lands on the antipode of a globe).
    fit_bbox <- if (is_merged) {
      bb <- mdl_bbox(layer_mdl_key); if (is.null(bb)) er_bbox else bb
    } else if (!is.null(asset) && !is.na(asset$xmin)) {
      c(asset$xmin, asset$ymin, asset$xmax, asset$ymax)
    } else { bb <- mdl_bbox(sp_row$mdl_key); if (is.null(bb)) er_bbox else bb }

    # clear BOTH the previous raster (r_lyr/r_src) and pmtiles (r_pm/pm_src) layer+source;
    # clear_layer removes a source of that id too, so re-adding a source doesn't collide
    # client-side (the bug that left the stale merged surface when switching to an input).
    map_proxy <- map_proxy |>
      clear_layer("r_lyr") |> clear_layer("r_src") |>
      clear_layer("r_pm")  |> clear_layer("pm_src") |> clear_legend()

    if (is_merged || (!is.null(asset) && asset$asset_type == "cog")) {
      tile_url <- if (is_merged) {
        sql <- glue("SELECT cell_id, val AS value FROM model_cell WHERE mdl_key = '{layer_mdl_key}'")
        msens::cell_tile_url(sql, colormap = "spectral_r", rescale = c(1, 100),
                             mtime = db_mtime, base = tile_base_url)
      } else {
        msens::cog_tile_url(asset$asset_url,
                            colormap = coalesce(asset$colormap, "spectral_r"),
                            rescale  = c(coalesce(asset$rescale_min, 1L), coalesce(asset$rescale_max, 100L)),
                            base = tile_base_url)
      }
      map_proxy  <- map_proxy |>
        msens::add_cell_tiles(tile_url, id = "r_lyr", raster_opacity = 0.8, before_id = "er_ln") |>
        add_legend(title_str, values = rng_r, colors = cols_r, position = "bottom-right")
      active_lyr <- c("Raster cell values" = "r_lyr")
    } else if (!is.null(asset) && asset$asset_type == "pmtiles") {
      pm_col     <- "#3388ff"
      map_proxy  <- map_proxy |>
        add_pmtiles_source(id = "pm_src", url = asset$asset_url) |>
        add_fill_layer(
          id = "r_pm", source = "pm_src", source_layer = asset$source_layer,
          filter = list("==", list("get", "mdl_key"), layer_mdl_key),
          fill_color = pm_col, fill_opacity = 0.5, before_id = "er_ln") |>
        add_categorical_legend(
          legend_title = title_str, values = "range (presence)", colors = pm_col,
          position = "bottom-right")
      active_lyr <- c("Range (presence)" = "r_pm")
    } else {
      showNotification("No native surface available for this input", type = "warning")
      active_lyr <- character(0)
    }

    # record what's shown so a map click can sample its value (merged -> cell-SQL by cell_id;
    # cog -> titiler /cog/point; pmtiles -> presence only)
    rx_shown(list(
      mdl_key = layer_mdl_key, name = sp_name,
      type    = if (is_merged) "merged" else (asset$asset_type %||% "pmtiles"),
      url     = if (!is_merged && identical(asset$asset_type, "cog")) asset$asset_url else NA_character_))

    # outline overlay: show Program Areas, Ecoregions, or None (the "Outlines:" selector)
    pa_vis <- if (input$sel_mask == "programarea_key") "visible" else "none"
    er_vis <- if (input$sel_mask == "ecoregion_key")   "visible" else "none"
    map_proxy |>
      fit_bounds(bbox = fit_bbox, animate = TRUE) |>
      set_layout_property("pra_ln",  "visibility", pa_vis) |>
      set_layout_property("pra_lbl", "visibility", pa_vis) |>
      set_layout_property("er_ln",   "visibility", er_vis) |>
      clear_controls("layers") |>
      add_layers_control(layers = c(list(
        "Program Area outlines" = "pra_ln",
        "Program Area labels"   = "pra_lbl",
        "Ecoregion outlines"    = "er_ln"), as.list(active_lyr)))

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

    shown <- rx_shown()
    if (is.null(shown)) return()
    lng <- click$lng; lat <- click$lat

    # global 0.05° cell_id (row-major, top-left origin) — matches the grid the surfaces use
    col <- floor((lng + 180) / 0.05); row <- floor((90 - lat) / 0.05)
    if (col < 0 || col >= 7200 || row < 0 || row >= 3600) return()
    cell_id <- as.integer(row) * 7200L + as.integer(col) + 1L

    # sample the value of the shown layer at this cell
    val <- if (shown$type == "merged") {
      v <- tryCatch(dbGetQuery(con_sdm, glue(
        "SELECT val FROM model_cell WHERE mdl_key = '{shown$mdl_key}' AND cell_id = {cell_id}")),
        error = function(e) NULL)
      if (!is.null(v) && nrow(v)) as.numeric(v$val[1]) else NA_real_
    } else if (identical(shown$type, "cog") && !is.na(shown$url)) {
      pv <- tryCatch(
        httr2::request(glue("{tile_base_url}/cog/point/{lng},{lat}")) |>
          httr2::req_url_query(url = shown$url) |> httr2::req_perform() |>
          httr2::resp_body_json(),
        error = function(e) NULL)
      if (!is.null(pv) && length(pv$values)) as.numeric(pv$values[[1]]) else NA_real_
    } else NA_real_   # pmtiles = presence; no per-cell value

    if (verbose) message("map_click: cell_id=", cell_id, ", val=", val)

    if (is.na(val)) {
      mapboxgl_proxy("map") |> clear_markers() |>
        add_markers(data = c(lng, lat), marker_id = "click_marker",
                    popup = glue('<div style="padding:6px;color:black;">Cell {cell_id}<br>',
                                 'Lon {round(lng,3)}, Lat {round(lat,3)}<br><i>no value here</i></div>'))
      return()
    }

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
