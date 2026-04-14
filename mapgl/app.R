# TODO:
# - [ ] on click, add marker (rast) or highlight (pa)
# - [use new features mapgl 0.3](https://walker-data.com/mapgl/news/index.html#mapgl-03)
#   - [ ] on hover, show rast value using `enable_shiny_hover()`
#   - [ ] legend pretty, more compact with `legend_style()`
# - [ ] on light theme, change basemap to light too
# - [ ] Species table: rename columns and add explanation with info popups
# - [ ] Disconnect db connections when Shiny closes
# - [ ] later manage multiple species per model; assume 1 taxa to 1 species for now
# - [ ] Generate metrics raster
# - [ ] Add Download button for cell (tif), pa (gpkg), er (gpkg) with README.md, csv lookup table as zip (ea.)

# packages ----
librarian::shelf(
  bsicons,
  bslib,
  etiennebacher / conductor,
  DBI,
  dbplyr,
  digest,
  dplyr,
  duckdb,
  DT,
  fs,
  future,
  ggiraph,
  ggplot2,
  glue,
  here,
  httr2,
  MarineSensitivity/msens,
  yogevherz / plotme,
  plotly,
  promises,
  purrr,
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
options(readr.show_col_types = F)

# async backend for background `/report` POSTs so long renders don't
# block the Shiny session. Workers are shared across sessions.
future::plan(future::multisession, workers = 2)

# profile performance of app:
#   profvis::profvis(shiny::runApp(here::here("mapgl")))

# variables ----
verbose <- interactive()

# version ----
ver <- "v6"
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
dir_v <- glue("{dir_data}/derived/{ver}")
dir_big <- ifelse(
  is_server,
  glue("/share/data/big/{ver}"),
  glue("~/_big/msens/derived/{ver}")
)
is_prod <- Sys.getenv("MSENS_ENV") == "prod"
pmtiles_base_url <- ifelse(
  is_prod,
  "/pmtiles",
  "https://file.marinesensitivity.org/pmtiles")

mapbox_tkn_txt <- glue("{dir_private}/mapbox_token_bdbest.txt")
cell_tif <- glue("{dir_data}/derived/r_bio-oracle_planarea.tif")
sdm_db <- glue("{dir_big}/sdm.duckdb")
er_gpkg <- glue("{dir_v}/ply_ecoregions_2025.gpkg")
lyrs_csv <- glue("{dir_v}/layers_{ver}.csv")
metrics_tif <- glue("{dir_v}/r_metrics_{ver}.tif")
pra_gpkg <- glue("{dir_v}/ply_programareas_2026_{ver}.gpkg")
sr_pra_csv <- glue("{dir_v}/subregion_programareas.csv")
sr_bb_csv <- here("mapgl/cache/subregion_bboxes.csv")
init_tif <- here("mapgl/cache/r_init_full.tif")
outside_pra_tif <- here("mapgl/cache/r_cells_outside_pra.tif")
taxonomy_csv <- here(
  "mapgl/data/taxonomic_hierarchy_worms_2025-10-30.csv")
tbl_er <- "ply_ecoregions_2025"
tbl_sr <- glue("ply_subregions_2026_{ver}")
tbl_pra <- glue("ply_programareas_2026_{ver}")
tbl_pra_pm <- "ply_programareas_2026"

v_required <- c(
  mapbox_tkn_txt,
  cell_tif,
  sdm_db,
  er_gpkg,
  lyrs_csv,
  metrics_tif,
  pra_gpkg,
  sr_pra_csv,
  # init_tif,
  taxonomy_csv
)
v_missing <- v_required[!file.exists(v_required)]
if (length(v_missing) > 0) {
  stop(glue(
    "Required files missing:\n  {paste(v_missing, collapse = '\n  ')}"
  ))
}

# spp_global_csv <- glue("{dir_data}/derived/spp_global_cache.csv")

if (verbose) {
  message(glue("Verbose: TRUE"))
}

# mapbox token ----
Sys.setenv(MAPBOX_PUBLIC_TOKEN = readLines(mapbox_tkn_txt))
librarian::shelf(
  mapgl
)

# database ----
con_sdm <- dbConnect(duckdb(), dbdir = sdm_db, read_only = T)
# dbListTables(con_sdm)
# duckdb_shutdown(duckdb()); rm(con_sdm)

# helper functions ----
get_rast <- function(m_key, subregion_key = "FULL") {
  # m_key         = "score_extriskspcat_primprod_ecoregionrescaled_equalweights"
  # m_key = "extrisk_mammal"; subregion_key = "FULL" (full study area)

  d_metric <- tbl(con_sdm, "metric") |> # get metric.metric_seq
    filter(metric_key == !!m_key) |> #   by input$sel_lyr
    select(metric_seq) |>
    inner_join(
      # get cell_metric.value
      tbl(con_sdm, "cell_metric") |>
        select(metric_seq, cell_id, value),
      by = join_by(metric_seq)
    ) |>
    select(cell_id, value)

  d <- if (subregion_key == "FULL") {
    # full study area: all cells with this metric
    d_metric |> collect()
  } else {
    # limit to subregion zone
    d_metric |>
      inner_join(
        tbl(con_sdm, "zone") |>
          filter(
            tbl == !!tbl_sr,
            fld == "subregion_key",
            value == !!subregion_key
          ) |>
          select(zone_seq) |>
          inner_join(
            tbl(con_sdm, "zone_cell") |>
              select(zone_seq, cell_id),
            by = join_by(zone_seq)
          ) |>
          select(cell_id),
        by = join_by(cell_id)
      ) |>
      collect()
  }

  r <- init(r_cell[[1]], NA) # plot(r)
  r[d$cell_id] <- d$value

  r <- trim(r) # plot(r)

  r
}

get_lyr_name <- function(lyr) {
  # get layer name from d_lyrs
  lyr_name <- d_lyrs |>
    filter(lyr == !!lyr) |>
    pull(layer)
  if (length(lyr_name) == 0) {
    stop(glue("Layer '{lyr}' not found in d_lyrs."))
  }
  lyr_name
}

# plot_flower, cells_in_polygon, scores_for_cells, species_for_cells:
# now provided by the msens package (loaded via librarian::shelf above).
# Polygon drawing moved from the Map tab to the Report tab; the old
# paint_drawn_polygon helper was removed along with it.

# data prep ----

# * pra_pts: program area label points (cached) ----
pra_pts_csv <- here("mapgl/cache/pra_label_pts.csv")
if (!file.exists(pra_pts_csv)) {
  pra_pts <- read_sf(pra_gpkg) |>
    st_shift_longitude() |>
    st_point_on_surface() |>
    select(programarea_key, programarea_name) |>
    mutate(
      lng = st_coordinates(geom)[, 1],
      lat = st_coordinates(geom)[, 2]) |>
    st_drop_geometry()
  write_csv(pra_pts, pra_pts_csv)
} else {
  pra_pts <- read_csv(pra_pts_csv)
}

# * pra_full_sf: full program-area polygons, used to render areas added
# on the Report tab (so the user can see what they're submitting) ----
pra_full_sf <- read_sf(pra_gpkg) |>
  select(programarea_key, programarea_name)
pra_pts <- st_as_sf(pra_pts, coords = c("lng", "lat"), crs = 4326)

# * sr_choices ----
# sr_choices <- c(
#   "All USA" = "USA",
#   "Mainland USA" = "L48",
#   "Alaska" = "AK",
#   "Mainland USA & Alaska" = "AKL48")
# TODO: version subregions
sr_choices <- c(
  "Full study area" = "FULL",
  "All USA"         = "USA",
  "Alaska"          = "AK",
  "Gulf of America" = "GA",
  "Pacific"         = "PA"
)
# TODO: add other subregions:
# - `HI`  : Hawaii
# - `HIPI`: Hawaii & Pacific Island Territories
# - `HIPI`: Pacific Island Territories
# - `PAC` : Pacific Islands & Mainland USA
# - `GOA` : Gulf of America
# - `ATL` : Mainland Atlantic
# - `ATL` : Atlantic & Gulf of America, incl. Puerto Rico

# * check cached: cell_tif, pra|er_gpkg, metrics_tif, lyrs_csv ----
# v_f <- file_exists(c(cell_tif, pa_gpkg, er_gpkg, lyrs_csv, metrics_tif))
v_f <- file_exists(c(cell_tif, pra_gpkg, er_gpkg, lyrs_csv, metrics_tif))
if (any(!v_f)) {
  stop(glue(
    "Required cached files do not exist:
       {paste(basename(names(v_f)[v_f == F]), collapse = ', ').}
     Run calc_scores.qmd, chunk: update_cached_downloads."
  ))
}

r_cell <- rast(cell_tif)

# * lyrs ----
d_lyrs <- read_csv(lyrs_csv)

# ** test lyrs (eval = F for performance) ----
if (F) {
  source(here("../workflows/libs/db.R")) # con
  # dbDisconnect(con, shutdown = T)

  # confirm all layers available for both planareas and cell metrics
  # lyrs_pa <- dbListFields(con, "ply_planareas_2025")
  lyrs_pra <- dbListFields(con, tbl_pra)
  lyrs_cell <- tbl(con_sdm, "metric") |>
    semi_join(
      tbl(con_sdm, "cell_metric") |>
        distinct(metric_seq),
      by = "metric_seq"
    ) |>
    pull(metric_key)
  # stopifnot(all(d_lyrs$lyr %in% lyrs_pa))
  stopifnot(all(d_lyrs$lyr %in% lyrs_pra))
  stopifnot(all(d_lyrs$lyr %in% lyrs_cell))
}

lyr_choices <- d_lyrs |>
  group_by(order, category) |>
  summarise(
    layer = list(setNames(lyr, layer)),
    .groups = "drop"
  ) |>
  arrange(order, layer) |>
  select(-order) |>
  deframe()

lyr_default <- d_lyrs$lyr[1]

# * planareas by subregion ---

# if (!file.exists(sr_pa_csv)) {
#   # calculate subregion - planarea cells
#   message(glue("Calculating subregion - planarea cells..."))
#
#   # subregion cells
#   tbl_sr_cell <- tbl(con_sdm, "zone") |>
#     filter(fld == "subregion_key") |>
#     select(sr_key = value, zone_seq) |>
#     inner_join(
#       tbl(con_sdm, "zone_cell") |>
#         select(zone_seq, cell_id),
#       by = join_by(zone_seq)
#     ) |>
#     select(sr_key, cell_id)
#
#   # planarea cells
#   tbl_pa_cell <- tbl(con_sdm, "zone") |>
#     filter(fld == "planarea_key") |>
#     select(pa_key = value, zone_seq) |>
#     inner_join(
#       tbl(con_sdm, "zone_cell") |>
#         select(zone_seq, cell_id),
#       by = join_by(zone_seq)
#     ) |>
#     select(pa_key, cell_id)
#
#   # planareas per subregion
#   d_sr_pa <- tbl_sr_cell |>
#     inner_join(
#       tbl_pa_cell,
#       by = join_by(cell_id)
#     ) |>
#     group_by(sr_key, pa_key) |>
#     summarise(n_cells = n(), .groups = "drop") |>
#     arrange(sr_key, pa_key) |>
#     select(
#       subregion_key = sr_key,
#       planarea_key = pa_key
#     ) |>
#     collect()
#
#   # write to csv
#   write_csv(d_sr_pa, sr_pa_csv)
# } else {
#   d_sr_pa <- read_csv(sr_pa_csv)
# }
# sr <- read_sf(sr_gpkg)

# * programareas by subregion ----

if (!file.exists(sr_pra_csv)) {
  # calculate subregion - programarea cells
  message(glue("Calculating subregion - programarea cells..."))

  # subregion cells
  tbl_sr_cell <- tbl(con_sdm, "zone") |>
    filter(
      tbl == !!tbl_sr,
      fld == "subregion_key"
    ) |>
    select(sr_key = value, zone_seq) |>
    inner_join(
      tbl(con_sdm, "zone_cell") |>
        select(zone_seq, cell_id),
      by = join_by(zone_seq)
    ) |>
    select(sr_key, cell_id)

  # programarea cells
  tbl_pra_cell <- tbl(con_sdm, "zone") |>
    filter(fld == "programarea_key") |>
    select(pra_key = value, zone_seq) |>
    inner_join(
      tbl(con_sdm, "zone_cell") |>
        select(zone_seq, cell_id),
      by = join_by(zone_seq)
    ) |>
    select(pra_key, cell_id)

  # programareas per subregion
  d_sr_pra <- tbl_sr_cell |>
    inner_join(
      tbl_pra_cell,
      by = join_by(cell_id)
    ) |>
    group_by(sr_key, pra_key) |>
    summarise(n_cells = n(), .groups = "drop") |>
    arrange(sr_key, pra_key) |>
    select(
      subregion_key = sr_key,
      programarea_key = pra_key
    ) |>
    collect()

  # write to csv
  write_csv(d_sr_pra, sr_pra_csv)
}
d_sr_pra <- read_csv(sr_pra_csv)

# * r_metrics ----
r_metrics <- rast(metrics_tif)

# * r_init (cached) ----
if (!file_exists(init_tif)) {
  # writing r_init
  r_init <- get_rast(lyr_default, subregion_key = sr_choices[[1]])

  if (verbose) {
    message(glue("Writing cached: {basename(init_tif)}"))
    # show extent of raster
    print(ext(r_init))
  }

  writeRaster(r_init, init_tif, overwrite = T)
  # plot(r_init)
}
r_init <- rast(init_tif)

# * d_sr_bb (cached) ----
get_sr_bbox <- function(sr_key) {
  # sr_key = "GA" # sr_key = "AK"
  pra_sr <- d_sr_pra |>
    filter(subregion_key == !!sr_key) |>
    pull(programarea_key)
  r <- init(r_cell[[1]], NA)
  plot(r_metrics[["programarea_key"]])
  r_sr <- r_metrics[["programarea_key"]] %in% pra_sr
  r_sr <- mask(r_sr, r_sr, maskvalues = F) |> trim()
  r_sr |> st_bbox() |> as.numeric()
}
if (!file_exists(sr_bb_csv)) {
  for (sr_key in unique(d_sr_pra$subregion_key)) {
    # sr_key = "GA"
    bbox <- get_sr_bbox(sr_key)
    d_bb_sr <- tibble(
      subregion_key = sr_key,
      xmin = bbox[1],
      ymin = bbox[2],
      xmax = bbox[3],
      ymax = bbox[4]
    )
    if (!exists("d_sr_bb")) {
      d_sr_bb <- d_bb_sr
    } else {
      d_sr_bb <- bind_rows(d_sr_bb, d_bb_sr)
    }
  }
  write_csv(d_sr_bb, sr_bb_csv)
}
d_sr_bb <- read_csv(sr_bb_csv)

# append FULL bbox = full extent of cells with metric values (in-memory only)
if (!"FULL" %in% d_sr_bb$subregion_key) {
  # st_bbox order: xmin, ymin, xmax, ymax
  bb_full <- st_bbox(r_init) |> as.numeric()
  d_sr_bb <- bind_rows(
    tibble(
      subregion_key = "FULL",
      xmin = bb_full[1], ymin = bb_full[2],
      xmax = bb_full[3], ymax = bb_full[4]),
    d_sr_bb)
}

# * r_outside_pra (cached) ----
# binary raster of cells that have metric values but lie outside any
# v6 Program Area zone — used as a semi-transparent gray overlay so the
# data-vs-program-area gap (Atlantic, Gulf of America, Hawaii, Puerto Rico,
# Pacific Island Territories) is visible.
if (!file_exists(outside_pra_tif)) {
  if (verbose) message("Computing r_cells_outside_pra.tif ...")
  pra_zone_seqs <- tbl(con_sdm, "zone") |>
    filter(fld == "programarea_key") |>
    pull(zone_seq)
  cells_in_pra <- tbl(con_sdm, "zone_cell") |>
    filter(zone_seq %in% pra_zone_seqs) |>
    distinct(cell_id) |>
    pull(cell_id)
  cells_with_metrics <- tbl(con_sdm, "cell_metric") |>
    distinct(cell_id) |>
    pull(cell_id)
  cells_outside <- setdiff(cells_with_metrics, cells_in_pra)

  r_out <- init(r_cell[[1]], NA)
  r_out[cells_outside] <- 1L
  r_out <- trim(r_out)
  writeRaster(r_out, outside_pra_tif, overwrite = TRUE, datatype = "INT1U")
}
r_outside_pra <- rast(outside_pra_tif)

# * default subregion flower-plot data (cached) ----
# Pre-compute the flower-plot tibble for each subregion zone (USA, AK, GA,
# PA) once at startup so the default Plot of Scores tab loads instantly.
# zone_metric for subregions was added by the cell_metrics_to_zone_metrics
# chunk in calc_scores.qmd; if it's missing for some reason this still
# falls back to on-the-fly aggregation across cell_metric x zone_cell.
flower_default_csv <- here("mapgl/cache/flower_default_subregions.csv")
if (!file_exists(flower_default_csv)) {
  if (verbose) message("Building flower_default_subregions cache...")
  d_flower_default <- tbl(con_sdm, "zone") |>
    filter(tbl == !!tbl_sr, fld == "subregion_key") |>
    select(zone_seq, subregion_key = value) |>
    inner_join(tbl(con_sdm, "zone_metric"), by = "zone_seq") |>
    inner_join(
      tbl(con_sdm, "metric") |>
        filter(str_detect(metric_key, ".*_ecoregion_rescaled$")) |>
        select(metric_seq, metric_key),
      by = "metric_seq") |>
    select(subregion_key, metric_key, score = value) |>
    collect() |>
    mutate(
      component = metric_key |>
        str_replace("extrisk_", "") |>
        str_replace("_ecoregion_rescaled", "") |>
        str_replace("_", " "),
      even = 1) |>
    filter(component != "all")
  if (nrow(d_flower_default) == 0) {
    warning("No subregion zone_metric rows; default flower plot will be empty. ",
            "Re-run the cell_metrics_to_zone_metrics chunk in calc_scores.qmd.")
  }
  write_csv(d_flower_default, flower_default_csv)
}
d_flower_default <- read_csv(flower_default_csv)

# * d_taxonomy ----
d_taxonomy <- read_csv(taxonomy_csv, guess_max = Inf)

# ui ----
light <- bs_theme()
# dark <- bs_theme(bg = "black", fg = "white", primary = "purple")
dark <- bs_theme()
ui <- page_sidebar(
  tags$head(
    tags$style(HTML("
      .mapboxgl-popup-content{color:black;}
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
      }
      .header-right { margin-left: auto; display: flex; align-items: center; gap: 12px; }
      .header-right .action-button { background: none; border: none; color: inherit; cursor: pointer; text-decoration: underline; font-size: 0.9em; padding: 0; }
      .modal-footer { flex-wrap: wrap; justify-content: center; }
      .modal-footer .form-group { width: 100%; margin-bottom: 0.5rem; }
    ")),
    tags$script(HTML("
      $(document).on('shiny:connected', function() {
        var show = localStorage.getItem('msens_mapgl_show_splash');
        Shiny.setInputValue('show_splash_pref', show === null ? 'true' : show);
      });
      Shiny.addCustomMessageHandler('saveSplashPref', function(val) {
        localStorage.setItem('msens_mapgl_show_splash', val);
      });

      // open a rendered-report URL in a new browser tab
      Shiny.addCustomMessageHandler('openUrl', function(url) {
        window.open(url, '_blank');
      });

      // program area tooltip lookup (updated from server)
      var praTooltips = {};
      var praPopup = null;
      var praHandlersAdded = false;
      Shiny.addCustomMessageHandler('setPraTooltips', function(data) {
        praTooltips = data;
        // add hover handlers once map is ready
        if (praHandlersAdded) return;
        var widget = HTMLWidgets.find('#map');
        if (!widget) return;
        var map = widget.getMap();
        if (!map) return;
        praHandlersAdded = true;
        praPopup = new mapboxgl.Popup({
          closeButton: false, closeOnClick: false
        });
        map.on('mousemove', 'pra_lyr', function(e) {
          if (!e.features || !e.features.length) return;
          var key = e.features[0].properties.programarea_key;
          var tip = praTooltips[key] ||
            e.features[0].properties.programarea_name || key;
          map.getCanvas().style.cursor = 'pointer';
          praPopup.setLngLat(e.lngLat).setHTML(tip).addTo(map);
        });
        map.on('mouseleave', 'pra_lyr', function() {
          map.getCanvas().style.cursor = '';
          praPopup.remove();
        });
      });
    "))
  ),
  useConductor(),
  title = div(
    style = "display: flex; align-items: center; width: 100%;",
    span(glue("BOEM Marine Sensitivity ({ver})")),
    div(
      class = "header-right",
      actionLink("btn_about", "About"),
      input_dark_mode(id = "tgl_dark", mode = "dark")
    )
  ),
  sidebar = sidebar(
    tags$div(
      id = "tour_subregion",
      selectInput(
        "sel_subregion",
        "Study area",
        choices = sr_choices
      )
    ),
    tags$div(
      id = "tour_unit",
      selectInput(
        "sel_unit",
        "Spatial units",
        choices = c(
          "Raster cells (0.05°)" = "cell",
          # "Planning areas"       = "pa",
          "Program areas" = "pra"
        )
      )
    ),
    tags$div(
      id = "tour_lyr",
      selectInput(
        "sel_lyr",
        "Layer",
        choices = lyr_choices,
        selected = lyr_default
      )
    ),
    input_switch(
      "tgl_sphere",
      "Sphere",
      T
    ),
  ),

  navset_card_tab(
    id          = "main_tabs",
    full_screen = TRUE,
    nav_panel(
      title = "Map",
      value = "Map",
      mapboxglOutput("map")
    ),
    nav_panel(
      title = "Plot of Scores",
      value = "Plot of Scores",
      card(
        full_screen = T,
        card_header(textOutput("flower_panel_title", inline = TRUE)),
        card_body(
          girafeOutput("plot_flower", height = "100%")
        )
      )
    ),
    nav_panel(
      title = "Table of Species",
      value = "Table of Species",
      card(
        card_header(
          span(
            textOutput("spp_tbl_hdr", inline = T),
            actionButton(
              "btn_tbl_info",
              "",
              icon = icon("circle-info"),
              class = "btn-sm"
            )
          ),
          class = "d-flex justify-content-between align-items-center",
          downloadButton("download_tbl", "Download CSV", class = "btn-sm")
        ),
        card_body(
          navset_card_tab(
            nav_panel(
              "Table",
              DTOutput("spp_tbl")
            ),
            nav_panel(
              "Composition",
              "NOTE: The 'bird' component has yet to be added to this visualization.",
              plotlyOutput("spp_comp")
            )
          )
        )
      )
    ),
    # Report tab ----
    # Build a list of labeled areas (drawn polygons and/or selected
    # Program Areas) and submit to the parameterized Quarto report
    # endpoint. Drawing lives here, not on the Map tab.
    nav_panel(
      title = "Report",
      value = "Report",
      layout_sidebar(
        sidebar = sidebar(
          width = 360,
          textInput(
            "rpt_title",
            "Report title",
            value = "BOEM Marine Sensitivity Report"),
          selectInput(
            "rpt_ver",
            "Data version",
            choices  = c("v6", "v5", "v4c", "v4b", "v3"),
            selected = "v6"),
          radioButtons(
            "rpt_format",
            "Output format",
            choices  = c("HTML" = "html",
                         "Word (.docx)" = "docx",
                         "PDF" = "pdf"),
            selected = "html",
            inline   = TRUE),
          hr(),
          tags$h5("Add area"),
          tags$p(class = "text-muted small",
            "Draw a polygon on the map OR click a Program Area ",
            "(set Spatial units = Program areas in the sidebar), then:"),
          textInput("rpt_area_label", "Label for next area",
                    value = "Area 1"),
          div(class = "d-flex gap-2 mb-2",
            actionButton("btn_add_drawn", "Add drawn polygon",
                         icon  = icon("plus"),
                         class = "btn-sm btn-outline-primary"),
            actionButton("btn_add_pra", "Add selected Program Area",
                         icon  = icon("plus"),
                         class = "btn-sm btn-outline-primary")),
          hr(),
          tags$h5("Areas"),
          uiOutput("rpt_areas_ui"),
          hr(),
          actionButton("btn_rpt_submit", "Generate report",
                       class = "btn-primary w-100",
                       icon  = icon("file-export"))
        ),
        mapboxglOutput("map_rpt", height = "700px")
      )
    )
  )
)

# server ----
server <- function(input, output, session) {
  # observe(session$setCurrentTheme(
  #   if (isTRUE(input$tgl_dark)) dark else light
  # ))

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
          "Explore composite sensitivity scores across US Program Areas,",
          "component scores, and species found in cells or Program Areas. Also see:"),
        tags$ul(
          tags$li(tags$a(
            href   = "../mapsp/",
            target = "_blank",
            "Species app"), " for mapping individual species distributions"),
          tags$li(tags$a(
            href   = "https://marinesensitivity.org/docs/",
            target = "_blank",
            "Documentation"), " for methods and data sources"))),
      footer = tagList(
        checkboxInput(
          "chk_show_splash",
          tagList(
            "Show this welcome screen on startup", br(),
            helpText("Click About (upper right) to revisit this screen later")),
          value = TRUE),
        actionButton("btn_tour", "Take a Tour", icon = icon("route")),
        modalButton("Explore")
      )
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
      title    = "Study Area",
      text     = "Pick a region to focus on. 'Full study area' shows all US federal waters from the Aleutians to the Caribbean and out to the Pacific Island Territories. The other choices zoom into a single subregion.",
      el       = "#tour_subregion",
      position = "right")$
    step(
      title    = "Spatial Units",
      text     = "Toggle between fine-grained raster cells (0.05\u00b0) and aggregated BOEM Program Area polygons. Cell mode lets you click any pixel; Program Area mode shows pre-aggregated zone scores.",
      el       = "#tour_unit",
      position = "right")$
    step(
      title    = "Layer Selection",
      text     = "Choose which sensitivity metric to display \u2014 composite score, individual species categories (bird, fish, mammal, etc.), or primary productivity. Note that some cells (Atlantic, Gulf of America, Hawaii, Puerto Rico, Pacific Islands) have scores but lie outside any v6 BOEM Program Area; they appear dimmed under a 'Cells outside Program Areas' overlay.",
      el       = "#tour_lyr",
      position = "right")$
    step(
      title    = "Map tab",
      text     = "The Map tab is where you explore scores spatially \u2014 click cells, click Program Areas, draw polygons.",
      el       = "[data-value='Map'].nav-link",
      position = "bottom")$
    step(
      title    = "Layers control",
      text     = "Toggle individual map layers on and off \u2014 program area outlines, ecoregion outlines, raster cells, and the gray 'Cells outside Program Areas' overlay.",
      el       = ".layers-control",
      position = "right")$
    step(
      title    = "Go to location",
      text     = "Search for a place name and the map will fly there \u2014 useful for jumping to a specific Program Area, port, or feature.",
      el       = ".mapboxgl-ctrl-geocoder",
      position = "left")$
    step(
      title    = "Draw your own area",
      text     = "Click the polygon tool, then click on the map to define vertices and double-click to finish. The Plot of Scores and Table of Species tabs will update to show results for whatever cells your polygon covers \u2014 even in the gray-overlay regions outside the Program Areas. Use the trash icon to delete the polygon and revert to the prior selection.",
      el       = ".mapbox-gl-draw_polygon",
      position = "left")$
    step(
      title    = "Full screen",
      text     = "Expand the map to fill the entire window.",
      el       = ".mapboxgl-ctrl-fullscreen",
      position = "left")$
    step(
      title    = "Zoom in / out",
      text     = "Zoom and reset the view. You can also use the mouse wheel, pinch gesture, or double-click.",
      el       = ".mapboxgl-ctrl-zoom-in",
      position = "left")$
    step(
      title    = "Plot of Scores tab",
      text     = "Switch here to see the flower plot of aggregated sensitivity scores for the current selection, broken out by species category. Petal length = score (0\u2013100); center number = weighted mean. Defaults to 'All USA' until you click a cell, click a Program Area, or draw a polygon.",
      el       = "[data-value='Plot of Scores'].nav-link",
      position = "bottom")$
    step(
      title    = "Table of Species tab",
      text     = "A sortable, downloadable table of every species in the currently selected area, with extinction-risk codes, areas, and per-category contributions.",
      el       = "[data-value='Table of Species'].nav-link",
      position = "bottom")
  tour$init()
  if (verbose) {
    message("conductor tour initialized")
  }

  observe({
    if (verbose) {
      message("starting conductor tour")
    }
    removeModal()
    session$onFlushed(
      function() {
        tour$start()
        if (verbose) message("conductor tour started")
      },
      once = TRUE
    )
  }) |>
    bindEvent(input$btn_tour)

  # reactive values ----
  rx <- reactiveValues(
    clicked_pa       = NULL,
    clicked_pra      = NULL,
    clicked_cell     = NULL,
    rpt_areas        = list(),  # Report tab: list of {label, kind, value}
    spp_tbl          = NULL,
    spp_tbl_hdr      = NULL,
    spp_tbl_filename = NULL
  )

  # dynamic title shown in the flower panel drag handle
  output$flower_panel_title <- renderText({
    if (!is.null(rx$clicked_cell)) {
      glue("Cell {rx$clicked_cell$cell_id}")
    } else if (!is.null(rx$clicked_pra)) {
      glue("{rx$clicked_pra$properties$programarea_name}")
    } else {
      sr_key <- input$sel_subregion %||% "FULL"
      sr_lbl <- if (sr_key == "FULL") "All USA" else
        names(sr_choices)[sr_choices == sr_key]
      glue("{sr_lbl} (default)")
    }
  })

  # * get_rast_rx ----
  get_rast_rx <- reactive({
    req(input$sel_subregion, input$sel_unit, input$sel_lyr)

    if (input$sel_unit %in% c("pa", "pra")) {
      return(NULL)
    }

    if (verbose) {
      message(glue("get_rast_rx() input$sel_subregion: {input$sel_subregion}"))
    }

    if (input$sel_lyr == lyr_default & input$sel_subregion == sr_choices[[1]]) {
      if (verbose) {
        message(glue("Getting cached: {basename(init_tif)}"))
      }
      r <- r_init
    } else {
      if (verbose) {
        message(glue(
          "Getting raster for layer: {input$sel_lyr} and subregion: {input$sel_subregion}"
        ))
      }
      r <- get_rast(input$sel_lyr, subregion_key = input$sel_subregion)
    }

    r
  })

  # build_initial_map ----
  # construct the initial map state (base layers, controls) shared by the
  # Map tab and the Report tab's embedded map. The Report tab chains
  # add_draw_control() after calling this.
  build_initial_map <- function(sphere = TRUE) {
    r      <- r_init
    bbox   <- st_bbox(r) |> as.numeric()
    n_cols <- 11
    cols_r <- rev(RColorBrewer::brewer.pal(n_cols, "Spectral"))
    rng_r  <- minmax(r) |> as.numeric() |> signif(digits = 3)

    mapboxgl(
      style      = mapbox_style("dark"),
      projection = ifelse(sphere, "globe", "mercator")
    ) |>
      fit_bounds(bbox) |>
      msens::add_pmline(list(
        list(url = glue("{pmtiles_base_url}/{tbl_pra_pm}.pmtiles"),
             source_layer = tbl_pra_pm, id = "pra_ln", source_id = "pra_src",
             line_color = "white", line_width = 1),
        list(url = glue("{pmtiles_base_url}/{tbl_er}.pmtiles"),
             source_layer = tbl_er, id = "er_ln", source_id = "er_src",
             line_color = "black", line_width = 3, before_id = "pra_ln"))) |>
      msens::add_pmlabel(list(
        list(source     = pra_pts,
             text_field = "programarea_key",
             id         = "pra_lbl"))) |>
      msens::add_cells(r, cols_r, raster_opacity = 0.6, before_id = "er_ln") |>
      msens::add_cells(
        r_outside_pra,
        colors         = c("#222222", "#222222"),
        id             = "outside_pra_lyr",
        source_id      = "outside_pra_lyr",
        raster_opacity = 0.55,
        before_id      = "er_ln") |>
      mapgl::add_legend(
        get_lyr_name(lyr_default),
        values   = rng_r,
        colors   = cols_r,
        position = "bottom-right") |>
      add_fullscreen_control() |>
      add_navigation_control() |>
      add_scale_control() |>
      add_layers_control(
        layers = list(
          "Program Area outlines"       = "pra_ln",
          "Program Area labels"         = "pra_lbl",
          "Ecoregions outlines"         = "er_ln",
          "Raster cell values"          = "r_lyr",
          "Cells outside Program Areas" = "outside_pra_lyr")) |>
      add_geocoder_control(placeholder = "Go to location")
  }

  # a draw_control spec the Map tab and Report tab share; only the Report
  # tab actually adds it (Task 4 removed drawing from the Map tab)
  add_msens_draw_control <- function(m) {
    m |> add_draw_control(
      position     = "top-right",
      fill_color   = "#fbb03b",
      line_color   = "#fbb03b",
      fill_opacity = 0.2,
      controls     = list(
        point              = FALSE,
        line_string        = FALSE,
        polygon            = TRUE,
        trash              = TRUE,
        combine_features   = FALSE,
        uncombine_features = FALSE))
  }

  # map ----
  output$map <- renderMapboxgl({
    build_initial_map(sphere = input$tgl_sphere)
  })

  # update map ----
  observeEvent(
    c(input$sel_subregion, input$sel_unit, input$sel_lyr),
    {
      req(input$sel_subregion, input$sel_unit, input$sel_lyr)

      # explicitly reference either select to force update
      sr_key <- input$sel_subregion
      unit <- input$sel_unit
      lyr <- input$sel_lyr

      if (unit == "cell") {
        # * cell ----

        if (verbose) {
          message(glue("update map cell - beg"))
        }

        # rx$clicked_pa <- NULL
        rx$clicked_pra <- NULL

        # show raster layer
        r <- get_rast_rx()
        n_cols <- 11
        cols_r <- rev(RColorBrewer::brewer.pal(n_cols, "Spectral"))
        rng_r <- minmax(r) |> as.numeric() |> signif(digits = 3)

        # applied to both the Map tab's proxy and the Report tab's
        # embedded map proxy so the Report map stays in sync with
        # sidebar selections.
        apply_cell_update <- function(map_proxy) {
          map_proxy |>
            clear_layer("pra_lyr") |>
            clear_layer("outside_pra_lyr") |>
            clear_layer("r_lyr") |>
            clear_layer("r_src") |>
            clear_legend() |>
            msens::add_cells(r, cols_r, raster_opacity = 0.6, before_id = "er_ln") |>
            msens::add_cells(
              r_outside_pra,
              colors         = c("#222222", "#222222"),
              id             = "outside_pra_lyr",
              source_id      = "outside_pra_lyr",
              raster_opacity = 0.55,
              before_id      = "er_ln") |>
            mapgl::add_legend(
              get_lyr_name(input$sel_lyr),
              values   = rng_r,
              colors   = cols_r,
              position = "bottom-right") |>
            mapgl::fit_bounds(
              bbox    = as.numeric(st_bbox(r)),
              animate = TRUE) |>
            clear_controls("layers") |>
            add_layers_control(
              layers = list(
                "Program Area outlines"       = "pra_ln",
                "Program Area labels"         = "pra_lbl",
                "Ecoregion outlines"          = "er_ln",
                "Raster cell values"          = "r_lyr",
                "Cells outside Program Areas" = "outside_pra_lyr"))
        }
        apply_cell_update(mapboxgl_proxy("map"))
        apply_cell_update(mapboxgl_proxy("map_rpt"))

        if (verbose) {
          message(glue("update map cell - end"))
        }
        # } else if (unit == "pa") {
        #   # * planarea ---
        #
        #   if (verbose) {
        #     message(glue("update map pa - beg"))
        #   }
        #
        #   rx$clicked_cell <- NULL
        #   rx$clicked_pra <- NULL
        #
        #   sr_bb <- sr |>
        #     filter(subregion_key == !!sr_key) |>
        #     st_shift_longitude() |>
        #     st_bbox() |>
        #     as.numeric()
        #
        #   pa_keys <- d_sr_pa |>
        #     filter(subregion_key == !!sr_key) |>
        #     pull(planarea_key)
        #   pa_filter <- c("in", "planarea_key", pa_keys)
        #   # TODO: consider filtering pa and er outlines too
        #
        #   # show planning area layer
        #   n_cols <- 11
        #   cols_r <- rev(RColorBrewer::brewer.pal(n_cols, "Spectral"))
        #
        #   # get range of planning area values
        #   rng_pa <- tbl(con_sdm, "zone") |>
        #     filter(
        #       fld == "planarea_key",
        #       value %in% pa_keys
        #     ) |>
        #     select(planarea_key = value, zone_seq) |>
        #     inner_join(tbl(con_sdm, "zone_metric"), by = join_by(zone_seq)) |> # zone_seq metric_seq  value
        #     inner_join(
        #       tbl(con_sdm, "metric") |>
        #         filter(metric_key == !!lyr),
        #       by = join_by(metric_seq)
        #     ) |>
        #     pull(value) |>
        #     range()
        #
        #   # colors
        #   cols_pa <- colorRampPalette(cols_r, space = "Lab")(n_cols)
        #   brks_pa <- seq(rng_pa[1], rng_pa[2], length.out = n_cols)
        #
        #   # remove layers if exist
        #   map_proxy |>
        #     clear_layer("r_lyr") |>
        #     clear_layer("pa_lyr") |>
        #     clear_layer("pra_lyr") |>
        #     clear_legend()
        #
        #   # add planning area fill layer
        #   map_proxy |>
        #     add_fill_layer(
        #       id = "pa_lyr",
        #       source = "pa_src",
        #       source_layer = "public.ply_planareas_2025",
        #       fill_color = interpolate(
        #         column = lyr,
        #         values = brks_pa,
        #         stops = cols_pa,
        #         na_color = "lightgrey"
        #       ),
        #       fill_outline_color = "white",
        #       tooltip = concat("Value: ", get_column(lyr)),
        #       hover_options = list(
        #         fill_color = "purple",
        #         fill_opacity = 1
        #       ),
        #       before_id = "pa_ln",
        #       filter = pa_filter
        #     ) |>
        #     mapgl::add_legend(
        #       get_lyr_name(input$sel_lyr),
        #       values = round(rng_pa, 1),
        #       colors = cols_pa,
        #       position = "bottom-right"
        #     ) |>
        #     mapgl::fit_bounds(sr_bb, animate = T)
        #   # TODO: sr_bb is odd when AK included
        #
        #   if (verbose) {
        #     message(glue("update map pa - end"))
        #   }
      } else if (unit == "pra") {
        # * programarea ----

        if (verbose) {
          message(glue("update map pra - beg"))
        }

        rx$clicked_cell <- NULL
        # rx$clicked_pa <- NULL

        sr_bb <- d_sr_bb |>
          filter(subregion_key == !!sr_key) |>
          select(xmin, ymin, xmax, ymax) |>
          as.numeric()
        if (verbose) {
          message(glue("sr_bb: {paste(round(sr_bb,2), collapse = ', ')}"))
        }

        # full study area = all program areas; otherwise filter by subregion
        pra_keys <- if (sr_key == "FULL") {
          unique(d_sr_pra$programarea_key)
        } else {
          d_sr_pra |>
            filter(subregion_key == !!sr_key) |>
            pull(programarea_key)
        }
        pra_filter <- c("in", "programarea_key", pra_keys)

        # query program area values from db
        n_cols <- 11
        cols_r <- rev(RColorBrewer::brewer.pal(n_cols, "Spectral"))

        d_pra <- tbl(con_sdm, "zone") |>
          filter(
            fld == "programarea_key",
            value %in% pra_keys) |>
          select(programarea_key = value, zone_seq) |>
          inner_join(tbl(con_sdm, "zone_metric"), by = join_by(zone_seq)) |>
          inner_join(
            tbl(con_sdm, "metric") |>
              filter(metric_key == !!lyr),
            by = join_by(metric_seq)) |>
          select(programarea_key, value) |>
          collect()

        rng_pra <- range(d_pra$value)
        cols_pra <- colorRampPalette(cols_r, space = "Lab")(n_cols)

        # assign color per program area by scaling value to color index
        d_pra <- d_pra |>
          mutate(
            val_scaled = (value - rng_pra[1]) / max(rng_pra[2] - rng_pra[1], 1e-6),
            col_idx    = pmin(pmax(round(val_scaled * (n_cols - 1)) + 1, 1), n_cols),
            fill_color = cols_pra[col_idx])

        # build tooltip lookup and send to client
        pra_tooltip <- d_pra |>
          left_join(
            pra_pts |> st_drop_geometry() |> select(programarea_key, programarea_name),
            by = "programarea_key") |>
          mutate(tip = glue("{programarea_name}: {round(value)}")) |>
          select(programarea_key, tip) |>
          deframe() |>
          as.list()
        session$sendCustomMessage("setPraTooltips", pra_tooltip)

        # applied to both the Map tab's proxy and the Report tab's
        # embedded map proxy so the Report map shows Program Areas too.
        apply_pra_update <- function(map_proxy) {
          map_proxy |>
            clear_layer("r_lyr") |>
            clear_layer("r_src") |>
            clear_layer("pra_lyr") |>
            clear_layer("outside_pra_lyr") |>
            clear_legend() |>
            add_fill_layer(
              id           = "pra_lyr",
              source       = "pra_src",
              source_layer = tbl_pra_pm,
              fill_color   = match_expr(
                column  = "programarea_key",
                values  = d_pra$programarea_key,
                stops   = d_pra$fill_color,
                default = "lightgrey"),
              fill_opacity       = 0.7,
              fill_outline_color = "white",
              hover_options      = list(
                fill_color   = "purple",
                fill_opacity = 1),
              before_id = "pra_ln",
              filter    = pra_filter) |>
            msens::add_cells(
              r_outside_pra,
              colors         = c("#222222", "#222222"),
              id             = "outside_pra_lyr",
              source_id      = "outside_pra_lyr",
              raster_opacity = 0.55,
              before_id      = "er_ln") |>
            mapgl::add_legend(
              get_lyr_name(input$sel_lyr),
              values   = round(rng_pra, 1),
              colors   = cols_pra,
              position = "bottom-right") |>
            mapgl::fit_bounds(sr_bb, animate = TRUE) |>
            clear_controls("layers") |>
            add_layers_control(
              layers = list(
                "Program Area outlines"       = "pra_ln",
                "Program Area labels"         = "pra_lbl",
                "Ecoregion outlines"          = "er_ln",
                "Program Area values"         = "pra_lyr",
                "Cells outside Program Areas" = "outside_pra_lyr"))
        }
        apply_pra_update(mapboxgl_proxy("map"))
        apply_pra_update(mapboxgl_proxy("map_rpt"))

        if (verbose) {
          message(glue("update map pra - end"))
        }
      }
    },
    ignoreInit = FALSE
  )

  # map_click ----
  observeEvent(input$map_click, {
    req(input$map_click)

    click <- input$map_click

    if (input$sel_unit == "cell") {
      rx$clicked_pa <- NULL

      # handle raster click
      lng <- click$lng
      lat <- click$lat

      # extract cell value at clicked location
      pt <- vect(
        data.frame(x = lng, y = lat),
        geom = c("x", "y"),
        crs = "EPSG:4326"
      ) |>
        st_as_sf() |>
        st_shift_longitude() # [-180,180] -> [0,360]
      cell_id <- terra::extract(r_cell$cell_id, pt) |> pull(cell_id)

      if (!is.na(cell_id)) {
        rx$clicked_cell <- list(
          lng = lng,
          lat = lat,
          cell_id = cell_id,
          lyr = input$sel_lyr
        )
      }
    } else if (input$sel_unit == "pa") {
      # handle planning area click
      rx$clicked_cell <- NULL
      rx$clicked_pra <- NULL

      if (!is.null(input$map_feature_click)) {
        rx$clicked_pa <- list(
          id = input$map_feature_click$id,
          properties = input$map_feature_click$properties
        )
      }
    } else if (input$sel_unit == "pra") {
      # handle program area click
      rx$clicked_cell <- NULL
      rx$clicked_pa <- NULL

      if (!is.null(input$map_feature_click)) {
        rx$clicked_pra <- list(
          id = input$map_feature_click$id,
          properties = input$map_feature_click$properties
        )
      }
    }
  })

  # (drawn-polygon reactive removed — drawing lives on the Report tab now)

  # plot_flower ----
  output$plot_flower <- renderGirafe({
    # set height based on container size
    height <- "100%"

    if (input$sel_unit == "cell" && !is.null(rx$clicked_cell)) {
      # get data for cell
      cell_id <- rx$clicked_cell$cell_id
      lng <- rx$clicked_cell$lng |> round(3)
      lat <- rx$clicked_cell$lat |> round(3)

      if (verbose) {
        message(glue("Rendering flower plot for cell id: {cell_id}"))
      }

      # get species group scores for the cell
      d_fl <- tbl(con_sdm, "metric") |>
        filter(str_detect(metric_key, ".*_ecoregion_rescaled$")) |>
        left_join(
          tbl(con_sdm, "cell_metric"),
          by = "metric_seq"
        ) |>
        filter(cell_id == !!cell_id) |>
        select(metric_key, score = value) |>
        mutate(
          component = metric_key |>
            str_replace("extrisk_", "") |>
            str_replace("_ecoregion_rescaled", "") |>
            str_replace("_", " "),
          even = 1
        ) |>
        filter(component != "all") |>
        collect()

      if (nrow(d_fl) > 0) {
        return(
          d_fl |>
            plot_flower(
              fld_category = component,
              fld_height = score,
              fld_width = even,
              tooltip_expr = "{component}: {round(score, 2)}",
              title = glue("Cell ID: {cell_id} (x: {lng}, y: {lat})")
            ))
      }
      # } else if (input$sel_unit == "pa" && !is.null(rx$clicked_pa)) {
      #   # get data for planning area
      #   pa_name <- rx$clicked_pa$properties$planarea_name
      #   pa_key <- rx$clicked_pa$properties$planarea_key
      #
      #   l <- rx$clicked_pa$properties
      #   l <- l[str_detect(names(l), "_ecoregion_rescaled$")]
      #
      #   d_fl <- tibble(
      #     metric_key = names(l),
      #     score = unlist(l)
      #   ) |>
      #     mutate(
      #       component = metric_key |>
      #         str_replace("extrisk_", "") |>
      #         str_replace("_ecoregion_rescaled", "") |>
      #         str_replace("_", " "),
      #       even = 1
      #     ) |>
      #     filter(component != "all")
      #
      #   if (nrow(d_fl) > 0) {
      #     d_fl |>
      #       plot_flower(
      #         fld_category = component,
      #         fld_height = score,
      #         fld_width = even,
      #         tooltip_expr = "{component}: {round(score, 2)}",
      #         title = pa_name
      #       )
      #   }
    } else if (input$sel_unit == "pra" && !is.null(rx$clicked_pra)) {
      # get data for program area from database
      pra_name <- rx$clicked_pra$properties$programarea_name
      pra_key  <- rx$clicked_pra$properties$programarea_key

      if (verbose) {
        message(glue("Rendering flower plot for Program Area: {pra_name} ({pra_key})"))
      }

      # look up zone_seq for this program area
      z_seq <- tbl(con_sdm, "zone") |>
        filter(tbl == !!tbl_pra, value == !!pra_key) |>
        pull(zone_seq)

      if (length(z_seq) > 0) {
        d_fl <- tbl(con_sdm, "metric") |>
          filter(str_detect(metric_key, ".*_ecoregion_rescaled$")) |>
          left_join(
            tbl(con_sdm, "zone_metric"),
            by = "metric_seq") |>
          filter(zone_seq == !!z_seq) |>
          select(metric_key, score = value) |>
          mutate(
            component = metric_key |>
              str_replace("extrisk_", "") |>
              str_replace("_ecoregion_rescaled", "") |>
              str_replace("_", " "),
            even = 1) |>
          filter(component != "all") |>
          collect()

        if (nrow(d_fl) > 0) {
          return(
            d_fl |>
              plot_flower(
                fld_category = component,
                fld_height   = score,
                fld_width    = even,
                tooltip_expr = "{component}: {round(score, 2)}",
                title        = pra_name))
        }
      }
    }

    # ** subregion default ----
    # nothing clicked: read pre-cached flower data for the current subregion
    # zone (FULL falls back to USA). The cache is built at app startup from
    # zone_metric, populated by cell_metrics_to_zone_metrics in calc_scores.qmd.
    if (is.null(rx$clicked_cell) && is.null(rx$clicked_pra)) {
      sr_key   <- input$sel_subregion %||% "FULL"
      z_sr_key <- if (sr_key == "FULL") "USA" else sr_key
      sr_lbl   <- if (sr_key == "FULL") "All USA" else
        names(sr_choices)[sr_choices == sr_key]
      d_fl <- d_flower_default |>
        filter(subregion_key == !!z_sr_key) |>
        select(component, score, even)
      if (nrow(d_fl) > 0) {
        return(
          d_fl |>
            plot_flower(
              fld_category = component,
              fld_height   = score,
              fld_width    = even,
              tooltip_expr = "{component}: {round(score, 2)}",
              title        = sr_lbl))
      }
    }
  })

  # click_info ----
  output$click_info <- renderPrint({
    if (input$sel_unit == "cell" && !is.null(rx$clicked_cell)) {
      cat(
        "Location:",
        round(rx$clicked_cell$lng, 4),
        ",",
        round(rx$clicked_cell$lat, 4)
      )
    } else if (input$sel_unit == "pa" && !is.null(rx$clicked_pa)) {
      cat("Planning Area:", rx$clicked_pa$feature$properties$planarea_name)
    } else if (input$sel_unit == "pra" && !is.null(rx$clicked_pra)) {
      cat("Program Area:", rx$clicked_pra$feature$properties$programarea_name)
    }
  })

  # spp_tbl ----

  # * spp_tbl_hdr ----
  output$spp_tbl_hdr <- renderText({
    req(rx$spp_tbl_hdr)
    rx$spp_tbl_hdr
  })

  # * btn_tbl_info ----
  observe({
    showModal(modalDialog(
      title = "Species table information",
      size = "l",
      easy_close = T,
      helpText(markdown(
        "Species are listed for the entire USA waters, or the currently
          selected area — a clicked Planning Area or cell. The columns correspond to:
          - `cat` species categorical component; one of: bird, coral, fish, invertebrate, mammal, reptile, other
          - `taxon` taxonomic identifier from Birds of the World (botw) or World Registry of Marine Species (worms) for non-bird species
          - `scientific` scientific name
          - `common` common name, if available
          - `er_code` extinction risk code with authority prefix: `NMFS` (National Marine Fisheries Service), `FWS` (Fish & Wildlife Service), or `IUCN` (International Union for Conservation of Nature), followed by status code — CR (Critically Endangered), EN (Endangered), VU (Vulnerable), TN (Threatened), NT (Near Threatened), LC (Least Concern), Data Deficient (DD). US national listings (NMFS, FWS) take precedence over IUCN.
          - `er_score` extinction risk score (1-100%): derived from the max of extinction risk codes (NMFS|FWS:EN=100, NMFS|FWS:TN=50, IUCN:CR=50, IUCN:EN=25, IUCN:VU=5, IUCN:NT=2, IUCN:LC|DD=1) and if protected under MMPA (20) or MBTA (10).
          - `is_mmpa` whether the species is protected under the Marine Mammal Protection Act (MMPA)
          - `is_mbta` whether the species is protected under the Migratory Bird Treaty Act (MBTA)
          - `model` model identifier; click to visit species distribution in seperate app
          - `area_km2` area of cells with non-zero value for distribution, within selected area (USA, Program Area or Cell)
          - `avg_suit` average suitability across all non-zero cells, ranging from 1 to 100%
          - `pct_cat` percent contribution of the species (`er_score * avg_suit * area_km2`) towards the total summed category (`cat`) within selected area (USA, Program Area or Cell).
             Note rescaling by Ecoregion min/max that contributes to the component and overall scores is not captured by this simpler metric."
      ))
    ))
  }) |>
    bindEvent(input$btn_tbl_info)

  # * get_spp_tbl ----
  get_spp_tbl <- reactive({
    if (is.null(rx$clicked_cell) && is.null(rx$clicked_pra)) {
      # ** subregion default ----
      # FULL falls back to USA (the only superset zone in zone_taxon)
      sr_key   <- input$sel_subregion %||% "FULL"
      z_sr_key <- if (sr_key == "FULL") "USA" else sr_key
      sr_lbl   <- if (sr_key == "FULL") "All USA" else
        names(sr_choices)[sr_choices == sr_key]

      if (verbose) {
        message(glue(
          "Getting species table for subregion: {sr_lbl} ({z_sr_key})"))
      }

      rx$spp_tbl_hdr      <- glue("Species in {sr_lbl}")
      rx$spp_tbl_filename <- glue("species_{z_sr_key}")

      d_spp <- tbl(con_sdm, "zone_taxon") |>
        select(-is_mmpa, -is_mbta) |>
        filter(
          zone_tbl   == !!tbl_sr,
          zone_fld   == "subregion_key",
          zone_value == !!z_sr_key
        ) |>
        inner_join(
          tbl(con_sdm, "taxon") |>
            filter(is_ok, !is.na(mdl_seq)) |>
            select(taxon_id, is_mmpa, is_mbta),
          by = "taxon_id"
        ) |>
        collect() |>
        rename(er_code = rl_code) |>
        mutate(er_score = er_score / 100)
    }

    # ** cell ----
    if (input$sel_unit == "cell" && !is.null(rx$clicked_cell)) {
      # see original: https://github.com/MarineSensitivity/workflows/blob/76d711aed5ea319bde44158efade00a02c1031e4/ingest_aquamaps_to_sdm_duckdb.qmd#L1817-L1954

      cell_id <- rx$clicked_cell$cell_id

      if (verbose) {
        message(glue("Getting species table for cell id: {cell_id}"))
      }

      # cell_id <- 4151839
      rx$spp_tbl_hdr <- glue("Species for Cell ID: {cell_id}")
      rx$spp_tbl_filename <- glue("species_cellid-{cell_id}")

      tbl_taxon <- tbl(con_sdm, "taxon") |>
        filter(is_ok) |>
        select(
          sp_cat,
          sp_common = common_name,
          sp_scientific = scientific_name,
          taxon_id,
          taxon_authority,
          er_code = extrisk_code,
          er_score,
          is_mmpa,
          is_mbta,
          mdl_seq
        ) |>
        mutate(er_score = er_score / 100)

      d_spp <- tbl(con_sdm, "model_cell") |>
        filter(cell_id == !!cell_id) |>
        inner_join(
          tbl_taxon,
          by = join_by(mdl_seq)
        ) |>
        inner_join(
          tbl(con_sdm, "cell") |>
            select(cell_id, area_km2),
          by = join_by(cell_id)
        ) |>
        group_by(
          mdl_seq,
          sp_cat,
          sp_common,
          sp_scientific,
          taxon_id,
          taxon_authority,
          er_code,
          er_score,
          is_mmpa,
          is_mbta
        ) |>
        summarize(
          area_km2 = sum(area_km2, na.rm = T),
          avg_suit = mean(value, na.rm = T) / 100,
          .groups = "drop"
        ) |>
        collect() |>
        mutate(
          suit_er = avg_suit * er_score,
          suit_er_area = avg_suit * er_score * area_km2
        ) |>
        group_by(sp_cat) |>
        mutate(
          cat_suit_er_area = sum(suit_er_area, na.rm = T)
        ) |>
        ungroup() |>
        mutate(
          pct_cat = suit_er_area / cat_suit_er_area
        )
    }

    # ** pa ---
    # if (input$sel_unit == "pa" && !is.null(rx$clicked_pa)) {
    #   pa_key <- rx$clicked_pa$properties$planarea_key
    #   pa_name <- rx$clicked_pa$properties$planarea_name
    #
    #   rx$spp_tbl_hdr <- glue("Species for Planning Area: {pa_name}")
    #   rx$spp_tbl_filename <- glue(
    #     "species_planarea-{str_replace(pa_name, ' ', '-') |> str_to_lower()}"
    #   )
    #
    #   # debug #  pa_key = "CGA"; pa_name = "Central Gulf of America"
    #
    #   # model stats in given zone
    #   # ℹ In argument: `value == "WAO"`
    #   # Caused by error:
    #   #   ! Object `value` not found.
    #
    #   d_spp <- tbl(con_sdm, "zone_taxon") |>
    #     filter(
    #       zone_fld == "planarea_key",
    #       zone_value == !!pa_key
    #     ) |>
    #     collect()
    # }

    # ** pra ----
    if (input$sel_unit == "pra" && !is.null(rx$clicked_pra)) {
      pra_key <- rx$clicked_pra$properties$programarea_key
      pra_name <- rx$clicked_pra$properties$programarea_name

      if (verbose) {
        message(glue("Getting species table for Program Area: {pra_name}"))
      }

      rx$spp_tbl_hdr <- glue("Species for Program Area: {pra_name}")
      rx$spp_tbl_filename <- glue(
        "species_programarea-{str_replace(pra_name, ' ', '-') |> str_to_lower()}"
      )

      d_spp <- tbl(con_sdm, "zone_taxon") |>
        select(-is_mmpa, -is_mbta) |>
        filter(
          zone_fld == "programarea_key",
          zone_value == !!pra_key
        ) |>
        left_join(
          tbl(con_sdm, "taxon") |>
            select(taxon_id, is_mmpa, is_mbta),
          by = "taxon_id"
        ) |>
        collect() |>
        rename(er_code = rl_code) |>
        mutate(er_score = er_score / 100)
    }

    # rename columns
    d_spp |>
      mutate(
        model_url = glue(
          "../mapsp/?mdl_seq={mdl_seq}"
        ),
        taxon_str = glue("{taxon_authority}:{taxon_id}"),
        taxon_url = ifelse(
          taxon_authority == "botw",
          "https://birdsoftheworld.org",
          glue(
            "https://www.marinespecies.org/aphia.php?p=taxdetails&id={taxon_id}"
          )
        )
      ) |>
      # TODO: construct URL
      # Search: "Limosa lapponica" taxon_id: 22693158
      # at https://birdsoftheworld.org/
      # https://birdsoftheworld.org/bow/api/v1/taxa?limit=100&q=Limosa%20lapponica
      # [{  "code": "batgod",
      #     "name": "Bar-tailed Godwit - Limosa lapponica",
      #     "order": 5973  }]
      # https://birdsoftheworld.org/bow/species/batgod/cur/introduction
      # https://www.iucnredlist.org/species/22693158/111221714
      # https://www.iucnredlist.org/species/22693158 taxon_id alone doesn't work
      select(
        component = sp_cat,
        taxon_authority,
        taxon_id,
        taxon_str,
        taxon_url,
        scientific = sp_scientific,
        common = sp_common,
        er_code,
        er_score,
        is_mmpa,
        is_mbta,
        model_id = mdl_seq,
        model_url,
        area_km2,
        avg_suit,
        pct_component = pct_cat
      ) |>
      arrange(component, scientific)
  })

  # * spp_tbl ----
  output$spp_tbl <- renderDT(
    {
      d <- get_spp_tbl()

      # store for download
      rx$spp_tbl <- d

      d |>
        mutate(
          taxon = glue('<a href="{taxon_url}" target="_blank">{taxon_str}</a>'),
          model = glue('<a href="{model_url}" target="_blank">{model_id}</a>'),
        ) |>
        relocate(taxon, .after = component) |>
        relocate(model, .after = er_score) |>
        select(
          -taxon_id,
          -taxon_authority,
          -taxon_str,
          -taxon_url,
          -model_url,
          -model_id
        ) |>
        rename(cat = component, pct_cat = pct_component) |>
        datatable(
          escape = F,
          rownames = F,
          fillContainer = T,
          filter = "top",
          # style         = "auto",  # "bootstrap4",
          # style         = "bootstrap4",  # "auto",
          class = "display compact",
          extensions = c("ColReorder", "KeyTable", "Responsive"),
          options = list(
            colReorder = T,
            keys = T,
            pageLength = 5,
            lengthMenu = c(5, 50, 100),
            scrollX = TRUE, # scrollY    = "600px",
            dom = 'lfrtip'
          )
        ) |>
        formatPercentage(
          c(
            "er_score"
          ),
          0
        ) |>
        formatPercentage(
          c(
            "avg_suit",
            "pct_cat"
          ),
          2
        ) |>
        formatSignif(c("area_km2"), 4)
    },
    server = T
  )

  # * download_tbl ----
  output$download_tbl <- downloadHandler(
    filename = function() {
      rx$spp_tbl_filename |>
        paste0("_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(rx$spp_tbl_hdr, rx$spp_tbl)
      write_csv(rx$spp_tbl, file)
    }
  )

  # * spp_comp ----
  output$spp_comp <- renderPlotly({
    # TODO:
    # - [ ] birds
    # - [ ] ranks_with_variety
    d <- get_spp_tbl() |>
      filter(taxon_authority == "worms") |>
      inner_join(
        d_taxonomy |>
          select(-component),
        by = join_by(taxon_id == species_id)
      ) |>
      mutate(
        name = glue("{scientific} ({common}; worms:{taxon_id})"),
        n = 1
      ) |>
      select(component, Kingdom, Phylum, Class, Order, Family, Genus, name, n)

    p <- count_to_treemap(d)

    theme <- bs_current_theme()
    if (input$tgl_dark == "dark") {
      bg <- bs_get_variables(theme, "body-bg-dark")[["body-bg-dark"]]
      fg <- bs_get_variables(theme, "body-color-dark")[["body-color-dark"]]
    } else {
      bg <- bs_get_variables(theme, "body-bg")[["body-bg"]]
      fg <- bs_get_variables(theme, "body-color")[["body-color"]]
    }

    p |>
      layout(
        font = list(
          color = fg
        ),
        plot_bgcolor = bg,
        paper_bgcolor = bg
      )
  })

  # Report tab ----

  # * map_rpt: embedded map with draw control ----
  # reuses build_initial_map() from the Map tab so layer/subregion/sphere
  # toggles in the sidebar apply here too; add_msens_draw_control() is
  # chained on top.
  output$map_rpt <- renderMapboxgl({
    build_initial_map(sphere = input$tgl_sphere) |>
      add_msens_draw_control()
  })

  # * rpt_drawn_sf: most-recently-drawn polygon on map_rpt ----
  # `input$map_rpt_drawn_features` is a GeoJSON string (see mapgl JS:
  # updateDrawnFeatures — Shiny.setInputValue(..., JSON.stringify(fc))),
  # so parse it directly rather than round-tripping through the proxy.
  rpt_drawn_sf <- reactive({
    fc_json <- input$map_rpt_drawn_features
    req(fc_json, is.character(fc_json), nzchar(fc_json))
    sf_feats <- tryCatch(
      sf::read_sf(fc_json, quiet = TRUE),
      error = function(e) NULL)
    req(sf_feats, nrow(sf_feats) > 0)
    sf_feats[nrow(sf_feats), ]
  })

  # * rpt_areas_sf: sf of all currently-added Report areas ----
  # rebuilt whenever rx$rpt_areas changes. PRA polygons come from the
  # versioned gpkg loaded at startup (pra_full_sf); wkt areas are
  # parsed from their stored WKT strings.
  rpt_areas_sf <- reactive({
    areas <- rx$rpt_areas
    if (length(areas) == 0) return(NULL)
    rows <- purrr::map(areas, function(a) {
      g <- tryCatch(
        {
          if (identical(a$kind, "pra")) {
            pra_full_sf |>
              filter(programarea_key == a$value) |>
              st_geometry()
          } else {
            st_as_sfc(a$value, crs = 4326)
          }
        },
        error = function(e) NULL)
      if (is.null(g) || length(g) == 0) return(NULL)
      st_sf(label = a$label, kind = a$kind, geometry = g, crs = 4326)
    })
    rows <- Filter(Negate(is.null), rows)
    if (length(rows) == 0) return(NULL)
    do.call(rbind, rows)
  })

  # * render added Report areas on map_rpt with thick pink border ----
  # + symbol label at the polygon's point-on-surface. Re-runs whenever
  # rpt_areas_sf() invalidates; clears stale layers first.
  observe({
    sf_data <- rpt_areas_sf()
    proxy   <- mapboxgl_proxy("map_rpt")
    proxy |>
      clear_layer("rpt_added_lbl") |>
      clear_layer("rpt_added_ln") |>
      clear_layer("rpt_added_fill")
    if (is.null(sf_data) || nrow(sf_data) == 0) return()
    pts <- suppressWarnings(st_point_on_surface(sf_data))
    proxy |>
      add_fill_layer(
        id           = "rpt_added_fill",
        source       = sf_data,
        fill_color   = "#ff00aa",
        fill_opacity = 0.15) |>
      add_line_layer(
        id         = "rpt_added_ln",
        source     = sf_data,
        line_color = "#ff00aa",
        line_width = 4) |>
      add_symbol_layer(
        id              = "rpt_added_lbl",
        source          = pts,
        text_field      = "label",
        text_size       = 14,
        text_color      = "#ffffff",
        text_halo_color = "#ff00aa",
        text_halo_width = 2,
        text_offset     = c(0, -1))
  })

  # * map_rpt_click: register Program Area clicks on the Report map ----
  # mirrors the main Map tab's click handler so `btn_add_pra` works after
  # clicking a Program Area on either map.
  observeEvent(input$map_rpt_click, {
    req(input$map_rpt_click)
    if (input$sel_unit != "pra") return()
    if (is.null(input$map_rpt_feature_click)) return()
    rx$clicked_cell <- NULL
    rx$clicked_pa   <- NULL
    rx$clicked_pra  <- list(
      id         = input$map_rpt_feature_click$id,
      properties = input$map_rpt_feature_click$properties)
  })

  # * prepopulate the "Label for next area" input with the clicked
  # Program Area name so the user can click > Add without typing.
  observeEvent(rx$clicked_pra, {
    props <- rx$clicked_pra$properties
    req(props)
    nm <- props$programarea_name %||% props$programarea_key
    if (!is.null(nm) && nzchar(nm))
      updateTextInput(session, "rpt_area_label", value = nm)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # * highlight the currently-clicked Program Area on both maps with a
  # thick bright-pink border so it stands out against the Spectral
  # cell colormap. Applies to the Map tab and the Report tab's
  # embedded map; clears the highlight when rx$clicked_pra is NULL.
  observe({
    clicked <- rx$clicked_pra
    key <- if (!is.null(clicked))
      clicked$properties$programarea_key %||% clicked$properties$planarea_key
    for (mid in c("map", "map_rpt")) {
      proxy <- mapboxgl_proxy(mid)
      proxy |> clear_layer("pra_highlight_ln")
      if (!is.null(key) && nzchar(key)) {
        proxy |> add_line_layer(
          id           = "pra_highlight_ln",
          source       = "pra_src",
          source_layer = tbl_pra_pm,
          line_color   = "#ff00aa",
          line_width   = 4,
          filter       = list("==", "programarea_key", key))
      }
    }
  })

  # * btn_add_drawn ----
  observeEvent(input$btn_add_drawn, {
    p   <- tryCatch(rpt_drawn_sf(), error = function(e) NULL)
    if (is.null(p)) {
      showNotification(
        "Draw a polygon on the map first.",
        type = "warning")
      return()
    }
    wkt <- sf::st_as_text(sf::st_geometry(p))
    if (nchar(wkt) > 8000) {
      p   <- sf::st_simplify(p, dTolerance = 0.01, preserveTopology = TRUE)
      wkt <- sf::st_as_text(sf::st_geometry(p))
      showNotification(
        "Polygon simplified to fit request.",
        type = "warning")
    }
    lbl <- input$rpt_area_label %||% paste0("Area ", length(rx$rpt_areas) + 1)
    rx$rpt_areas <- c(
      rx$rpt_areas,
      list(list(label = lbl, kind = "wkt", value = wkt)))
    updateTextInput(
      session, "rpt_area_label",
      value = paste0("Area ", length(rx$rpt_areas) + 1))
  })

  # * btn_add_pra ----
  observeEvent(input$btn_add_pra, {
    if (is.null(rx$clicked_pra)) {
      showNotification(
        "Click a Program Area on the map first (set Spatial units = Program areas).",
        type = "warning")
      return()
    }
    props <- rx$clicked_pra$properties
    key   <- props$programarea_key %||% props$planarea_key
    nm    <- props$programarea_name %||% props$planarea_name
    lbl   <- if (nzchar(input$rpt_area_label)) input$rpt_area_label else nm
    rx$rpt_areas <- c(
      rx$rpt_areas,
      list(list(label = lbl, kind = "pra", value = key)))
    updateTextInput(
      session, "rpt_area_label",
      value = paste0("Area ", length(rx$rpt_areas) + 1))
  })

  # * rpt_areas_ui: list of added areas with delete buttons ----
  output$rpt_areas_ui <- renderUI({
    areas <- rx$rpt_areas
    if (length(areas) == 0)
      return(tags$p(class = "text-muted small", "No areas yet."))
    tagList(lapply(seq_along(areas), function(i) {
      a <- areas[[i]]
      kind_lbl <- switch(a$kind, pra = "Program Area", wkt = "drawn", a$kind)
      div(
        class = "d-flex align-items-center mb-1",
        tags$span(
          class = "flex-grow-1 small",
          sprintf("%d. %s (%s)", i, a$label, kind_lbl)),
        actionButton(
          paste0("rpt_del_", i),
          "",
          icon  = icon("trash"),
          class = "btn-sm btn-outline-danger"))
    }))
  })

  # dynamically wire up the per-row delete buttons
  observe({
    lapply(seq_along(rx$rpt_areas), function(i) {
      local({
        idx <- i
        observeEvent(
          input[[paste0("rpt_del_", idx)]],
          {
            cur <- rx$rpt_areas
            cur[[idx]] <- NULL
            rx$rpt_areas <- cur
          },
          ignoreInit = TRUE,
          once       = TRUE)
      })
    })
  })

  # * btn_rpt_submit: POST to plumber in a background worker and open
  # the returned URL on resolve. Keeps the Shiny session responsive
  # while the render runs (which can take a couple of minutes).
  observeEvent(input$btn_rpt_submit, {
    areas <- rx$rpt_areas
    if (length(areas) == 0) {
      showNotification("Add at least one area first.", type = "error")
      return()
    }
    body <- list(
      title  = input$rpt_title,
      ver    = input$rpt_ver,
      format = input$rpt_format,
      areas  = areas)
    endpoint <- Sys.getenv(
      "MSENS_REPORT_URL",
      unset = "https://api.marinesensitivity.org/report")

    # sticky indeterminate progress notification with a bootstrap spinner;
    # removed in both the resolve and reject handlers below.
    notif_id <- showNotification(
      tags$div(
        tags$div(
          class        = "spinner-border spinner-border-sm me-2",
          role         = "status",
          `aria-hidden`= "true"),
        tags$span(
          "Generating report — this may take a couple of minutes...")),
      duration    = NULL,
      closeButton = FALSE,
      type        = "message")

    # run the request in a background R worker. `body` and `endpoint`
    # are captured and serialized to the worker; httr2 is referenced via
    # namespace so the worker loads it automatically. The .then callbacks
    # run back on the main Shiny thread, so they can safely touch
    # `session` and the notification stack.
    promises::future_promise(
      {
        httr2::request(endpoint) |>
          httr2::req_body_json(body) |>
          httr2::req_timeout(600) |>
          httr2::req_perform() |>
          httr2::resp_body_json()
      },
      seed = TRUE) |>
      promises::then(
        onFulfilled = function(resp) {
          removeNotification(notif_id)
          if (is.null(resp$url)) {
            showNotification(
              paste0(
                "Report finished but returned no URL",
                if (!is.null(resp$error)) paste0(": ", resp$error) else "."),
              type = "error", duration = 10)
            return()
          }
          session$sendCustomMessage("openUrl", resp$url)
          showNotification(
            "Report ready — opening in a new tab.",
            type = "message", duration = 5)
        },
        onRejected = function(e) {
          removeNotification(notif_id)
          showNotification(
            paste("Report request failed:", conditionMessage(e)),
            type = "error", duration = 10)
        })

    # return invisibly so the observer doesn't block on the promise
    invisible(NULL)
  })
}
shinyApp(ui, server)
