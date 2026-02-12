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
  DBI,
  dplyr,
  duckdb,
  DT,
  fs,
  ggiraph,
  ggplot2,
  glue,
  here,
  yogevherz / plotme,
  plotly,
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

# profile performance of app:
#   profvis::profvis(shiny::runApp(here::here("mapgl")))

# variables ----
verbose <- interactive()

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
  dir_v,
  glue("~/_big/msens/derived/{v_dir}"))

mapbox_tkn_txt <- glue("{dir_private}/mapbox_token_bdbest.txt")
cell_tif    <- glue("{dir_data}/derived/r_bio-oracle_planarea.tif")
sdm_db      <- glue("{dir_big}/sdm.duckdb")
er_gpkg     <- glue("{dir_v}/ply_ecoregions_2025{v_sfx}.gpkg")
lyrs_csv    <- glue("{dir_v}/layers{v_sfx}.csv")
metrics_tif <- glue("{dir_v}/r_metrics{v_sfx}.tif")
pra_gpkg    <- glue("{dir_v}/ply_programareas_2026{v_sfx}.gpkg")
sr_pra_csv  <- glue("{dir_v}/subregion_programareas.csv")
sr_bb_csv <- here("mapgl/cache/subregion_bboxes.csv")
init_tif <- here("mapgl/cache/r_init.tif")
taxonomy_csv <- here(
  "../workflows/data/taxonomic_hierarchy_worms_2025-10-30.csv"
)
stopifnot(all(file.exists(c(
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
))))

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
get_rast <- function(m_key, subregion_key = "USA") {
  # m_key         = "score_extriskspcat_primprod_ecoregionrescaled_equalweights"
  # m_key = "extrisk_mammal"; subregion_key = "L48"

  d <- tbl(con_sdm, "metric") |> # get metric.metric_seq
    filter(metric_key == !!m_key) |> #   by input$sel_lyr
    select(metric_seq) |>
    inner_join(
      # get cell_metric.value
      tbl(con_sdm, "cell_metric") |>
        select(metric_seq, cell_id, value),
      by = join_by(metric_seq)
    ) |>
    select(cell_id, value) |>
    inner_join(
      # limit to zone
      tbl(con_sdm, "zone") |>
        filter(
          tbl == !!glue("ply_subregions_2026{v_sfx}"),
          fld == "subregion_key",
          value == !!subregion_key
        ) |> #   by input$sel_subregion
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

plot_flower <- function(
  data,
  fld_category,
  fld_height,
  fld_width,
  tooltip_expr = NULL,
  score = NULL,
  # colors    = "Set2",
  title = NULL
) {
  # TODO: ck NA scores ok with colors

  stopifnot(is.numeric(data |> pull({{ fld_height }})))
  stopifnot(is.numeric(data |> pull({{ fld_width }})))

  if (is.null(score)) {
    score <- data |>
      # ensure both are not just integer (weighted.mean goes to 0)
      mutate(
        "{{fld_height}}" := as.double({{ fld_height }}),
        "{{fld_width}}" := as.double({{ fld_width }})
      ) |>
      summarize(
        score = weighted.mean({{ fld_height }}, {{ fld_width }}, na.rm = T)
      ) |>
      pull(score)
  }

  # Calculate positions
  d <- data |>
    arrange({{ fld_category }}) |>
    mutate(across(!where(is.character), as.double)) |>
    mutate(
      # Calculate angles for plotting
      ymax = cumsum({{ fld_width }}),
      ymin = lag(ymax, default = 0), # ,  c(0, head(ymax, n=-1)),
      xmax = {{ fld_height }},
      xmin = 0
    )

  sym_category <- ensym(fld_category)
  sym_height <- ensym(fld_height)
  sym_width <- ensym(fld_width)

  if (!is.null(tooltip_expr)) {
    d <- d |>
      mutate(
        tooltip = glue(tooltip_expr)
      )
  } else {
    d <- d |>
      mutate(
        tooltip = glue("{!!fld_category}")
      )
  }

  # components <- tbl(con_sdm, "taxon") |> filter(is_ok) |> distinct(sp_cat) |>
  #   pull(sp_cat) %>% c(., "primprod") |> sort() |>  paste(collapse = '", "') |> cat()
  components <- c(
    "invertebrate",
    "mammal",
    "other",
    "primprod",
    "turtle",
    "bird",
    "coral",
    "fish"
  )
  # primprod in 4th green position
  cols <- setNames(
    hue_pal()(length(components)),
    components
  )
  # show_col(cols)

  g <- ggplot(d) +
    geom_rect_interactive(
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax,
        fill = {{ fld_category }},
        color = "white",
        data_id = {{ fld_category }},
        tooltip = tooltip
      ),
      color = "white",
      alpha = 0.5
    ) +
    scale_fill_manual(values = cols) +
    coord_polar(theta = "y") +
    # Create donut hole
    xlim(c(-10, max(data |> pull({{ fld_height }})))) +
    # Add center score
    annotate(
      "text",
      x = -10,
      y = 0,
      label = round(score),
      size = 8,
      fontface = "bold"
    ) +
    # scale_fill_brewer(
    #   palette = colors) +
    # scale_fill_brewer() +
    theme_minimal() +
    # theme_void() +
    theme(
      legend.position = "bottom",
      plot.margin = unit(c(20, 20, 20, 20), "pt")
    )

  if (!is.null(title)) {
    g <- g +
      ggtitle(title)
  }

  girafe(
    ggobj = g,
    options = list(
      opts_sizing(rescale = TRUE, width = 1),
      opts_tooltip(
        css = "background-color:white;color:black;padding:5px;border-radius:3px;"
      )
    )
  )
}

# data prep ----

# * sr_choices ----
# sr_choices <- c(
#   "All USA" = "USA",
#   "Mainland USA" = "L48",
#   "Alaska" = "AK",
#   "Mainland USA & Alaska" = "AKL48")
# TODO: version subregions
sr_choices <- c(
  "All USA"         = "USA",
  "Alaska"          = "AK",
  "Gulf of America" = "GA",
  "Pacific"         = "PA")
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
  lyrs_pra <- dbListFields(con, glue("ply_programareas_2026{v_sfx}"))
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
      tbl == !!glue("ply_subregions_2026{v_sfx}"),
      fld == "subregion_key") |>
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

  if (verbose){
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
  pra_sr <- d_sr_pra |> filter(subregion_key == !!sr_key) |> pull(programarea_key)
  r <- init(r_cell[[1]], NA)
  plot(r_metrics[["programarea_key"]])
  r_sr <- r_metrics[["programarea_key"]] %in% pra_sr
  r_sr <- mask(r_sr, r_sr, maskvalues = F) |> trim()
  r_sr |> st_bbox() |> as.numeric()
}
if (!file_exists(sr_bb_csv)) {
  for (sr_key in unique(d_sr_pra$subregion_key)) { # sr_key = "GA"
    bbox <- get_sr_bbox(sr_key)
    d_bb_sr <- tibble(
      subregion_key = sr_key,
      xmin = bbox[1],
      ymin = bbox[2],
      xmax = bbox[3],
      ymax = bbox[4])
    if (!exists("d_sr_bb")) {
      d_sr_bb <- d_bb_sr
    } else {
      d_sr_bb <- bind_rows(d_sr_bb, d_bb_sr)
    }
  }
  write_csv(d_sr_bb, sr_bb_csv)
}
d_sr_bb <- read_csv(sr_bb_csv)

# * d_taxonomy ----
d_taxonomy <- read_csv(taxonomy_csv, guess_max = Inf)

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
     }"
  ))),
  title = glue("BOEM Marine Sensitivity (v{v_int})"),
  sidebar = sidebar(
    selectInput(
      "sel_subregion",
      "Study area",
      choices = sr_choices
    ),
    selectInput(
      "sel_unit",
      "Spatial units",
      choices = c(
        "Raster cells (0.05°)" = "cell",
        # "Planning areas"       = "pa",
        "Program areas"        = "pra"
      )
    ),
    selectInput(
      "sel_lyr",
      "Layer",
      choices = lyr_choices,
      selected = lyr_default
    ),
    input_switch(
      "tgl_sphere",
      "Sphere",
      T
    ),
    input_dark_mode(
      id = "tgl_dark",
      mode = "dark"
    )
  ),

  navset_card_tab(
    full_screen = TRUE,
    nav_panel(
      "Map",
      mapboxglOutput("map"),
      conditionalPanel(
        absolutePanel(
          id = "flower_panel",
          top = 80,
          left = 30,
          # width       = 350,
          height = "auto",
          draggable = T,
          accordion(accordion_panel(
            "Plot",
            card(
              # style       = "resize:vertical;",
              full_screen = T,
              # card_header(class = "bg-dark", "Score"),
              card_body(
                girafeOutput("plot_flower", height = "100%")
              )
            )
          ))
        ),
        condition = 'output.flower_status'
      )
    ),
    nav_panel(
      "Species",
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
    )
  )
)

# server ----
server <- function(input, output, session) {
  # observe(session$setCurrentTheme(
  #   if (isTRUE(input$tgl_dark)) dark else light
  # ))

  # reactive values ----
  rx <- reactiveValues(
    clicked_pa       = NULL,
    clicked_pra      = NULL,
    clicked_cell     = NULL,
    spp_tbl          = NULL,
    spp_tbl_hdr      = NULL,
    spp_tbl_filename = NULL)

  output$flower_status <- reactive({
    if (!is.null(rx$clicked_pa) || !is.null(rx$clicked_pra) || !is.null(rx$clicked_cell)) {
      return(T)
    }
    F
  })
  outputOptions(output, "flower_status", suspendWhenHidden = FALSE)

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

  # map ----
  output$map <- renderMapboxgl({
    # default to show raster score
    r <- r_init
    bbox <- st_bbox(r) |> as.numeric()
    n_cols <- 11
    cols_r <- rev(RColorBrewer::brewer.pal(n_cols, "Spectral"))
    rng_r <- minmax(r) |> as.numeric() |> signif(digits = 3)

    # TODO: eval: input = list(tgl_sphere = T)
    mapboxgl(
      style = mapbox_style("dark"),
      projection = ifelse(input$tgl_sphere, "globe", "mercator")
    ) |>
      fit_bounds(bbox) |>
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
      add_image_source(
        id = "r_src",
        data = r,
        colors = cols_r
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
      add_raster_layer(
        id = "r_lyr",
        source = "r_src",
        raster_opacity = 0.6,
        raster_resampling = "nearest",
        before_id = "er_ln"
      ) |>
      mapgl::add_legend(
        get_lyr_name(lyr_default),
        values = rng_r,
        colors = cols_r,
        position = "bottom-right"
      ) |>
      add_fullscreen_control() |>
      add_navigation_control() |>
      add_scale_control() |>
      add_layers_control(
        layers = list(
          "Program Area outlines" = "pra_ln",
          "Ecoregions outlines"   = "er_ln",
          "Raster cell values"    = "r_lyr")) |>
      # add_draw_control(position = "top-right") |>
      add_geocoder_control(placeholder = "Go to location")
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

      map_proxy <- mapboxgl_proxy("map")

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

        # remove layers if exist
        map_proxy |>
          # clear_layer("pa_lyr") |>
          clear_layer("pra_lyr") |>
          clear_layer("r_lyr") |>
          clear_layer("r_src") |>
          clear_legend()

        # add raster source and layer
        map_proxy |>
          add_image_source(
            id = "r_src",
            data = r,
            colors = cols_r
          ) |>
          add_raster_layer(
            id = "r_lyr",
            source = "r_src",
            raster_opacity = 0.6,
            raster_resampling = "nearest",
            before_id = "er_ln"
          ) |>
          mapgl::add_legend(
            get_lyr_name(input$sel_lyr),
            values = rng_r,
            colors = cols_r,
            position = "bottom-right"
          ) |>
          mapgl::fit_bounds(
            bbox = as.numeric(st_bbox(r)),
            animate = T
          ) |>
          clear_controls("layers") |>
          add_layers_control(
            layers = list(
              "Program Area outlines" = "pra_ln",
              "Ecoregion outlines"    = "er_ln",
              "Raster cell values"    = "r_lyr"))

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
        if (verbose)
          message(glue("sr_bb: {paste(round(sr_bb,2), collapse = ', ')}"))

        pra_keys <- d_sr_pra |> filter(subregion_key == !!sr_key) |> pull(programarea_key)
        pra_filter <- c("in", "programarea_key", pra_keys)

        # show program area layer
        n_cols <- 11
        cols_r <- rev(RColorBrewer::brewer.pal(n_cols, "Spectral"))

        # get range of program area values
        rng_pra <- tbl(con_sdm, "zone") |>
          filter(
            fld == "programarea_key",
            value %in% pra_keys
          ) |>
          select(programarea_key = value, zone_seq) |>
          inner_join(tbl(con_sdm, "zone_metric"), by = join_by(zone_seq)) |>
          inner_join(
            tbl(con_sdm, "metric") |>
              filter(metric_key == !!lyr),
            by = join_by(metric_seq)
          ) |>
          pull(value) |>
          range()

        # colors
        cols_pra <- colorRampPalette(cols_r, space = "Lab")(n_cols)
        brks_pra <- seq(rng_pra[1], rng_pra[2], length.out = n_cols)

        # remove layers if exist
        map_proxy |>
          clear_layer("r_lyr") |>
          # clear_layer("pa_lyr") |>
          clear_layer("pra_lyr") |>
          clear_legend()

        # add program area fill layer
        map_proxy |>
          add_fill_layer(
            id = "pra_lyr",
            source = "pra_src",
            source_layer = glue("public.ply_programareas_2026{v_sfx}"),
            fill_color = interpolate(
              column = lyr,
              values = brks_pra,
              stops = cols_pra,
              na_color = "lightgrey"
            ),
            fill_outline_color = "white",
            tooltip = concat("Value: ", get_column(lyr)),
            hover_options = list(
              fill_color = "purple",
              fill_opacity = 1
            ),
            before_id = "pra_ln",
            filter = pra_filter
          ) |>
          mapgl::add_legend(
            get_lyr_name(input$sel_lyr),
            values = round(rng_pra, 1),
            colors = cols_pra,
            position = "bottom-right"
          ) |>
          mapgl::fit_bounds(sr_bb, animate = T) |>
          clear_controls("layers") |>
          add_layers_control(
            layers = list(
              "Program Area outlines" = "pra_ln",
              "Ecoregion outlines"    = "er_ln",
              "Program Area values"   = "pra_lyr"))

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

  # plot_flower ----
  output$plot_flower <- renderGirafe({
    # set height based on container size
    height <- "100%"

    if (input$sel_unit == "cell" && !is.null(rx$clicked_cell)) {

      # get data for cell
      cell_id <- rx$clicked_cell$cell_id
      lng <- rx$clicked_cell$lng |> round(3)
      lat <- rx$clicked_cell$lat |> round(3)

      if (verbose)
        message(glue("Rendering flower plot for cell id: {cell_id}"))

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
        d_fl |>
          plot_flower(
            fld_category = component,
            fld_height = score,
            fld_width = even,
            tooltip_expr = "{component}: {round(score, 2)}",
            title = glue("Cell ID: {cell_id} (x: {lng}, y: {lat})")
          )
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

      # get data for program area
      pra_name <- rx$clicked_pra$properties$programarea_name
      pra_key <- rx$clicked_pra$properties$programarea_key

      if (verbose)
        message(glue("Rendering flower plot for Program Area: {pra_name}"))

      l <- rx$clicked_pra$properties
      l <- l[str_detect(names(l), "_ecoregion_rescaled$")]

      d_fl <- tibble(
        metric_key = names(l),
        score = unlist(l)
      ) |>
        mutate(
          component = metric_key |>
            str_replace("extrisk_", "") |>
            str_replace("_ecoregion_rescaled", "") |>
            str_replace("_", " "),
          even = 1
        ) |>
        filter(component != "all")

      if (nrow(d_fl) > 0) {
        d_fl |>
          plot_flower(
            fld_category = component,
            fld_height = score,
            fld_width = even,
            tooltip_expr = "{component}: {round(score, 2)}",
            title = pra_name
          )
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
    # ** subregion ----
    # if (is.null(rx$clicked_cell) && is.null(rx$clicked_pa) && is.null(rx$clicked_pra)) {
    if (is.null(rx$clicked_cell) && is.null(rx$clicked_pra)) {
      sr_key <- input$sel_subregion
      sr_lbl <- names(sr_choices)[sr_choices == sr_key]

      if (verbose)
        message(glue("Getting species table for subregion: {sr_lbl} ({sr_key})"))

      rx$spp_tbl_hdr <- glue("Species in {sr_lbl}")
      rx$spp_tbl_filename <- glue("species_{sr_key}")

      d_spp <- tbl(con_sdm, "zone_taxon") |>
        filter(
          zone_tbl == "ply_subregions_2026",
          zone_fld == "subregion_key",
          zone_value == sr_key
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

      if (verbose)
        message(glue("Getting species table for cell id: {cell_id}"))

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
          suit_er      = avg_suit * er_score,
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

      if (verbose)
        message(glue("Getting species table for Program Area: {pra_name}"))

      rx$spp_tbl_hdr <- glue("Species for Program Area: {pra_name}")
      rx$spp_tbl_filename <- glue(
        "species_programarea-{str_replace(pra_name, ' ', '-') |> str_to_lower()}"
      )

      d_spp <- tbl(con_sdm, "zone_taxon") |>
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

    spp_sci_cmn_fixes <- tribble(
      ~scientific_name,         ~common_name,
      "Eubalaena glacialis",    "North Atlantic right whale", # OLD: black right whale
      "Megaptera novaeangliae", "humpback whale",             # OLD: hump
      "Balaena mysticetus",     "bowhead whale"               # OLD: Arctic right whale
    )

    # rename columns
    d_spp |>
      mutate(
        model_url = glue(
          "https://shiny.marinesensitivity.org/mapsp/?mdl_seq={mdl_seq}"
        ),
        taxon_str = glue("{taxon_authority}:{taxon_id}"),
        taxon_url = ifelse(
          taxon_authority == "botw",
          "https://birdsoftheworld.org",
          glue(
            "https://www.marinespecies.org/aphia.php?p=taxdetails&id={taxon_id}"
          )
        ),
        sp_common = recode_values(
          # TODO: update common names in DB
          sp_scientific,
          from = spp_sci_cmn_fixes$scientific_name,
          to   = spp_sci_cmn_fixes$common_name,
          default = sp_common)
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
}
shinyApp(ui, server)
