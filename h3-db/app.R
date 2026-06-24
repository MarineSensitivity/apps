# OBIS biodiversity-by-H3 explorer (h3t) ----
#
# Explore the live `h3t` tile service (https://h3t.marinesensitivity.org) that
# serves OBIS biodiversity indicators (ES50, richness, Shannon, # records) as
# on-demand H3 hexagon tiles from a read-only DuckDB store. Pick an indicator,
# a taxon filter (preset or custom), a year range, and a hexagon-detail cap;
# see the map + value stats; or paste a custom SELECT.
#
# Reused later by the species app (apps/mapsp) to show per-species OBIS
# occurrence summaries. The tile SQL/URL helpers are the single source of truth
# in obisindicators::obis_h3t_sql() / obis_h3t_url() (sourced below).

# packages ----
librarian::shelf(
  base64enc, bslib, glue, htmltools, jsonlite, shiny,
  # antimeridian-fixed mapgl (PR walkerke/mapgl#211); revert to walkerke/mapgl once merged.
  # if mapgl is already installed, force it: remotes::install_github("bbest/mapgl@fix/h3t-antimeridian")
  "bbest/mapgl@fix/h3t-antimeridian",
  quiet = T)

options(
  shiny.autoreload = T,
  bslib.color_contrast_warnings = F,
  timeout = 6)   # cap the /h3t/stats fetch so a slow/down service can't stall the map

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# config ----
H3T_HOST   <- "https://h3t.marinesensitivity.org"
TILES_BASE <- "h3tiles://h3t.marinesensitivity.org/h3t/{z}/{x}/{y}.h3t"
RELEASE    <- format(Sys.Date(), "v%Y%m%d")
VIRIDIS5   <- c("#440154", "#3b528b", "#21918c", "#5ec962", "#fde725")

# h3t SQL/URL helpers — single source of truth in obisindicators/R/h3t.R ----
# (needs only base64enc + glue; not the full package / gsl / h3)
h3t_src <- Sys.glob(c(
  "/share/github/marinebon/obisindicators/R/h3t.R",          # MST server
  "../../../marinebon/obisindicators/R/h3t.R",               # local sibling checkout
  "~/Github/marinebon/obisindicators/R/h3t.R"))[1]
if (is.na(h3t_src))
  stop("could not locate obisindicators/R/h3t.R — clone marinebon/obisindicators alongside this repo")
source(h3t_src)

# choices ----
INDICATORS <- c(
  "ES(50) — expected species / 50 records" = "es",
  "Species richness"                        = "sp",
  "Shannon diversity"                       = "shannon",
  "Number of records"                       = "n")

# real OBIS taxonomic groups (work once the global store is built; the current
# South Atlantic demo store is species-only, so these return empty until then)
PRESETS <- list(
  "All taxa"                       = list(),
  "Seabirds (class Aves)"          = list(class  = "Aves"),
  "Bony fishes (Actinopterygii)"   = list(class  = "Actinopterygii"),
  "Sharks & rays (Elasmobranchii)" = list(class  = "Elasmobranchii"),
  "Marine mammals (Mammalia)"      = list(class  = "Mammalia"),
  "Sea turtles (order Testudines)" = list(order  = "Testudines"),
  "Corals & anemones (Anthozoa)"   = list(class  = "Anthozoa"),
  "Mollusks (phylum Mollusca)"     = list(phylum = "Mollusca"),
  "Crustaceans (Malacostraca)"     = list(class  = "Malacostraca"))
RANKS <- c("phylum", "class", "order", "family", "genus", "species")

# stats endpoint (value distribution for color-ramp breaks) ----
get_stats <- function(sql, res_h3 = 3) {
  q   <- gsub("\n", "", base64enc::base64encode(charToRaw(sql)))
  url <- glue("{H3T_HOST}/h3t/stats?q={utils::URLencode(q, reserved = TRUE)}&res_h3={res_h3}")
  tryCatch(jsonlite::fromJSON(url), error = function(e) list(error = conditionMessage(e)))
}

# ui ----
ui <- page_sidebar(
  title = "OBIS biodiversity by H3 hexagon",
  theme = bs_theme(version = 5, preset = "darkly"),

  sidebar = sidebar(
    width = 360,
    helpText(
      "Live tiles from ", tags$code("h3t.marinesensitivity.org"),
      " — each hexagon is rendered on demand from a read-only SQL query."),

    selectInput("indicator", "Indicator", INDICATORS),

    selectInput("preset", "Taxon group", names(PRESETS)),
    checkboxInput("custom_taxon", "Custom taxon filter", FALSE),
    conditionalPanel(
      "input.custom_taxon",
      selectInput("rank", "Rank", RANKS, selected = "class"),
      textInput("taxon_val", "Value", placeholder = "e.g. Aves")),

    sliderInput("years", "Year range", min = 1900, max = 2026,
                value = c(1900, 2026), step = 1, sep = ""),

    sliderInput("res_max", "Max hexagon detail (1 = coarse … 7 = fine)",
                min = 1, max = 7, value = 5, step = 1),

    checkboxInput("custom_sql", "Advanced: custom SQL", FALSE),
    conditionalPanel(
      "input.custom_sql",
      textAreaInput(
        "sql", "SQL — project exactly cell_id, value, n; use {{res}}", rows = 6,
        value = "SELECT cell_id, es AS value, n\nFROM idx_h3\nWHERE res = LEAST({{res}}, 7)")),

    actionButton("update", "Update map", class = "btn-primary w-100"),

    hr(),
    uiOutput("stats_ui"),
    tags$details(
      tags$summary("Generated SQL & tile URL"),
      tags$pre(style = "white-space:pre-wrap;font-size:11px;", textOutput("sql_txt", inline = TRUE)),
      tags$pre(style = "white-space:pre-wrap;font-size:10px;", textOutput("url_txt", inline = TRUE)))),

  card(full_screen = TRUE, padding = 0, maplibreOutput("map", height = "100%")))

# server ----
server <- function(input, output, session) {

  # build the tile SQL from the controls (or custom SQL) — fires on load + Update
  q_sql <- eventReactive(input$update, {
    if (isTRUE(input$custom_sql)) return(input$sql)

    taxon <-
      if (isTRUE(input$custom_taxon) && nzchar(input$taxon_val)) {
        setNames(list(input$taxon_val), input$rank)
      } else {
        p <- PRESETS[[input$preset]]
        if (length(p)) p else NULL
      }
    years <- if (identical(as.integer(input$years), c(1900L, 2026L))) NULL else input$years

    obis_h3t_sql(
      indicator = input$indicator,
      taxon     = taxon,
      years     = years,
      res_max   = input$res_max)
  }, ignoreNULL = FALSE)

  stats <- reactive(get_stats(q_sql(), res_h3 = min(input$res_max, 4)))

  output$sql_txt <- renderText(q_sql())
  output$url_txt <- renderText(
    obis_h3t_url(base_url = TILES_BASE, sql = q_sql(), release = RELEASE))

  output$stats_ui <- renderUI({
    s <- stats()
    if (!is.null(s$error))
      return(div(class = "text-warning",
                 strong("query error: "), s$reason %||% s$error))
    if (is.null(s$n) || is.na(s$n))
      return(em("no cells for this filter (check taxon / years / data extent)"))
    tags$table(
      class = "table table-sm",
      tags$tbody(
        tags$tr(tags$td("cells"),  tags$td(format(s$n, big.mark = ","))),
        tags$tr(tags$td("min"),    tags$td(signif(s$min, 4))),
        tags$tr(tags$td("max"),    tags$td(signif(s$max, 4))),
        tags$tr(tags$td("p02–p98"),tags$td(glue("{signif(s$p02,3)} – {signif(s$p98,3)}")))))
  })

  output$map <- renderMaplibre({
    sql   <- q_sql()
    s     <- stats()
    tiles <- obis_h3t_url(base_url = TILES_BASE, sql = sql, release = RELEASE)

    # color ramp from the robust p02–p98 range (fallback 0–1)
    lo <- s$p02 %||% 0; hi <- s$p98 %||% 1
    if (!is.numeric(lo) || !is.numeric(hi) || !is.finite(lo) || !is.finite(hi) || lo >= hi) {
      lo <- 0; hi <- 1
    }
    brks <- seq(lo, hi, length.out = length(VIRIDIS5))
    lbl  <- names(INDICATORS)[match(input$indicator, INDICATORS)] %||% "value"

    maplibre(style = carto_style("dark-matter"), center = c(-20, 5), zoom = 1.4) |>
      add_h3t_source(id = "obis", tiles = tiles) |>
      add_fill_layer(
        id           = "obis_fill",
        source       = "obis",
        source_layer = "obis",
        fill_color   = interpolate(column = "value", values = brks, stops = VIRIDIS5),
        fill_opacity = 0.8) |>
      add_legend(lbl, values = round(c(lo, hi), 2), colors = VIRIDIS5,
                 position = "bottom-left") |>
      add_navigation_control() |>
      add_scale_control() |>
      add_fullscreen_control()
  })
}

shinyApp(ui, server)
