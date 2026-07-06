# OBIS biodiversity-by-H3 explorer (h3t) ----
#
# Explore the live `h3t` tile service (https://h3t.marinesensitivity.org) that
# serves OBIS biodiversity indicators (ES50, richness, Shannon, # records) as
# on-demand H3 hexagon tiles from a read-only DuckDB store. Pick an indicator,
# a taxon filter (preset or custom), and a year range; the map + value stats
# update live over a `maplibre_proxy` so the view never jumps.
#
# Resolution control (floating, vertical, left of the map):
#   - AUTO  (default): hexagon detail follows the map zoom hierarchically — the
#            service substitutes each tile's H3 resolution into `LEAST({{res}},7)`.
#            The slider is a live read-out synced from the map zoom.
#   - MANUAL (check the box): pin a single H3 resolution; hexagons stay that size
#            at every zoom (the SQL hard-codes `res = <n>`, no `{{res}}` token).
# Time control floats horizontally across the bottom (more width for the range).
#
# Reused later by the species app (apps/mapsp) to show per-species OBIS
# occurrence summaries. Tile SQL/URL helpers are the single source of truth in
# obisindicators::obis_h3t_sql() / obis_h3t_url() (sourced below).

# packages ----
librarian::shelf(
  base64enc, bslib, DT, glue, htmltools, jsonlite, shiny, shinyjs, shinyWidgets,
  # antimeridian-fixed mapgl (PR walkerke/mapgl#211); revert to walkerke/mapgl once merged.
  # if mapgl is already installed, force it: remotes::install_github("bbest/mapgl@fix/h3t-antimeridian", force=T)
  # "bbest/mapgl@fix/h3t-antimeridian",
  mapgl,
  quiet = T)

options(
  shiny.autoreload = T,
  bslib.color_contrast_warnings = F,
  timeout = 6)   # cap the /h3t/stats fetch so a slow/down service can't stall the map

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a

# config ----
H3T_HOST    <- "https://h3t.marinesensitivity.org"
TILES_BASE  <- "h3tiles://h3t.marinesensitivity.org/h3t/{z}/{x}/{y}.h3t"
RELEASE     <- format(Sys.Date(), "v%Y%m%d")
VIRIDIS5    <- c("#440154", "#3b528b", "#21918c", "#5ec962", "#fde725")
INIT_CENTER <- c(-20, 5)
INIT_ZOOM   <- 1.4
INIT_OPACITY <- 0.85
YR_MIN      <- 1900L
YR_MAX      <- 2026L
RES_STORE_MAX <- 7L   # the DuckDB store tops out at H3 res 7

# zoom -> H3 resolution (port of server/h3t app/h3t_query.py zoom_to_res) ----
ZOOM_BREAKS <- local({ b <- 1 + (13 - 1) * (0:10) / 10; b[1] <- 0; b[11] <- 22; b })
zoom_to_res <- function(z) max(1L, min(10L, findInterval(as.numeric(z), ZOOM_BREAKS)))
# resolution actually served (store-capped) for a given map zoom
zoom_res_capped <- function(z) min(zoom_to_res(z), RES_STORE_MAX)

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

# store schema (for the "Schema" modal that helps users write custom SQL) ----
# the read-only DuckDB store registered as `obis` exposes exactly two tables.
# both files below are kept beside the app so the schema stays editable as data;
# the authoritative source is the DuckDB DDL in build_obis_h3_duckdb():
# https://github.com/marinebon/obisindicators/blob/52e3ac81503892dd0fa64d7d5291d8c5dd5ac620/R/h3t.R#L201-L223
SCHEMA_COLS <- read.csv("schema_cols.csv", stringsAsFactors = FALSE)
ERD_MMD     <- paste(readLines("schema_erd.mmd", warn = FALSE), collapse = "\n")
# prepend a mermaid theme directive so the ER diagram matches the app's skin
erd_src <- function(dark = TRUE)
  paste0('%%{init: {"theme":"', if (dark) "dark" else "neutral", '"}}%%\n', ERD_MMD)

# stats endpoint (value distribution for color-ramp breaks) ----
get_stats <- function(sql, res_h3 = 4) {
  q   <- gsub("\n", "", base64enc::base64encode(charToRaw(sql)))
  url <- glue("{H3T_HOST}/h3t/stats?q={utils::URLencode(q, reserved = TRUE)}&res_h3={res_h3}")
  tryCatch(jsonlite::fromJSON(url), error = function(e) list(error = conditionMessage(e)))
}

# color ramp (robust p02–p98 range) + legend label from stats ----
ramp_of <- function(s, indicator) {
  lo <- s$p02 %||% 0; hi <- s$p98 %||% 1
  if (!is.numeric(lo) || !is.numeric(hi) || !is.finite(lo) || !is.finite(hi) || lo >= hi) {
    lo <- 0; hi <- 1 }
  list(lo = lo, hi = hi,
       brks = seq(lo, hi, length.out = length(VIRIDIS5)),
       lbl  = names(INDICATORS)[match(indicator, INDICATORS)] %||% "value")
}

# hover/click label: "<indicator>: <value>  ·  n = <n>" (rounded) ----
tip_expr <- function(indicator) {
  short <- switch(indicator, es = "ES50", sp = "Richness",
                  shannon = "Shannon", n = "Records", "value")
  concat(short, ": ",
         number_format(get_column("value"), maximum_fraction_digits = 2),
         "  ·  n = ", number_format(get_column("n"), maximum_fraction_digits = 0))
}

# floating-control + dark-noUiSlider styling ----
CSS <- "
.map-wrap { position: relative; width: 100%; height: 100%; }
.float-panel {
  position: absolute; z-index: 5;
  background: rgba(18,20,26,.74); backdrop-filter: blur(5px);
  border: 1px solid rgba(255,255,255,.12); border-radius: 12px;
  color: #e9e9ec; box-shadow: 0 4px 18px rgba(0,0,0,.35); }
.float-res, .float-fill {
  top: 50%; transform: translateY(-50%);
  width: 84px; padding: 8px 6px 8px;
  display: flex; flex-direction: column; align-items: stretch; gap: 6px; }
.float-res  { left: 14px; }
.float-fill { right: 14px; }
.float-res .checkbox { margin: 0; min-height: 0; }
.float-res .checkbox label { font-size: 12px; color: #e9e9ec; padding-left: 20px; }
.float-years {
  left: 50%; bottom: 16px; transform: translateX(-50%);
  width: min(76%, 720px); padding: 6px 16px 9px;
  display: flex; flex-direction: column; gap: 4px; }
.float-cap { font-size: 11px; text-transform: uppercase; letter-spacing: .05em;
  opacity: .85; font-weight: 600; }
.float-sub { font-size: 12px; opacity: .95; font-weight: 600; text-align: center; }
/* draggable header + collapse toggle */
.panel-head { display: flex; align-items: center; justify-content: space-between;
  gap: 8px; cursor: move; user-select: none; }
.panel-body { display: flex; flex-direction: column; align-items: center; gap: 6px; }
.float-years .panel-body { align-items: stretch; }
.float-panel.collapsed .panel-body { display: none; }
.float-panel.dragging { opacity: .92; box-shadow: 0 8px 26px rgba(0,0,0,.5); }
.panel-btn { background: transparent; border: 0; color: #e9e9ec; cursor: pointer;
  font-size: 15px; line-height: 1; padding: 0 3px; opacity: .6; }
.panel-btn:hover { opacity: 1; }
.panel-collapse::before { content: '\\2212'; }            /* minus when open */
.float-panel.collapsed .panel-collapse::before { content: '\\002b'; }   /* plus when collapsed */
/* top-right toolbar: theme toggle + about */
.float-topbar { top: 12px; right: 12px; display: flex; align-items: center;
  gap: 8px; padding: 6px 8px; }
.float-topbar .btn { padding: 2px 10px; font-size: 12px; line-height: 1.4; }
.float-topbar .form-group { margin: 0; }
.float-topbar .btn-group .btn { box-shadow: none; }
.res-col { display: flex; justify-content: center; }
/* native vertical range input — reliable handle tracking, min bottom / max top */
.nrange {
  -webkit-appearance: none; appearance: none;
  writing-mode: vertical-lr; direction: rtl;
  width: 18px; height: 180px; margin: 0; padding: 0;
  background: transparent; cursor: pointer; }
.nrange:focus { outline: none; }
.nrange::-webkit-slider-runnable-track {
  width: 7px; background: #2b2f3a; border-radius: 6px; }
.nrange::-webkit-slider-thumb {
  -webkit-appearance: none; appearance: none;
  width: 16px; height: 16px; border-radius: 50%; background: #f4f4f7;
  box-shadow: 0 0 3px rgba(0,0,0,.5); margin-left: -4.5px; }
.nrange::-moz-range-track { width: 7px; background: #2b2f3a; border-radius: 6px; }
.nrange::-moz-range-thumb {
  width: 16px; height: 16px; border: 0; border-radius: 50%; background: #f4f4f7; }
.locked { opacity: .45; }
.locked .nrange { pointer-events: none; }
/* dark noUiSlider skin */
.noUi-target { background: #2b2f3a; border: 0; box-shadow: none; border-radius: 6px; }
.noUi-connect { background: #5ec962; }
.noUi-connects { border-radius: 6px; }
.noUi-handle { background: #f4f4f7; border: 0; border-radius: 50%;
  box-shadow: 0 0 3px rgba(0,0,0,.5); cursor: pointer; }
.noUi-handle:before, .noUi-handle:after { display: none; }
.noUi-horizontal { height: 7px; }
.noUi-horizontal .noUi-handle { width: 15px; height: 15px; right: -8px; top: -4px; }
.noUi-vertical { width: 7px; }
.noUi-vertical .noUi-handle { width: 15px; height: 15px; left: -4px; top: -8px; }
.noUi-tooltip { background: #11141a; color: #e9e9ec; border: 0; font-size: 11px;
  padding: 1px 5px; }
"

# ui ----
ui <- page_sidebar(
  title = "OBIS biodiversity by H3 hexagon",
  theme = bs_theme(version = 5, preset = "darkly"),
  shinyjs::useShinyjs(),
  tags$head(
    tags$style(HTML(CSS)),
    # mermaid.js (ESM from CDN) for the Schema modal's ER diagram. startOnLoad is
    # off — the modal calls window.mermaid.run() itself once its content mounts.
    tags$script(type = "module", HTML(
      "import mermaid from 'https://cdn.jsdelivr.net/npm/mermaid@11/dist/mermaid.esm.min.mjs';",
      "mermaid.initialize({ startOnLoad: false, securityLevel: 'loose' });",
      "window.mermaid = mermaid;")),
    # grabbing the resolution slider (mouse/touch down) flags user intent so the
    # server can auto-switch to manual — distinct from programmatic zoom syncs
    tags$script(HTML(
      "$(document).on('mousedown touchstart keydown', '#res',",
      " function(){ Shiny.setInputValue('res_grab', Date.now(), {priority:'event'}); });"))),

  sidebar = sidebar(
    width = 340,
    selectInput("indicator", "Indicator", INDICATORS),

    selectInput("preset", "Taxon group", names(PRESETS)),
    checkboxInput("custom_taxon", "Custom taxon filter", FALSE),
    conditionalPanel(
      "input.custom_taxon",
      selectInput("rank", "Rank", RANKS, selected = "class"),
      textInput("taxon_val", "Value", placeholder = "e.g. Aves")),

    checkboxInput("custom_sql", "Advanced: custom SQL", FALSE),
    conditionalPanel(
      "input.custom_sql",
      textAreaInput(
        "sql", "SQL — project exactly cell_id, value, n; use {{res}}", rows = 6,
        value = "SELECT cell_id, es AS value, n\nFROM idx_h3\nWHERE res = LEAST({{res}}, 7)"),
      div(class = "d-flex justify-content-between align-items-center mb-2",
          actionLink("schema_link", "table schema ↗", class = "small"),
          tags$span()),
      actionButton("run_sql", "Run SQL", class = "btn-primary btn-sm w-100")),

    hr(),
    uiOutput("stats_ui"),
    tags$details(
      tags$summary("Generated SQL & tile URL"),
      tags$pre(style = "white-space:pre-wrap;font-size:11px;", textOutput("sql_txt", inline = TRUE)),
      tags$pre(style = "white-space:pre-wrap;font-size:10px;", textOutput("url_txt", inline = TRUE)))),

  card(
    full_screen = TRUE, padding = 0,
    div(
      class = "map-wrap",
      maplibreOutput("map", height = "100%"),

      # top-right toolbar: theme toggle + About (draggable) ----
      div(
        class = "float-panel float-topbar",
        div(class = "panel-head", style = "cursor:move;",
            radioGroupButtons(
              "theme", label = NULL, size = "sm",
              choiceNames = list(icon("moon"), icon("sun")),
              choiceValues = list("dark", "light"), selected = "dark"),
            actionButton("schema", "Schema", class = "btn-sm btn-outline-light"),
            actionButton("about", "About", class = "btn-sm btn-outline-light"))),

      # vertical resolution control, floating mid-left ----
      div(
        class = "float-panel float-res",
        div(class = "panel-head",
            tags$span(class = "float-cap", "Res"),
            tags$button(class = "panel-btn panel-collapse", type = "button")),
        div(class = "panel-body",
          div(id = "res_col", class = "res-col",
              tags$input(
                id = "res", class = "nrange", type = "range",
                min = 1, max = RES_STORE_MAX, step = 1, value = zoom_res_capped(INIT_ZOOM))),
          div(class = "float-sub", textOutput("res_lbl", inline = TRUE)),
          checkboxInput("res_manual", "manual", value = FALSE))),

      # horizontal year range, floating bottom-center ----
      div(
        class = "float-panel float-years",
        div(class = "panel-head",
            tags$span(class = "float-cap", "Years ", textOutput("yr_lbl", inline = TRUE)),
            tags$button(class = "panel-btn panel-collapse", type = "button")),
        div(class = "panel-body",
          noUiSliderInput(
            "years", label = NULL, width = "100%",
            min = YR_MIN, max = YR_MAX, value = c(YR_MIN, YR_MAX), step = 1,
            orientation = "horizontal", behaviour = "drag", margin = 1,
            color = "#5ec962", tooltips = FALSE, format = wNumbFormat(decimals = 0)))),

      # vertical fill-opacity control, floating mid-right ----
      div(
        class = "float-panel float-fill",
        div(class = "panel-head",
            tags$span(class = "float-cap", "Fill"),
            tags$button(class = "panel-btn panel-collapse", type = "button")),
        div(class = "panel-body",
          div(class = "res-col",
              tags$input(
                id = "opacity", class = "nrange", type = "range",
                min = 0, max = 100, step = 5, value = round(INIT_OPACITY * 100))),
          div(class = "float-sub", textOutput("fill_lbl", inline = TRUE)))))),

  # Shiny input binding for the native vertical range (#res). Registered
  # synchronously at end-of-body so it lands before Shiny's initial bindAll.
  tags$script(HTML("
    (function(){
      if (typeof Shiny === 'undefined' || !Shiny.InputBinding) return;
      var b = new Shiny.InputBinding();
      $.extend(b, {
        find: function(scope){ return $(scope).find('input.nrange'); },
        getValue: function(el){ return parseFloat(el.value); },
        setValue: function(el, value){ el.value = value; },
        subscribe: function(el, cb){ $(el).on('input.resB change.resB', function(){ cb(false); }); },
        unsubscribe: function(el){ $(el).off('.resB'); },
        receiveMessage: function(el, data){ if (data && data.value != null) el.value = data.value; },
        getRatePolicy: function(){ return { policy: 'debounce', delay: 150 }; }
      });
      Shiny.inputBindings.register(b, 'msens.resRange');
    })();")),

  # make the floating panels draggable (by their header) and collapsible
  tags$script(HTML("
    $(function(){
      function clamp(v,a,b){ return Math.max(a, Math.min(b, v)); }
      document.querySelectorAll('.map-wrap .float-panel').forEach(function(panel){
        var head = panel.querySelector('.panel-head'); if(!head) return;
        head.addEventListener('mousedown', function(e){
          if (e.target.closest('.panel-btn, .btn, input, label')) return;  // let controls work
          e.preventDefault();
          var wrap = panel.parentElement.getBoundingClientRect();
          var r = panel.getBoundingClientRect();
          var offX = e.clientX - r.left, offY = e.clientY - r.top;
          panel.style.transform='none'; panel.style.right='auto'; panel.style.bottom='auto';
          panel.classList.add('dragging');
          function move(ev){
            panel.style.left = clamp(ev.clientX - wrap.left - offX, 0, wrap.width  - panel.offsetWidth ) + 'px';
            panel.style.top  = clamp(ev.clientY - wrap.top  - offY, 0, wrap.height - panel.offsetHeight) + 'px';
          }
          function up(){ panel.classList.remove('dragging');
            document.removeEventListener('mousemove', move);
            document.removeEventListener('mouseup', up); }
          document.addEventListener('mousemove', move);
          document.addEventListener('mouseup', up);
        });
      });
      document.addEventListener('click', function(e){
        var btn = e.target.closest('.panel-collapse'); if(!btn) return;
        e.preventDefault(); e.stopPropagation();
        btn.closest('.float-panel').classList.toggle('collapsed');
      });
    });"))
)

# server ----
server <- function(input, output, session) {

  # --- About modal (content formerly atop the sidebar) ----
  observeEvent(input$about, {
    showModal(modalDialog(
      title = "OBIS biodiversity by H3 hexagon", easyClose = TRUE, size = "l",
      footer = modalButton("Close"),
      p("Live H3-hexagon tiles served from ", tags$code("h3t.marinesensitivity.org"),
        " — each hexagon is rendered on demand from a read-only SQL query against an ",
        "OBIS-derived DuckDB store, via the ", tags$code("h3t"), " tile factory."),
      tags$ul(
        tags$li(tags$b("Indicator / taxon / years"), " (sidebar + bottom bar) rebuild the ",
                "tile query in place over a map proxy — the view never jumps."),
        tags$li(tags$b("Resolution"), " (left): in ", tags$i("auto"), " it tracks the map zoom ",
                "hierarchically; check ", tags$i("manual"), " (or just grab the slider) to pin one ",
                "H3 resolution."),
        tags$li(tags$b("Fill"), " (right): layer opacity (paint only, no refetch)."),
        tags$li("Hover a hexagon for its value; click to pin a popup."),
        tags$li("Drag any panel by its header; use ", tags$b("−"), " to collapse it.")),
      p(class = "text-muted mb-0", "Indicators: ES(50), species richness, Shannon diversity, # records.")))
  })

  # --- Schema modal (ERD + column dictionary) — helps write custom SQL ----
  show_schema_modal <- function() {
    showModal(modalDialog(
      title = "Store schema — build custom SQL", easyClose = TRUE, size = "xl",
      footer = modalButton("Close"),
      id = "schema-modal",
      p("Custom SQL runs read-only against the ", tags$code("obis"), " DuckDB store, ",
        "which exposes the two tables below. Your ", tags$code("SELECT"),
        " must project ", tags$b("exactly"), " ", tags$code("cell_id"), ", ",
        tags$code("value"), ", and optionally ", tags$code("n"),
        " (no ", tags$code("SELECT *"), "). Substitute the tile's resolution with ",
        tags$code("{{res}}"), " (clamp with ", tags$code("LEAST({{res}}, 7)"), ")."),
      tags$ul(class = "small text-muted",
        tags$li(tags$code("idx_h3"), " — precomputed all-taxa indicators, one row ",
                "per (res, cell) for resolutions 1-7. Fast path for unfiltered maps."),
        tags$li(tags$code("occ_h3"), " — species-level occurrence counts at resolution ",
                "tiers 3 / 5 / 7. Use for taxon/year filters; compute indicators on the fly.")),
      tags$h6(class = "mt-3", "Entity-relationship diagram"),
      # empty target — mermaid renders OFF-DOM (body temp node, which has real
      # layout) then we inject the SVG. rendering in-place with mermaid.run()
      # here fails ("Could not find a suitable point...") because the modal is
      # still hidden/animating, so the in-place SVG has zero size.
      div(id = "erd-out", class = "text-center small text-muted",
          style = "overflow-x:auto;", "rendering diagram…"),
      tags$h6(class = "mt-3", "Columns"),
      DT::dataTableOutput("schema_cols"),
      tags$script(HTML(sprintf("
        (function draw(){
          if (!window.mermaid) { setTimeout(draw, 120); return; }
          window.mermaid.render('erdSvg', %s).then(function(r){
            var el = document.getElementById('erd-out'); if (!el) return;
            el.classList.remove('text-muted'); el.innerHTML = r.svg;
            var svg = el.querySelector('svg');
            if (svg) {
              // scale the diagram down to a compact box (never upscale)
              var vb = svg.viewBox && svg.viewBox.baseVal;
              var maxW = el.clientWidth || 900;
              var maxH = Math.min(360, Math.round(window.innerHeight * 0.42));
              if (vb && vb.width && vb.height) {
                var s = Math.min(maxW / vb.width, maxH / vb.height, 1);
                svg.setAttribute('width',  Math.round(vb.width  * s));
                svg.setAttribute('height', Math.round(vb.height * s));
              }
              svg.style.maxWidth = '100%%';
            }
          }).catch(function(e){
            var el = document.getElementById('erd-out');
            if (el) el.innerHTML = '<div class=\"text-danger small\">ERD render error: ' +
              ((e && e.message) || e) + '</div>';
            console.error('mermaid ERD:', e);
          });
        })();", jsonlite::toJSON(erd_src(dark = is_dark()), auto_unbox = TRUE))))))
  }
  observeEvent(input$schema,      show_schema_modal())
  observeEvent(input$schema_link, show_schema_modal())

  output$schema_cols <- DT::renderDataTable({
    DT::datatable(
      SCHEMA_COLS, rownames = FALSE, filter = "top", style = "auto",
      colnames = c("Table", "Column", "Type", "Key", "Description"),
      class = "compact stripe hover",
      options = list(
        pageLength = 20, dom = "ftip", scrollX = TRUE, autoWidth = FALSE,
        columnDefs = list(list(className = "dt-nowrap", targets = 0:3)),
        order = list(list(0, "asc"))))
  }, server = FALSE)

  # --- light / dark theme: reskin the app + swap the basemap ----
  is_dark <- reactive((input$theme %||% "dark") == "dark")
  observeEvent(input$theme, {
    session$setCurrentTheme(
      bs_theme(version = 5, preset = if (is_dark()) "darkly" else "flatly"))
  }, ignoreInit = TRUE)

  # --- resolution: auto (zoom-driven) vs manual (pinned) ----
  auto_res <- reactive(zoom_res_capped(input$map_zoom %||% INIT_ZOOM))

  # auto mode: keep the vertical slider in step with the map zoom (programmatic
  # update — does NOT fire the grab handler, so it won't flip to manual)
  observe({
    if (isTRUE(input$res_manual)) return()
    session$sendInputMessage("res", list(value = auto_res()))   # move the native slider only
  })
  # the user grabbing the slider flags manual intent (mouse/touch down via JS)
  observeEvent(input$res_grab, {
    if (!isTRUE(input$custom_sql) && !isTRUE(input$res_manual))
      updateCheckboxInput(session, "res_manual", value = TRUE)
  }, ignoreInit = TRUE)
  # grey out the control only while custom SQL owns the resolution
  observe(shinyjs::toggleClass("res_col", "locked", condition = isTRUE(input$custom_sql)))

  output$res_lbl <- renderText({
    m <- isTRUE(input$res_manual) && !isTRUE(input$custom_sql)
    r <- if (m) input$res else auto_res()
    sprintf("res %d", as.integer(r %||% 1))
  })
  output$yr_lbl <- renderText({
    y <- input$years %||% c(YR_MIN, YR_MAX)
    if (identical(as.integer(y), c(YR_MIN, YR_MAX))) "(all)"
    else sprintf("%d–%d", as.integer(y[1]), as.integer(y[2]))
  })

  # --- fill opacity (0-1) — adjusts the layer paint only, never rebuilds tiles ----
  fill_opacity <- reactive(max(0, min(1, (input$opacity %||% (INIT_OPACITY * 100)) / 100)))
  output$fill_lbl <- renderText(sprintf("%d%%", as.integer(round(fill_opacity() * 100))))
  observeEvent(input$opacity, {
    maplibre_proxy("map") |>
      set_paint_property("obis_fill", "fill-opacity", fill_opacity())
  }, ignoreInit = TRUE)

  # --- filters -> SQL -> tiles ----
  taxon_r <- reactive({
    if (isTRUE(input$custom_taxon) && nzchar(input$taxon_val %||% ""))
      setNames(list(input$taxon_val), input$rank)
    else {
      p <- PRESETS[[input$preset]]
      if (length(p)) p else NULL
    }
  })
  years_r <- reactive({
    y <- input$years %||% c(YR_MIN, YR_MAX)
    if (identical(as.integer(y), c(YR_MIN, YR_MAX))) NULL else as.integer(y)
  })

  # custom SQL applies only on the Run button (not on every keystroke)
  sql_custom <- eventReactive(input$run_sql, input$sql, ignoreNULL = FALSE)

  sql_r <- reactive({
    if (isTRUE(input$custom_sql)) return(sql_custom())
    # reference input$res ONLY in manual mode, so zoom-driven slider syncs in
    # auto mode do NOT establish a dependency here (no needless tile rebuilds)
    res_ph <- if (isTRUE(input$res_manual)) as.character(input$res) else "{{res}}"
    obis_h3t_sql(
      indicator       = input$indicator,
      taxon           = taxon_r(),
      years           = years_r(),
      res_placeholder = res_ph)
  })

  tiles_r <- reactive(obis_h3t_url(base_url = TILES_BASE, sql = sql_r(), release = RELEASE))

  stats_r <- reactive({
    res_h3 <- if (isTRUE(input$res_manual) && !isTRUE(input$custom_sql)) min(input$res, 4L) else 4L
    get_stats(sql_r(), res_h3 = res_h3)
  })

  # bundle the data-affecting state; debounce so rapid edits coalesce. NOTE this
  # depends on filters/indicator/res(manual)/custom-SQL — NOT on map zoom/pan.
  state   <- reactive(list(
    sql = sql_r(), tiles = tiles_r(), stats = stats_r(), indicator = input$indicator))
  state_d <- debounce(state, 300)

  # --- map: re-renders only on THEME change (basemap swap); all data changes go
  #     through the proxy. View (center/zoom) is preserved across re-renders. ----
  output$map <- renderMaplibre({
    dark <- is_dark()                       # the only reactive dependency
    st   <- isolate(state())
    ramp <- ramp_of(st$stats, st$indicator)
    ctr  <- isolate(input$map_center); zm <- isolate(input$map_zoom)
    center <- if (!is.null(ctr)) c(ctr$lng, ctr$lat) else INIT_CENTER
    zoom   <- zm %||% INIT_ZOOM
    style  <- carto_style(if (dark) "dark-matter" else "positron")
    tip    <- tip_expr(st$indicator)
    maplibre(style = style, center = center, zoom = zoom) |>
      add_h3t_source(id = "obis", tiles = st$tiles) |>
      add_fill_layer(
        id = "obis_fill", source = "obis", source_layer = "obis",
        fill_color    = interpolate(column = "value", values = ramp$brks, stops = VIRIDIS5),
        fill_opacity  = isolate(fill_opacity()),
        tooltip = tip, popup = tip,
        tooltip_style = if (dark) "dark" else "light",
        popup_style   = if (dark) "dark" else "light") |>
      add_legend(ramp$lbl, values = round(c(ramp$lo, ramp$hi), 2), colors = VIRIDIS5,
                 position = "top-left") |>
      add_navigation_control(position = "bottom-right") |>
      add_scale_control(position = "bottom-left")
  })

  # --- proxy update: tear down + re-add source/layer/legend, view preserved ----
  observeEvent(state_d(), {
    st <- state_d(); req(st$tiles)
    ramp <- ramp_of(st$stats, st$indicator)
    dark <- is_dark()
    tip  <- tip_expr(st$indicator)
    maplibre_proxy("map") |>
      clear_layer(c("obis_fill", "obis")) |>          # layer first, then its source
      add_h3t_source(id = "obis", tiles = st$tiles) |>
      add_fill_layer(
        id = "obis_fill", source = "obis", source_layer = "obis",
        fill_color    = interpolate(column = "value", values = ramp$brks, stops = VIRIDIS5),
        fill_opacity  = isolate(fill_opacity()),
        tooltip = tip, popup = tip,
        tooltip_style = if (dark) "dark" else "light",
        popup_style   = if (dark) "dark" else "light") |>
      add_legend(ramp$lbl, values = round(c(ramp$lo, ramp$hi), 2), colors = VIRIDIS5,
                 position = "top-left", add = FALSE)
  }, ignoreInit = TRUE)

  # --- sidebar read-outs ----
  output$sql_txt <- renderText(state_d()$sql)
  output$url_txt <- renderText(state_d()$tiles)
  output$stats_ui <- renderUI({
    s <- state_d()$stats
    if (!is.null(s$error))
      return(div(class = "text-warning", strong("query error: "), s$reason %||% s$error))
    if (is.null(s$n) || is.na(s$n))
      return(em("no cells for this filter (check taxon / years / data extent)"))
    tags$table(
      class = "table table-sm",
      tags$tbody(
        tags$tr(tags$td("cells"),   tags$td(format(s$n, big.mark = ","))),
        tags$tr(tags$td("min"),     tags$td(signif(s$min, 4))),
        tags$tr(tags$td("max"),     tags$td(signif(s$max, 4))),
        tags$tr(tags$td("p02–p98"), tags$td(glue("{signif(s$p02,3)} – {signif(s$p98,3)}")))))
  })
}

shinyApp(ui, server)
