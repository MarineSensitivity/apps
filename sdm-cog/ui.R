page_sidebar(
  title = "SDM Raster Explorer",


  sidebar = sidebar(
    useShinyjs(),
    selectInput("sel_ds"   , "Dataset" , v_ds),
    selectInput("sel_sp"   , "Species" , choices = NULL),
    selectInput("sel_intvl", "Season"  , choices = NULL),
    selectInput("sel_var",   "Variable", choices = NULL),
    helpText(
      "Legend missing for now.", br(),
      textOutput("txt_legend") ) ),

  tags$head(
    tags$link(rel="stylesheet", type="text/css", href="styles.css"),
    tags$script(
      "document.addEventListener('DOMContentLoaded', function() {
      });") ),

  navset_card_pill(
    id        = "nav",
    placement = "above",
    nav_panel(
      "Map",
      rdeckOutput("map", width = "100vw", height = "100vh") ) ) )
