page_sidebar(
  title = "SDM Explorer",


  sidebar = sidebar(
    useShinyjs(),
    helpText("Collapse to see legend."),
    selectInput("sel_ds", "Dataset", v_ds),
    selectInput("sel_sp"  ,  "Species"   , choices = NULL),
    selectInput("sel_popn",  "Population", choices = NULL),
    selectInput("sel_intvl", "Time"      , choices = NULL),
    selectInput("sel_var",   "Variable"  , choices = NULL) ),

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
