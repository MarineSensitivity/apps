page_sidebar(
  title = "SDM Explorer",


  sidebar = sidebar(
    useShinyjs(),
    helpText("Collapse to see legend."),
    selectInput("sel_ds", "Dataset", v_ds),
    selectInput("sel_sp"  ,  "Species"   , choices = NULL),
    selectInput("sel_popn",  "Population", choices = NULL, selectize = F),
    selectInput("sel_intvl", "Time"      , choices = NULL, selectize = F),
    selectInput("sel_var",   "Variable"  , choices = NULL, selectize = F) ),

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
