page_sidebar(
  title = "Scores Explorer",

  sidebar = sidebar(
    selectInput(
      "sel_aois", "Areas of Interest",
      lst_aois,
      "public.ply_ep_planareas"),
    helpText(
      "Click on an area of interest in the map to show the flower plot."),
    girafeOutput("plot_flower_small")),

  tags$head(tags$link(rel="stylesheet", type="text/css", href="styles.css")),

  navset_card_pill(
    id        = "nav",
    placement = "above",
    nav_panel(
      "Map",
      rdeckOutput("map") ),
    nav_panel(
      "Plot",
      helpText("Click on an area of interest in the map to view the flower plot of taxonomic scores."),
      girafeOutput("plot_flower_big") ),
    nav_panel(
      "Table",
      helpText("Click on an area of interest in the map to view the table of species."),
      DTOutput("tbl_spp") ) )

  )

