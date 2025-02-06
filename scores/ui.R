page_sidebar(
  title = "Scores Explorer",

  sidebar = sidebar(
    selectInput(
      "sel_aois", "Areas of Interest",
      lst_aois,
      "public.ply_ep_planareas"),
    # uiOutput("htm_status"),
    helpText(
      "Click on an area of interest in the map to show the flower plot."),
    girafeOutput("plot_flower_small")),

  tags$head(tags$link(rel="stylesheet", type="text/css", href="styles.css")),

  navset_card_pill(
    id        = "nav",
    placement = "above",
    nav_panel(
      "Map",
      # leafletOutput("map")
      rdeckOutput("map") ),
    nav_panel(
      "Plot",
      helpText("Click on an area of interest in the map to view the flower plot of taxonomic scores."),
      girafeOutput("plot_flower_big") ),
    nav_panel(
      "Table",
      helpText("Click on an area of interest in the map to view the table of species."),
      dataTableOutput("tbl_spp") ) )
    # nav_panel(
    #   title = "Plot",
    #   helpText("Note: The rendering of this treeamp plot is SLOW if # species > 1,000. Use toolbar on left of map to draw a smaller area with fewer species."),
    #   plotlyOutput("plt_spp") )

  )

