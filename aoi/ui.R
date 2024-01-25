page_sidebar(
  title = "Area Explorer",

  sidebar = sidebar(
    selectInput(
      "sel_aois", "Areas of Interest",
      lst_aois,
      "public.ply_rgns"),
    uiOutput("htm_status")
    # helpText(
    #   "Select an existing Shape or Draw a polygon (toolbar at top of Map)
    # to filter to species and view as interactive Table or treemap Plot. All
    # species data are currently from AquaMaps.")
    ),

  tags$head(tags$link(rel="stylesheet", type="text/css", href="styles.css")),

  navset_card_pill(
    id        = "nav",
    placement = "above",
    nav_panel(
      "Map",
      # leafletOutput("map")
      rdeckOutput("map", width = "100vw", height = "100vh") ) )
    # nav_panel(
    #   title = "Plot",
    #   helpText("Note: The rendering of this treeamp plot is SLOW if # species > 1,000. Use toolbar on left of map to draw a smaller area with fewer species."),
    #   plotlyOutput("plt_spp") )

  )

