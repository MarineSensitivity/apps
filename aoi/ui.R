page_sidebar(
  title = "AoI Explorer",
  # theme = bs_theme(
  #   version               = 5,
  #   bootswatch            = "darkly",
  #   "font-size-base"      = "0.8rem",
  #   "navbar-padding-y"    = "0",
  #   "navbar-padding-x"    = "0",
  #   "container-padding-x" = "0"),

  sidebar = sidebar(
    # title = "Selection",
    selectInput(
      "sel_aois", "Areas of Interest",
      lst_aois,
      "public.ply_rgns"),
    textOutput("txt_status")
    # helpText(
    #   "Select an existing Shape or Draw a polygon (toolbar at top of Map)
    # to filter to species and view as interactive Table or treemap Plot. All
    # species data are currently from AquaMaps.")
    ),

  tags$head(tags$link(rel="stylesheet", type="text/css", href="styles.css")),

  navset_card_pill(
    placement = "above",
    nav_panel(
      title = "Map",
      # leafletOutput("map")
      rdeckOutput("map", width = "100vw", height = "100vh") )
    # nav_panel(
    #   title = "Table",
    #   helpText("amt = n_cells * avg_pct * avg_prob"),
    #   helpText("Amount (amt) is the multiplication of the number of cells (n_cells),
    #            average percent (avg_pct) of a cell's contents within the selected polygon,
    #            and the average Suitability (avg_suit; 0 to 1) of the species given by AquaMaps."),
    #   dataTableOutput("tbl_spp") ),
    # nav_panel(
    #   title = "Plot",
    #   helpText("Note: The rendering of this treeamp plot is SLOW if # species > 1,000. Use toolbar on left of map to draw a smaller area with fewer species."),
    #   plotlyOutput("plt_spp") )

  ) )

