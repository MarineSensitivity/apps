page_fillable(
    title   = "Bird Hotspots",
    sidebar = sidebar(open = "closed"),
    layout_columns(
      card(
        full_screen = T,
        card_header("Map"),
        editModUI("map") ),
      card(
        full_screen = T,
        card_header("Plot"),
        plotlyOutput("plot") ) ) )
