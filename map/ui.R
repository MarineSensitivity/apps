dashboardPage(
  dashboardHeader(
    title = "MarineSensitivity Map"),
  dashboardSidebar(
    selectInput(
      "sel_rgn", "Region",
      lst_rgns),
    actionButton(
      "btn_spp",
      glue("Show species (n={format(nrow(d_spp_g), big.mark = ',')})")),
    collapsed = F),
  dashboardBody(
    tags$head(tags$link(rel="stylesheet", type="text/css", href="styles.css")),
    leafletOutput("map") ) )
