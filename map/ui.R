dashboardPage(
  dashboardHeader(
    title = "Marine Sensitivities Map"),
  dashboardSidebar(
    selectInput(
      "sel_rgn", "Region",
      lst_rgns),
    collapsed = F),
  dashboardBody(
    tags$head(tags$link(rel="stylesheet", type="text/css", href="styles.css")),
    leafletOutput("map")
    ))
