fluidPage(

  fluidRow(
    column(6, editModUI("map_select")),
    column(6, leafletOutput("map_out")) ) )
