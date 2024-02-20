# Define server logic
function(input, output, session) {

  sel_cells <- reactiveValues()

  pal <- colorNumeric(
    palette = "inferno",
    domain  = hhvh$numhot)

  m = get_basemap() |>
    # https://rstudio.github.io/leaflet/articles/choropleths.html
    addPolygons(
      data         = hhvh,
      fillColor    = ~pal(numhot), fillOpacity = 0.7,
      color        = "white",      opacity     = 0.9, weight = 0.5,
      smoothFactor = 0.5)

  # https://github.com/r-spatial/mapedit/blob/35ae8ccfb90b682bf98a82e85c3047f098857db3/inst/examples/shiny_modules.R#L46C1-L71C20
  drawn <- callModule(editMod, "map_select", m)

  observe({
    req(drawn()$finished)
    sel_cells$intersection <- st_intersection(drawn()$finished, hhvh)
  })

  output$map_out <- renderLeaflet({
    req(sel_cells$intersection)

    (mapview(sel_cells$intersection) + mapview(drawn()$finished))@map
  })

}
