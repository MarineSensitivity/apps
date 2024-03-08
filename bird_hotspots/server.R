# Define server logic
function(input, output, session) {

  rx <- reactiveValues(
    cells = NULL)

  pal <- colorNumeric(
    palette = "inferno",
    domain  = hhvh$numhot)

  m = get_basemap() |>
    addPolygons(
      data         = hhvh,
      fillColor    = ~pal(numhot), fillOpacity = 0.7,
      color        = "white",      opacity     = 0.9, weight = 0.1,
      smoothFactor = 0.2)

  drawn <- callModule(
    editMod, "map", m,
    editorOptions = list(
      singleFeature       = T,
      polylineOptions     = F,
      markerOptions       = F,
      circleMarkerOptions = F))

  observe({
    req(drawn()$finished)
    rx$cells <- hhvh |>
      st_intersection(drawn()$finished)
    # TODO: consider calculating area, and applying area-weighted average to values
  })

  output$plot <- renderPlotly({
    req(rx$cells)

    d <- rx$cells |>
      st_drop_geometry() |>
      select(-ID, -numhot, -X_leaflet_id, -feature_type) |>
      pivot_longer(
        cols      = everything(),
        names_to  = "sp",
        values_to = "v") |>
      group_by(sp) |>
      summarize(
        mean = mean(v),
        n    = n(),
        se   = sd(v) / sqrt(n)) |>
      mutate(
        margin = qt(p = (1 - conf_level) / 2, df = n - 1, lower.tail = F) * se,
        upper  = mean + margin,
        lower  = mean - margin,
        lower  = if_else(lower < 0, 0, lower) ) |>  # truncate lower CI at 0
      arrange(desc(mean)) |>
      filter(mean != 0)

    g <- ggplot(d, aes(x = reorder(sp, -mean), y = mean, fill = sp)) +
      geom_col() +
      geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
      labs(x = "Species", y = "Avg Hotspot Probability", fill = "Species") +
      theme_bw() +
      coord_flip() +
      theme(legend.position = "none")
    # TODO: flag (or drop) species where lower==0 so mean is not meaningful

    # TODO:
    # - add species names to bars
    # - expand plot to full width as a new tab
    # - visually indicate spp w/ lower CI == 0:
    #   - darken background
    #   - move it all to a sortable table
    # - + Sort by: species name, or hotspot probability
    # - checkbox to include Zeros
    # - classify into guilds
    # - Select Species (or hotspot) to map
    # - Add legend

    ggplotly(g)
  })

}
