shinyServer(function(input, output, session) {
  # map ----
  output$map <-  renderLeaflet({

    pal <- colorFactor("Spectral", ply_rgns_s05$rgn_key)
    # ply_rgns_s05$rgn_key = factor(ply_rgns_s05$rgn_key)
    # pal <- colorRampPalette(brewer.pal(11,name = 'Spectral'))(length(unique(ply_rgns_s05$rgn_key)))
    # previewColors(pal, ply_rgns_s05$rgn_key)

    ms_basemap() |>
      addPolygons(
        data        = ply_rgns_s05,
        layerId     = ~rgn_key,
        color       = "gray",
        opacity     = 0.8,
        weight      = 1,
        fillColor   = ~pal(rgn_key),
        fillOpacity = 0.7,
        label       = ~rgn_name,
        highlightOptions = highlightOptions(
          weight       = 2,
          color        = "black",
          fillOpacity  = 0.8,
          opacity      = 0.9,
          bringToFront = T)) |>
      addDrawToolbar(
        targetGroup         = 'draw',
        polylineOptions     = F,
        circleMarkerOptions = F,
        editOptions         = editToolbarOptions(
          selectedPathOptions = selectedPathOptions()))

  })

  # * map zoom to selected region ----
  observe({
    # require selection from drop-down list
    req(input$sel_rgn)

    # zoom to selected region
    b <- ply_rgns_s05 |>
      filter(
        rgn_key == input$sel_rgn) |>
      st_bbox()
    leafletProxy("map") |>
      flyToBounds(b[['xmin']], b[['ymin']], b[['xmax']], b[['ymax']])
  })

  # * map zoom to clicked region ----
  observe({
    # require click on region in map
    req(input$map_shape_click$id)

    # zoom to selected region
    b <- ply_rgns_s05 |>
      filter(
        rgn_key == input$map_shape_click$id) |>
      st_bbox()
    leafletProxy("map") |>
      flyToBounds(b[['xmin']], b[['ymin']], b[['xmax']], b[['ymax']])
  })

  # * react to drawn region ----
  observeEvent(input$map_draw_new_feature, {
    feature <- input$map_draw_new_feature
    if(feature$geometry$type == "Polygon") {
      ply <- st_read(as.json(feature$geometry), quiet=T)
      output$polygon_data <- renderPrint({
        ply
      })
    }
  })

})
