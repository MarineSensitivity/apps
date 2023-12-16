shinyServer(function(input, output, session) {

  # rxvals ----
  rxvals <- reactiveValues(
    d_spp = d_spp_g,
    ply   = ply_g)

  # map ----
  output$map <-  renderLeaflet({

    # pal for regions
    # pal <- colorFactor(brewer.pal(11, "Spectral"), ply_rgns_s05$rgn_key)
    # previewColors(pal, ply_rgns_s05$rgn_key)

    # pal for raster
    pal <- colorNumeric(
      "Spectral",
      values(r_nspp_3857, na.rm = T),
      na.color = "transparent",
      reverse = T)

    ms_basemap(base_opacity = 0.7) |>
      addRasterImage(
        r_nspp_3857,
        project = F,
        colors  = pal,
        opacity = 0.9) |>
      addPolygons(
        data        = ply_rgns_s05,
        layerId     = ~rgn_key,
        color       = "lightgray",
        opacity     = 0.8,
        weight      = 2,
        # fillColor   = ~pal(rgn_key),
        # fillOpacity = 0.7,
        fillOpacity = 0,
        label       = ~glue("{rgn_key}: {rgn_name}"),
        highlightOptions = highlightOptions(
          weight       = 4,
          color        = "black",
          fillOpacity  = 0,
          opacity      = 0.9,
          bringToFront = T)) |>
      addLegend(
        pal      = pal,
        values   = values(r_nspp_3857),
        title    = "# species",
        position = "bottomright") |>
      addDrawToolbar(
        targetGroup         = 'draw',
        polylineOptions     = F,
        circleOptions       = F,
        circleMarkerOptions = F,
        markerOptions       = F,
        editOptions         = editToolbarOptions(
          selectedPathOptions = selectedPathOptions()),
        singleFeature = T) |>
      setView(-106, 45, 4)

  })

  # * map zoom and center ----
  # observe({
  #   message("map_zoom: ", input$map_zoom, "; map_center: ", paste(input$map_center, collapse=", "))
  # })

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

  # * drawn ply -> rxvals$ply ----
  observeEvent(input$map_draw_new_feature, {
    feature <- input$map_draw_new_feature

    stopifnot(feature$geometry$type == "Polygon")

    rxvals$ply <- st_read(as.json(feature$geometry), quiet=T)
    # write_sf(rxvals$ply, "data/polygon_data.geojson", delete_dsn=T)
  })

  # * rx.ply -> rx.d_spp ----
  observeEvent(rxvals$ply, {
    rxvals$d_spp <- am_spp_in_ply(rxvals$ply)
    updateActionButton(
      session,
      "btn_spp",
      glue("Show species (n={format(nrow(rxvals$d_spp), big.mark = ',')})"))
  })

  # spp modal ----
  observeEvent(input$btn_spp, {
    showModal(modalDialog(
      title = "Species",
      size = "xl",
      tabsetPanel(
        id = "tabs_spp",
        tabPanel(
          "Table",
          helpText("n = n_cells * avg_weight * avg_probability"),
          dataTableOutput("tbl_spp")),
        tabPanel(
          "Treemap",
          helpText("Note: Treeamp rendering is SLOW if species n > 1,000. Use toolbar on left of map to draw a smaller area with fewer species."),
          plotlyOutput("plt_spp")) )))})

  # * tbl_spp ----
  output$tbl_spp <- renderDataTable({
    rxvals$d_spp |>
      datatable(
        extensions = c("Buttons", "FixedColumns"),
        options = list(
          scrollX      = T,
          pageLength   = 10,
          dom          = "Blfrtip",
          buttons      = c("copy", "csv", "excel", "pdf", "print"),
          fixedColumns = T ),
        rownames = F)  |>
      formatRound(
        columns = c("n_cells", "avg_weight", "avg_probability", "n"),
        digits  = 2) },
    server = F)

  # * plt_spp ----
  output$plt_spp <- renderPlotly({

    rxvals$d_spp |>
      select(phylum, class, order, family, genus, species, n) |>
      count_to_treemap() })

})
