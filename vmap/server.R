function(input, output, session) {

  # get_d_mw_rs() ----
  # [d]ataframe of [m]aps & [w]eights, [r]egion & [s]eason
  get_d_mw_rs <- reactive({
    req(input$sel_rgn) # TODO: region
    req(input$sel_ssn)

    d_mw |>
      filter(
        season == input$sel_ssn)
  })

  # get_r_mw_rs() ----
  # [r]aster of [m]aps & [w]eights, [r]egion & [s]eason
  get_r_mw_rs <- reactive({

    eqn_r <- input$txt_eqn_r
    eqn_v <- input$txt_eqn_v

    n <- 4
    withProgress(
      message = 'Calculating: Cumulative Species Vulnerability',
      value   = 0, {

      incProgress(
        1/n,
        detail = "Process equation per species raster")
      d <- get_d_mw_rs() %>%
        mutate(
          r_e = pmap(., function(...) {
            with(list(...), {
              eval(parse(text = eqn_r)) } ) } ) )

      incProgress(
        2/n,
        detail = "Sum species rasters and project for display")
      res_r <- res(d$r[[1]]) # resolution
      r <- sum(rast(d$r_e)) |>
        terra::project("epsg:3857", method="bilinear", res=res_r)

      # apply equation to summed raster
      incProgress(
        3/n,
        detail = "Apply equation to summed raster")
      if (eqn_v > ""){
        v <- values(r)
        values(r) <- eval(parse(text = eqn_v))
      }

    })

    r
  })

  # get_d_mw_rs_spp() ----
  # species [spp] from [m]aps & [w]eights, [r]egion & [s]eason
  get_d_mw_rs_spp <- reactive({
    req(input$sel_spp)

    get_d_mw_rs() |>
      filter(
        sp_id == input$sel_spp)
  })

  # sel_ssn -> sel_spp ----
  observeEvent(input$sel_ssn, {
    req(input$sel_ssn)

    spp_ids <- get_d_mw_rs() |>
      pull(sp_id) |>
      unique()

    updateTreeInput(
      inputId  = "sel_spp",
      selected = spp_ids)
  })

  # tbl_mw ----
  output$tbl_mw_rs <- renderDT({

    get_d_mw_rs() |>
      select(
        !all_of(cols_mw_notdisplay), -sp_id) |>
      rename(
        sp_id = sp_id_html) |>
      relocate(
        sp_id, .before = sp_code) |>
      datatable(
        caption = tags$caption(
          glue("Table of Species Parameters matching Distribution Maps for
               {input$sel_rgn} region in the {input$sel_ssn} season"),
          style = "caption-side: top"),
        escape = F)
  })

  # map ----
  output$map <- renderLeaflet({

    r   <- get_r_mw_rs()
    o_r <- input$sldr_opacity_r
    o_b <- input$sldr_opacity_b

    pal <- colorNumeric(
      "Spectral", values(r),
      na.color = "transparent", reverse = T)

    leaflet() |>
      addProviderTiles(
        "Esri.OceanBasemap",
        options = providerTileOptions(
          opacity = o_b)) |>
      addRasterImage(
        r,
        project = F,
        colors  = pal,
        opacity = o_r) |>  # TODO: shiny input$opacity
      addLegend(
        pal    = pal,
        values = values(r),
        title  = glue(
          "Cumulative<br>
          Species<br>
          Vulnerability<br>
          in {input$sel_ssn}"))

  })


}