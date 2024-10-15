function(input, output, session) {

  # get_sp_ids() ----
  get_sp_ids <- reactive({
    # TODO: check for sp_id selected not applicable to available region-season

    input$sel_spp |>
      str_subset("GBIF:") |>
      sort()
  })

  # get_d_mw_rs() ----
  # [d]ataframe of [m]aps & [w]eights, [r]egion & [s]eason
  get_d_mw_rs <- reactive({
    req(input$sel_rgn) # TODO: region
    req(input$sel_ssn)

    sp_ids <- get_sp_ids()

    d_mw |>
      filter(
        season == input$sel_ssn,
        sp_id %in% sp_ids)
  })


  # get_m() ----
  get_m <- reactive({

    # check validity of input equation functions
    # DEBUG
    # input <- list(
    #   txt_eqn_r = "
    #     terra::scale(r) * (
    #           best_estimate_final_population_sensitivity +
    #           best_estimate_final_collision_sensitivity_rank +
    #           best_estimate_final_displacement_sensitivity_rank )",
    #   txt_eqn_v = "scales::rescale(v, c(0, 100))")
    allowed_fxns <-c("terra::scale", "scale",
                     "scales::rescale", "rescale",
                     "c", "mean", "sum")
    eqn_r_fxns  <- input$txt_eqn_r |>
      str_extract_all("[[:graph:]]+(?=\\()") |> unlist()
    eqn_v_fxns  <- input$txt_eqn_v |>
      str_extract_all("[[:graph:]]+(?=\\()") |> unlist()
    stopifnot(all(eqn_r_fxns %in% allowed_fxns))
    stopifnot(all(eqn_v_fxns %in% allowed_fxns))

    # metadata object from Configure tab
    m <- list(
      region = input$sel_rgn,
      season = input$sel_ssn,
      sp_ids = get_sp_ids(),
      eqn_r  = input$txt_eqn_r,
      eqn_v  = input$txt_eqn_v)
    # hash to uniquely identify raster
    h <- digest(m, algo="crc32")
    # output raster and metadata
    tif <- glue("{dir_cache}/vmap_{h}.tif")
    yml <- glue("{dir_cache}/vmap_{h}.yml")

    list(
      m   = m,
      h   = h,
      tif = tif,
      yml = yml)
  })

  # get_r_mw_rs() ----
  # [r]aster of [m]aps & [w]eights, [r]egion & [s]eason
  get_r_mw_rs <- reactive({

    eqn_r <- input$txt_eqn_r
    eqn_v <- input$txt_eqn_v

    m <- get_m()

    if (file.exists(m$tif)){
      r <- rast(m$tif)
      return(r)
    }

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

    writeRaster(r, m$tif)
    write_yaml(m$m, m$yml)

    r
  })

  # downloadData ----
  output$downloadData <- downloadHandler(
    filename = function() {
      m <- get_m()
      glue("vmap_{m$h}.zip")
    },
    content = function(fname) {
      m <- get_m()
      setwd(dir_cache)
      zip(zipfile=fname, files=basename(c(m$tif, m$yml)))
    },
    contentType = "application/zip"
  )

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

    d <- get_d_mw_rs() |>
      select(
        !all_of(cols_mw_notdisplay), -sp_id) |>
      rename(
        sp_id = sp_id_html) |>
      relocate(
        sp_id, sp_common, sp_scientific, .before = scientificName) |>
      relocate(
        study_param, .after = infraspecificEpithet)

    # browser()

    flds_m <- names(d)[1:4]
    flds_s <- names(d)[5:17]
    flds_w <- names(d)[18:27]

    hdr = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, 'n'),
          th(colspan = length(flds_m), 'Map'),
          th(colspan = length(flds_s), 'Species'),
          th(colspan = length(flds_w), 'Parameters')
        ),
        tr(
          lapply(c(flds_m, flds_s, flds_w), th) ) ) ) )

    d |>
      datatable(
        container = hdr,
        escape    = F,
        class     = "cell-border compact stripe",
        caption   = tags$caption(
          glue("Table of Species Parameters matching Distribution Maps for
               {input$sel_rgn} region in the {input$sel_ssn} season"),
          # style = "caption-side: top; text-align: center;"))
          style = "caption-side: top"))
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
