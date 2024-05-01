shinyServer(function(input, output, session) {

  # ui_mdl: ds > sp > popn > time > var ----

  vals <- reactiveValues(
    cog_rng = "")

  #
  output$txt_legend <- renderText({
    req(input$sel_var, vals$cog_rng)

    v <- vals$cog_rng
    glue("{input$sel_var}: {paste(round(v, 3), collapse = ' to ')} (violet to red)")
  })

  # * rx_ds ----
  rx_ds <- reactive({
    req(input$sel_ds)
    ds_key <- input$sel_ds

    # ds_key = "nc_atl_birds_dens"
    d_mdls |>
      filter(ds_key == !!ds_key)
  })

  # * rx_ds -> sel_sp ----
  observeEvent(rx_ds(), {
    choices <- rx_ds() |>
      distinct(sp_code) |>
      pull(sp_code) |>
      sort()

    updateSelectInput(
      inputId = "sel_sp",
      choices = choices)
  })

  # * rx_sp <- rx_ds ----
  rx_sp <- reactive({
    req(input$sel_sp)
    sp_code <- input$sel_sp

    rx_ds() |>
      filter(sp_code == !!sp_code)
  })

  # * rx_sp -> sel_intvl ----
  observeEvent(rx_sp(), {

    choices <- rx_sp() |>
      distinct(season) |>
      pull(season) |>
      sort()

    updateSelectInput(
      inputId = "sel_intvl",
      choices = choices)
  })

  # * rx_intvl <- rx_popn  ----
  rx_intvl <- reactive({
    req(input$sel_intvl)
    intvl <- input$sel_intvl

    rx_sp() |>
      filter(season == !!intvl)
  })

  # * rx_intvl -> sel_var ----
  observeEvent(rx_intvl(), {

    choices <- rx_intvl() |>
      arrange(bidx) |>
      distinct(var) |>
      pull(var)

    updateSelectInput(
      inputId = "sel_var",
      choices = choices)
  })

  # map ----
  output$map <- renderRdeck({

    req(
      input$sel_ds, input$sel_sp) # input$sel_popn, input$sel_intvl, input$sel_sp)

    ds_key  <- input$sel_ds
    sp_code <- input$sel_sp
    intvl   <- ifelse(is.na(input$sel_intvl) || input$sel_intvl == "", NA, input$sel_intvl)
    var     <- ifelse(is.na(input$sel_var)   || input$sel_var   == "", NA, input$sel_var)

    if (verbose)
      c("map",
        "\n  ds_key: ",  input$sel_ds,
        "\n  sp_code: ", input$sel_sp,
        "\n  intvl: ",   input$sel_intvl,
        "\n  var: ",     input$sel_var) |>
        message()

    t_mdl <- d_mdls |>
      filter(
        ds_key  == !!ds_key,
        sp_code == !!sp_code,
        season  == !!intvl,
        var     == !!var)

    n_mdls <- t_mdl |>
      summarize(n = n()) |>
      pull(n)
    req(n_mdls == 1)

    cog_url        = t_mdl$cog_url
    bidx           = t_mdl$bidx

    titiler_endpoint = "https://titiler.xyz"

    cog_bb <- request(titiler_endpoint) |>
      req_url_path_append("cog", "bounds") |>
      req_url_query(
        url = cog_url,
        bidx = bidx) |>
      req_perform() |>
      resp_body_json() |>
      unlist() |>
      as.numeric()
    # cog_bb

    cog_rng <- request(titiler_endpoint) |>
      req_url_path_append("cog", "statistics") |>
      req_url_query(
        url  = cog_url,
        bidx = bidx) |>
      req_perform() |>
      resp_body_json() |>
      pluck(glue("b{bidx}")) |>
      keep_at(~ .x %in% c("min", "max")) |>
      as.numeric()
    # cog_rng
    vals$cog_rng <- cog_rng

    cog_method     = "bilinear"
    cog_palette    = "spectral_r"
    lgnd_palette   = "Spectral"
    lgnd_palette_r = T

    tile_opts <- glue(
      "bidx={bidx}&expression=b{bidx}&
      unscale=false&
      resampling={cog_method}&reproject={cog_method}&return_mask=true&
      rescale={paste(cog_rng, collapse=',')}&
      colormap_name={cog_palette}") |> str_replace_all("\\s+", "")
    sdm_tile_url  <- glue(
      "{titiler_endpoint}/cog/tiles/WebMercatorQuad/{{z}}/{{x}}/{{y}}@2x.png?url={cog_url}&{tile_opts}")

    if (verbose)
      message("sdm_tile_url: ", URLdecode(sdm_tile_url))

    bb <- cog_bb

    rdeck(
      map_style      = mapbox_dark(),
      theme          = "light",
      initial_bounds = st_bbox(
        c(xmin=bb[1], ymin=bb[2], xmax=bb[3], ymax=bb[4]),
        crs = st_crs(4326) ) )  |>
      add_tile_layer(
        id              = "sdm",
        name            = "s dm",
        opacity         = 0.5,
        data            = sdm_tile_url,
        visible         = T)
      # TODO: get_tile_data
      #   <JS> retrieves the data of each tile. See deck.gl getTileData.

  })

})
