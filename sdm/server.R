shinyServer(function(input, output, session) {

  # ui_mdl: ds > sp > popn > time > var ----

  # * rx_ds ----
  rx_ds <- reactive({
    req(input$sel_ds)
    ds_key <- input$sel_ds

    tbl(con, "sdm_models") |>
      filter(ds_key == !!ds_key)
  })

  # * rx_ds -> sel_sp ----
  observeEvent(rx_ds(), {
    choices <- rx_ds() |>
      distinct(sp_key) |>
      pull(sp_key) |>
      sort()

    updateSelectInput(
      inputId = "sel_sp",
      choices = choices)
  })

  # * rx_sp <- rx_ds ----
  rx_sp <- reactive({
    req(input$sel_sp)
    sp_key <- input$sel_sp

    rx_ds() |>
      filter(sp_key == !!sp_key)
  })

  # * rx_sp -> sel_popn ----
  observeEvent(rx_sp(), {

    if (verbose)
      message("rx_sp > sel_popn BEG input$sel_popn: ", isolate(input$sel_popn))

    choices <- rx_sp() |>
      distinct(population) |>
      pull(population) |>
      sort()

    updateSelectInput(
      inputId  = "sel_popn",
      choices  = choices,
      selected = NULL)

    if (verbose)
      message("rx_sp > sel_popn END input$sel_popn: ", isolate(input$sel_popn))

    toggle("sel_popn", length(choices) > 0)
  })

  # * rx_popn <- rx_sp  ----
  rx_popn <- reactive({
    # req(input$sel_popn)
    popn <- input$sel_popn

    if (verbose)
      message("rx_popn < rx_sp popn: ", popn)

    if (popn == "" || is.null(popn))
      return(rx_sp())

    rx_sp() |>
      filter(population == !!popn)
  })

  # * rx_popn -> sel_intvl ----
  observeEvent(rx_popn(), {

    intvls <- rx_popn() |>
      distinct(time_interval) |>
      pull(time_interval) |>
      sort()

    mos <- intvls |>
      str_replace("([0-9]{4})-([0-9]{2})/P1M", "\\2") |>
      as.integer()

    choices <- setNames(
      intvls,
      glue("{month.abb[mos]}") )

    updateSelectInput(
      inputId = "sel_intvl",
      choices = choices)
  })

  # * rx_intvl <- rx_popn  ----
  rx_intvl <- reactive({
    req(input$sel_intvl)
    intvl <- input$sel_intvl

    rx_popn() |>
      filter(time_interval == !!intvl)
  })

  # * rx_intvl -> sel_var ----
  observeEvent(rx_popn(), {

    choices <- rx_popn() |>
      distinct(var) |>
      pull(var) |>
      sort()

    updateSelectInput(
      inputId = "sel_var",
      choices = choices)
  })

  # map ----
  output$map <- renderRdeck({

    req(
      input$sel_ds, input$sel_sp) # input$sel_popn, input$sel_intvl, input$sel_sp)

    ds_key <- input$sel_ds
    sp_key <- input$sel_sp
    popn   <- ifelse(is.na(input$sel_popn)  || input$sel_popn  == "", NA, input$sel_popn)
    intvl  <- ifelse(is.na(input$sel_intvl) || input$sel_intvl == "", NA, input$sel_intvl)
    var    <- ifelse(is.na(input$sel_var)   || input$sel_var   == "", NA, input$sel_var)

    if (verbose)
      c("map",
        "\n  ds_key: ", input$sel_ds,
        "\n  sp_key: ", input$sel_sp,
        "\n  popn: ",   input$sel_popn,
        "\n  intvl: ",  input$sel_intvl,
        "\n  var: ",    input$sel_var) |>
        message()

    t_mdl <- tbl(con, "sdm_models") |>
      filter(
        ds_key        ==   !!ds_key,
        sp_key        ==   !!sp_key,
        (is.na(!!popn)  || population    == !!popn),
        (is.na(!!intvl) || time_interval == !!intvl),
        (is.na(!!var)   || var           == !!var) )

    n_mdls <- t_mdl |>
      summarize(n = n()) |>
      pull(n)
    req(n_mdls == 1)

    val_rng <- t_mdl |>
      inner_join(
        tbl(con, "sdm_values") |>
          filter(),
        by = c("ds_key", "mdl_id")) |>
      filter(!is.null(val)) |>
      pull(val) |>
      range()

    params <- list(
      dataset_key = ds_key,
      species_key = sp_key,
      popn        = popn,
      model_time  = intvl,
      variable    = var ) |>
      discard(is.na)

    req <- request(
      base_url = "https://tile.marinesensitivity.org/public.sdm_spatial/{z}/{x}/{y}.pbf") |>
      req_url_query(!!!params)

    if (verbose)
      message("req$url: ", req$url)

    rdeck(
      map_style      = mapbox_dark(),
      theme          = "kepler", # kepler", # "light",
      layer_selector = F,
      editor         = F, # TODO: turn on editor for arbitrary draw & extract
      initial_bounds = st_bbox(
        c(xmin=b[1], ymin=b[2], xmax=b[3], ymax=b[4]),
        crs = st_crs(4326)) ) |>
      add_mvt_layer(
        id                = "sdm",
        name              = var,
        data              = as.character(req$url),
        auto_highlight    = T,
        pickable          = T,
        tooltip           = val,
        visible           = T,
        # visibility_toggle = F,
        opacity           = 0.7,
        # line_width_scale  = 5,
        # line_width_units  = "meters", # "pixels"
        # get_line_color    = "#A9A9A9",
        line_width_scale  = 0,
        get_fill_color    = scale_color_linear(
          val,
          limits = val_rng) ) # |>
      # TODO: move legend to left side
      # htmlwidgets::onRender("
      #   function(el, x) {
      #     console.log('onRender BEG');
      #     const legend_div = document.querySelector('div[class^=\"_legend\"]');
      #     if (legend_div){
      #       console.log('onRender legend_div');
      #       legend_div.style.width = '80px';
      #       const container_div = legend_div.closest('div[class^=\"_control-container\"]');
      #       if (container_div){
      #           console.log('onRender container_div');
      #           container_div.style.right = '';
      #           container_div.style.left  = '10px'; } } }")
  })

})
