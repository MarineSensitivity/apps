shinyServer(function(input, output, session) {

  # rx: reactive values ----
  rx <- reactiveValues(
    clicked = NULL,
    status  = NULL)

  # map ----
  output$map <- renderRdeck({

    req(input$sel_aois)

    # get extent of features in db  # input = list(sel_aois = "public.ply_shlfs_s05")
    schema_tbl <- input$sel_aois
    pts        <- str_split(schema_tbl, "\\.")[[1]]
    schema     <- pts[1]
    tbl        <- pts[2]
    fld_geom   <- ifelse(schema == "public", "geometry", "geom")

    # * get extent of features in db ----
    # TODO: use faster ST_EstimatedExtent(). For PostgreSQL >= 8.0.0 statistics are gathered by VACUUM ANALYZE and the result extent will be about 95% of the actual one. For PostgreSQL < 8.0.0 statistics are gathered by running update_geometry_stats() and the result extent is exact.
    if (schema_tbl == "public.ply_rgns"){
      b <- c(-193.3, 14.8, -14.7, 74.9)
    } else {
      b <- dbGetQuery(con, glue(
        # "SELECT ST_EstimatedExtent('{schema}', '{tbl}', '{fld_geom}');")) |>
        "SELECT ST_Extent({fld_geom}) AS ext FROM {schema_tbl}")) |>
        pull(ext) |>
        str_replace_all("BOX\\((.*)\\)", "\\1") |>
        str_split("[ ,]") %>%
        .[[1]] |>
        as.numeric()  # xmin, ymin, xmax, ymax
    }
    # message(glue("using extent: {paste(b, collapse=', ')}"))

    # * get fields to display ----
    flds <<- dbGetQuery(con, glue(
      "SELECT column_name FROM information_schema.columns
      WHERE
        table_schema = '{schema}' AND
        table_name   = '{tbl}';")) |>
      pull(column_name)
    # message(glue("with flds: {paste(flds, collapse=', ')}"))

    rdeck(
      map_style      = mapbox_dark(),
      theme          = "light",
      initial_bounds = st_bbox(
        c(xmin=b[1], ymin=b[2], xmax=b[3], ymax=b[4]),
        crs = st_crs(4326)),
      editor = T)  |>
      add_tile_layer(
        id                = "nspp",
        name              = "nspp",
        visible           = F,
        visibility_toggle = T,
        opacity           = 0.5,
        data              = nspp_tile_url) |>
      add_mvt_layer(
        id                = "aoi",
        name              = "aoi",
        data              = glue("https://tile.marinesensitivity.org/{schema_tbl}/{{z}}/{{x}}/{{y}}.pbf"),
        auto_highlight    = T,
        pickable          = T,
        tooltip           = all_of(!!flds), # c(mms_region, opd_name, prot_aprv, prot_numbe),
        # max_zoom          = 10,
        visibility_toggle = T,
        opacity           = 0.5,
        line_width_scale  = 1,
        line_width_units  = "pixels",
        get_fill_color    = "#0000FF80",  # blue 0.5 opacity
        get_line_color    = "#0000FFCC")  # blue 0.8 opacity

  })

  # txt_status ----
  output$htm_status <- renderUI({
    req(rx$status)
    h6(code(rx$status))
  })

  # * get clicked ----
  observe({
    d_c <- rdeck_proxy("map") |>
      get_clicked_object(session)  # default: NULL

    req(d_c)
    schema_tbl <- isolate(input$sel_aois)
    tbl        <- str_split(schema_tbl, "\\.")[[1]][2]

    key <- case_match(
      schema_tbl,
      "public.ply_shlfs" ~ "shlf_key",
      "public.ply_rgns"  ~ "rgn_key",
      .default = "ms_key")
    val <- d_c[key][1]
    url <- glue("https://tile.marinesensitivity.org/{schema_tbl}.html?filter={key}='{val}'")
    # txt <- glue("SCHEMA.TABLE: {schema_tbl}<br> WHERE: {key} = '{val}'")
    # message(glue("get_clicked(): [{txt}]({url})"))

    rx$clicked <- list(
      schema.table = schema_tbl,
      where        = glue("{key} = '{val}'"))

    nav_insert(
      "nav",
      target = "Map",
      nav_panel(
        "Table",
        helpText("amt = n_cells * avg_pct_cell * avg_suit"),br(),
        helpText("Amount (amt) is the multiplication of the number of cells (n_cells),
               average percent of a cell (avg_pct_cell) within the selected polygon,
               and the average Suitability (avg_suit; 0 to 1) of the species given by AquaMaps."),
        dataTableOutput("tbl_spp") ) )

    rx$status <- div(
      "clicked ", a("feature", href=url, target='_blank'), br(),
      "SCHEMA.TABLE: ", code(schema_tbl), br(),
      "WHERE: ", code(glue("{key} = '{val}'")))
  })

  # * get edited ----
  observe({
    # spatial data frame of edited (and uploaded) feature
    s_e <- rdeck_proxy("map") |>
      get_edited_features(session)  # default: Simple feature collection with 0 features and 0 fields
    # d_edited |> st_geometry() |> st_as_text()
    req(nrow(s_e) > 0)
    txt <- div("last edited:", st_geometry(s_e) %>% st_as_text())
    # message(glue("get_edited(): {txt}"))

    rx$clicked <- NULL
    nav_remove("nav", "Table")

    rx$status <- txt
  })

  # * get bounds ----
  # observe({
  #   b <- rdeck_proxy("map") |>
  #     get_view_bounds(session)
  #   message(glue("extent: {paste(b, collapse=', ')}"))
  # })

  # Table: tbl_spp ----
  output$tbl_spp <- DT::renderDataTable({

    req(rx$clicked)

    msens::get_species_by_feature(
      schema.table = rx$clicked$schema.table,
      where        = rx$clicked$where) |>
      datatable(
        extensions = c("Buttons", "FixedColumns", "FixedHeader", "Scroller"),
        options = list(
          dom = "Blfrtip",
          buttons = c("copy", "csv", "excel", "pdf", "print"),
          scrollX = T,
          scrollY = "50vh",
          scrollCollapse = T,
          fixedColumns = list(leftColumns = 1),
          fixedHeader = T)) |>
      formatPercentage(
        columns = c("avg_pct_cell", "avg_suit"),
        digits  = 0) |>
      formatRound(
        columns = c("n_cells"),
        digits  = 0) |>
      formatRound(
        columns = c("amt","area_km2"),
        digits  = 3)
  },
  server = F)

})
