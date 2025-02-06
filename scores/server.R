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
    # schema_tbl <- "public.ply_ep_planareas" # DEBUG
    parts      <- str_split(schema_tbl, "\\.")[[1]]
    schema     <- parts[1]
    tbl        <- parts[2]
    #fld_geom   <- ifelse(schema == "public", "geometry", "geom")
    fld_geom   <- "geom" # DEBUG

    # * get extent of features in db ----
    # TODO: use faster ST_EstimatedExtent(). For PostgreSQL >= 8.0.0 statistics are gathered by VACUUM ANALYZE and the result extent will be about 95% of the actual one. For PostgreSQL < 8.0.0 statistics are gathered by running update_geometry_stats() and the result extent is exact.
    # if (schema_tbl == "public.ply_ep_planareas"){
    #   b <- c(-193.3, 14.8, -14.7, 74.9)
    # } else {
      b <- dbGetQuery(con, glue(
        # "SELECT ST_EstimatedExtent('{schema}', '{tbl}', '{fld_geom}');")) |>
        "SELECT ST_Extent({fld_geom}) AS ext FROM {schema_tbl}")) |>
        pull(ext) |>
        str_replace_all("BOX\\((.*)\\)", "\\1") |>
        str_split("[ ,]") %>%
        .[[1]] |>
        as.numeric()  # xmin, ymin, xmax, ymax
    # }
    # message(glue("using extent: {paste(b, collapse=', ')}"))

    # * get fields to display ----
    flds <<- dbGetQuery(con, glue(
      "SELECT column_name FROM information_schema.columns
      WHERE
        table_schema = '{schema}' AND
        table_name   = '{tbl}';")) |>
      pull(column_name)
    # message(glue("with flds: {paste(flds, collapse=', ')}"))

    # browser()
    v <- tbl(con, tbl) |>
      pull(score_even)

    rdeck(
      map_style      = mapbox_dark(),
      theme          = "light",
      initial_bounds = st_bbox(
        c(xmin=b[1], ymin=b[2], xmax=b[3], ymax=b[4]),
        crs = st_crs(4326)),
      # editor = T)  |>
      editor = F)  |>
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
        # get_fill_color    = "#0000FF80",  # blue 0.5 opacity
        # get_line_color    = "#0000FFCC")  # blue 0.8 opacity
        get_fill_color    = scale_color_linear(
          col     = score_even,
          palette = turbo(256),
          limits  = range(v)
          ))  # blue 0.8 opacity

  })

  # plot_flower_small ----
  output$plot_flower_small <- renderGirafe({

    req(rx$clicked)

    rx$clicked$d_fl |>
      plot_flower(
        fld_category = taxa,
        fld_height   = score,
        fld_width    = even,
        tooltip_expr = "{taxa}: {round(score, 2)} (n_spp: {n_spp})",
        title        = rx$clicked$ply_name)
  })

  # plot_flower_big ----
  output$plot_flower_big <- renderGirafe({

    req(rx$clicked)

    rx$clicked$d_fl |>
      plot_flower(
        fld_category = taxa,
        fld_height   = score,
        fld_width    = even,
        tooltip_expr = "{taxa}: {round(score, 2)} (n_spp: {n_spp})",
        title        = rx$clicked$ply_name)
  })


  # txt_status ----

  # output$htm_status <- renderUI({
  #   req(rx$status)
  #   h6(code(rx$status))
  # })

  # * get clicked ----
  observe({
    d_c <- rdeck_proxy("map") |>
      get_clicked_object(session)  # default: NULL

    req(d_c)

    schema_tbl <- isolate(input$sel_aois)
    fld <- c(
      "public.ply_ep_ecorgns"   = "ecorgn_ukey",
      "public.ply_ep_planareas" = "planarea_ukey",
      "public.ply_ecoprot"      = "prodiag_ukey")[[schema_tbl]]
    ply_key <- d_c[[fld]]

    fld_name <- str_replace(fld, "_ukey", "_name")
    ply_name <- d_c[[fld_name]]

    d_fl <- tbl(con, "ply_taxa_scores") |>
      filter(ply_key == !!ply_key) |>
      mutate(
        score = score * 100,
        even  = 1) |>
      select(taxa, n_spp, score, even) |>
      collect()

    rx$clicked <- list(
      schema.table = schema_tbl,
      ply_key      = ply_key,
      ply_name     = ply_name,
      d_fl         = d_fl)
  })

  # observe({
  #   d_c <- rdeck_proxy("map") |>
  #     get_clicked_object(session)  # default: NULL
  #
  #   req(d_c)
  #   schema_tbl <- isolate(input$sel_aois)
  #   tbl        <- str_split(schema_tbl, "\\.")[[1]][2]
  #
  #   br
  #   key <- case_match(
  #     schema_tbl,
  #     "public.ply_shlfs" ~ "shlf_key",
  #     "public.ply_rgns"  ~ "rgn_key",
  #     .default = "ms_key")
  #   val <- d_c[key][1]
  #   url <- glue("https://tile.marinesensitivity.org/{schema_tbl}.html?filter={key}='{val}'")
  #   # txt <- glue("SCHEMA.TABLE: {schema_tbl}<br> WHERE: {key} = '{val}'")
  #   # message(glue("get_clicked(): [{txt}]({url})"))
  #
  #   rx$clicked <- list(
  #     schema.table = schema_tbl,
  #     where        = glue("{key} = '{val}'"))
  #
  #   nav_insert(
  #     "nav",
  #     target = "Map",
  #     nav_panel(
  #       "Table",
  #       helpText("amt = n_cells * avg_pct_cell * avg_suit"),br(),
  #       helpText("Amount (amt) is the multiplication of the number of cells (n_cells),
  #              average percent of a cell (avg_pct_cell) within the selected polygon,
  #              and the average Suitability (avg_suit; 0 to 1) of the species given by AquaMaps."),
  #       dataTableOutput("tbl_spp") ) )
  #
  #   rx$status <- div(
  #     "clicked ", a("feature", href=url, target='_blank'), br(),
  #     "SCHEMA.TABLE: ", code(schema_tbl), br(),
  #     "WHERE: ", code(glue("{key} = '{val}'")))
  # })

  # * get edited ----
  # observe({
  #   # spatial data frame of edited (and uploaded) feature
  #   s_e <- rdeck_proxy("map") |>
  #     get_edited_features(session)  # default: Simple feature collection with 0 features and 0 fields
  #   # d_edited |> st_geometry() |> st_as_text()
  #   req(nrow(s_e) > 0)
  #   txt <- div("last edited:", st_geometry(s_e) %>% st_as_text())
  #   # message(glue("get_edited(): {txt}"))
  #
  #   rx$clicked <- NULL
  #   nav_remove("nav", "Table")
  #
  #   rx$status <- txt
  # })

  # * get bounds ----
  # observe({
  #   b <- rdeck_proxy("map") |>
  #     get_view_bounds(session)
  #   message(glue("extent: {paste(b, collapse=', ')}"))
  # })

  # Table: tbl_spp ----
  output$tbl_spp <- DT::renderDataTable({

    req(rx$clicked)

    ply_key <- rx$clicked$ply_key # ply_key = "brAK_erGOA_paGOA"

    d_spp <- tbl(con, "ply_spp") |>
      filter(
        ply_key == !!ply_key,
        !is.na(sp_key)) |>
      select(ply_fld, ply_key, sp_key, sp_km2, sp_suit) |>
      left_join(
        tbl(con, "ply_area") |>
          rename(ply_km2 = area_km2),
        by = c("ply_fld", "ply_key")) |>
      mutate(
        sp_pct_ply = ifelse(sp_km2 / ply_km2 > 1, 1, sp_km2 / ply_km2)) |>
      left_join(
        tbl(con, "spp") |>
          select(
            scientificName, sp_key = sp_key_am, taxa = group_gmbi, iucn_cat = iucnRedListCategory,
            kingdom, phylum, order, family),
        by = "sp_key") |>
      mutate(
        iucn_score = case_match(
          iucn_cat,
          # skipped: EX (extinct), NE (not evaluated), DD (data deficient), <NA> (not available)
          # https://oceanhealthindex.org/images/htmls/Supplement.html#62_Biodiversity
          "CR" ~ 1,       #  - CR: Critically Endangered
          "EN" ~ 0.8,     #  - EN: Endangered
          "VU" ~ 0.6,     #  - VU: Vulnerable
          "NT" ~ 0.4,     #  - NT: Near Threatened
          "LC" ~ 0.2)) |>
      relocate(iucn_score, .after = iucn_cat) |>
      collect()

    d_spp |>
      datatable(
        caption = glue(
          "Species within {rx$clicked$ply_name}, including those without a
          scorable IUCN Red List Category:
          CR (Critically Endangered: 1),
          EN (Endangered: 0.8),
          VU (Vulnerable: 0.6),
          NT (Near Threatened: 0.4),
          LC (Least Concern: 0.2)"),
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
        columns = c("sp_suit", "sp_pct_ply", "iucn_score"),
        digits  = 1) |>
      # formatRound(
      #   columns = c("n_cells"),
      #   digits  = 0) |>
      formatRound(
        columns = c("sp_km2","ply_km2"),
        digits  = 1)
  },
  server = F)

})
