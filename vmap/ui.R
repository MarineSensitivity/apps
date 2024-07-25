library(shiny)

page_fillable(
  title = "Vulnerability Mapper",
  theme = dark,

  navset_card_pill(
    id        = "nav",
    placement = "above",
    selected  = "nav_map",
    # selected  = "nav_tables", # DEBUG

    # TODO: nav_about
    # nav_panel(
    #   title = "About",
    #   value = "nav_about",
    #   icon  = icon("info"),
    #   "TODO: explain"),

    # nav_configure ----
    nav_panel(
      title = "Configure",
      value = "nav_configure",
      icon = icon("gear"),

      layout_column_wrap(
        width         = 1/2,
        heights_equal = "row",

        selectInput(
          "sel_rgn",
          "Region",
          c("Atlantic", "Pacific [TODO]")),

        selectInput(
          "sel_ssn",
          "Season",
          c("Spring", "Summer", "Fall", "Winter")),

        treeInput(
          "sel_spp",
          label       = "Species",
          choices     = create_tree(
            d_spp,
            # d_spp |> select(class, order, family, sp_scientific, sp_id) |> arrange(class, order, family, sp_scientific)
            levels    = c("class", "order", "family", "sp_scientific"),
            levels_id = c("class", "order", "family", "sp_id") ),
          selected    = d_spp$sp_id,
          returnValue = "id",
          closeDepth  = 1 ),

        helpText(
          "Selecting a season (and eventually region) will update the selected
            Species in this list for those having matching distribution maps and
            vulnerability parameters. You can further uncheck species to
            customize the output."),

        textAreaInput(
          "txt_eqn_r",
          "Equation for each raster (r) before summing",
          width = "100%",
          height = "110px",
          "terra::scale(r) * (
              best_estimate_final_population_sensitivity +
              best_estimate_final_collision_sensitivity_rank +
              best_estimate_final_displacement_sensitivity_rank )"),

        textAreaInput(
          "txt_eqn_v",
          "Equation for summed raster values (v)",
          width = "100%",
          "scales::rescale(v, c(0, 100))"),

        sliderInput(
          "sldr_opacity_r",
          "Opacity of raster on map",
          min = 0, max = 1, step = 0.1,
          value = 1.0),

        sliderInput(
          "sldr_opacity_b",
          "Opacity of basemap",
          min = 0, max = 1, step = 0.1,
          value = 0.5)

      ) ),

    # nav_tables ----
    nav_panel(
      title = "Table",
      value = "nav_tables",
      icon  = icon("table"),

      # TODO: add [t]ables and [h]eaders for:
      # - Species Parameters with Maps [h]
      # - Species Parameters without Maps [t,h]
      # - Species Maps without Parameters [t,h]
      DTOutput("tbl_mw_rs") ),

    # nav_map ----
    nav_panel(
      title = "Map",
      value = "nav_map",
      icon  = icon("map"),

      leafletOutput("map"),
      downloadButton("downloadData", label = "Download")),

   )
)
