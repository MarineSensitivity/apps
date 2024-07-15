# libraries ----
librarian::shelf(
  bsicons, bslib, dplyr, DT, glue, here, htmltools, leaflet, purrr, readr,
  shiny, shinyWidgets, stringr, terra)
options(readr.show_col_types = F)

# paths ----
dir_workflows <- here("../workflows")
dir_tif       <- glue("{dir_workflows}/data/sdm/raw/nc_atl_birds_dens")
dir_csv       <- glue("{dir_workflows}/data/sdm/derived/nc_atl_birds_dens")
mw_csv        <- glue("{dir_csv}/mw.csv")
spp_csv       <- glue("{dir_csv}/mw_spp.csv")

# themes ----
light <- bs_theme(
  preset = "flatly",
  base_font = font_google("Playwright+MX"))
dark  <- bs_theme(
  preset = "darkly") |>
  bs_add_rules(
    list(
      ".treejs .treejs-switcher:before { border-top: 4px solid rgba(255, 255, 255, 0.6) !important }"))

# data ----
d_spp <- read_csv(mw_csv) |>  # species
  mutate(
    season   = str_to_title(season))
d_mw  <- read_csv(mw_csv) |>  # [m]aps and [w]eights
  mutate(
    season   = str_to_title(season),
    path_tif = glue("{dir_tif}/{tif}"),
    r        = map(path_tif, rast, lyrs = "n_per_km2") )
# d_mw |> names() |> paste(collapse = '","') |> cat()
cols_mw_notdisplay <- c(
  "r", "path_tif",
  "taxonID","taxonomicStatus","acceptedNameUsageID","parentNameUsageID","originalNameUsageID",
  "scientificNameAuthorship","vernacularName")
rgn_default <- "Atlantic"
ssn_default <- "spring"
d_mws_default <- d_mw |>
  filter(season == ssn_default)
