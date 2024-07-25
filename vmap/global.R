# libraries ----
librarian::shelf(
  bslib, digest, dplyr, DT, glue, here, htmltools, leaflet, purrr, readr,
  shiny, shinyWidgets, stringr, terra, yaml)
options(readr.show_col_types = F)

# paths ----
dir_workflows <- here("../workflows")
dir_tif       <- glue("{dir_workflows}/data/sdm/raw/nc_atl_birds_dens")
dir_csv       <- glue("{dir_workflows}/data/sdm/derived/nc_atl_birds_dens")
mw_csv        <- glue("{dir_csv}/mw.csv")
spp_csv       <- glue("{dir_csv}/mw_spp.csv")
dir_cache     <- here("vmap/cache")

dir.create(dir_cache, showWarnings = F)

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
    season      = str_to_title(season),
    sp_id_html  = glue(
      "<a href='https://www.gbif.org/species/{str_replace(sp_id, 'GBIF:', '')}' target='_blank'>{sp_id}</a>"),
    study_map   = case_match(
      study_map,
      "winship2018"  ~ "<a href='https://coastalscience.noaa.gov/data_reports/modeling-at-sea-density-of-marine-birds-to-support-atlantic-marine-renewable-energy-planning-final-report/' target='_blank'>winship2018</a>"),
    study_param = case_match(
      study_param,
      "willmott2013" ~ "<a href='https://espis.boem.gov/final%20reports/5319.PDF' target='_blank'>willmott2013</a>"),
    path_tif    = glue("{dir_tif}/{tif}"),
    tif         = glue(
      "<a href='https://github.com/MarineSensitivity/workflows/raw/main/data/sdm/raw/nc_atl_birds_dens/{tif}' target='_blank'>{tif}</a>"),
    r           = map(path_tif, rast, lyrs = "n_per_km2") )
# d_mw |> names() |> paste(collapse = '","') |> cat()
cols_mw_notdisplay <- c(
  "r", "path_tif",
  "taxonID","taxonomicStatus","acceptedNameUsageID","parentNameUsageID","originalNameUsageID",
  "scientificNameAuthorship","vernacularName")
rgn_default <- "Atlantic"
ssn_default <- "Spring"
d_mws_default <- d_mw |>
  filter(season == ssn_default)
