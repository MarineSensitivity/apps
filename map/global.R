# libraries ----
librarian::shelf(
  dplyr, glue, here, leaflet,
  MarineSensitivity/msens,
  RColorBrewer, shiny, shinydashboard, sf, tibble)
#source("functions.R")

lst_rgns <- ply_rgns_s05 |>
  st_drop_geometry() |>
  arrange(shlf_name, rgn_name) |>
  group_by(shlf_name) |>
  summarize(
    rgns = list(setNames(rgn_key, rgn_name))) |>
  deframe()
lst_rgns <- c(list(Overview = setNames(NA, "Overview")),
  lst_rgns)


