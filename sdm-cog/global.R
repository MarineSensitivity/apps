# remotes::install_github("qfes/rdeck")
if (!require("librarian"))
  install.packages("librarian")

librarian::shelf(
  bslib, dbplyr, dplyr, DT, glue, here, htmlwidgets, httr2,
  MarineSensitivity/msens,  # remotes::install_github("MarineSensitivity/msens")
  purrr, qfes/rdeck, readr, sf, terra,
  shiny,
  shinyjs,
  shinyWidgets, stringr, tibble, tidyr, tidyselect, viridis)
options(readr.show_col_types = F)

# variables ----
verbose = T


# mapbox token ----
dir_private <- switch(
  Sys.info()[["sysname"]],
  "Darwin" = "/Users/bbest/My Drive/private",
  "Linux"  = "/share/private")

mb_token_txt <- glue("{dir_private}/mapbox_token_bdbest.txt")
plys_csv     <- here("../workflows/data/ingest_blocks_spatial-paths.csv")
db_r         <- here("../workflows/libs/db.R")

stopifnot(all(file.exists(c(mb_token_txt, plys_csv, db_r))))

source(db_r)

mb_token <- readLines(mb_token_txt)
options(rdeck.mapbox_access_token = mb_token)

# datasets ----
v_ds <- tbl(con, "sdm_datasets") |>
  filter(spatial_data_type == "raster") |>
  select(name_short, ds_key) |>
  arrange(name_short) |>
  collect() |>
  deframe()

mdls_csv <- here("../workflows/data/nc_models.csv")
d_mdls <- read_csv(mdls_csv)

# tbl(con, "sdm_models") |> summarize(n()) # 672
