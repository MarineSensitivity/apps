# remotes::install_github("qfes/rdeck")
if (!require("librarian"))
  install.packages("librarian")

librarian::shelf(
  bslib, dbplyr, dplyr, DT, glue, here, htmlwidgets, httr2,
  MarineSensitivity/msens,  # remotes::install_github("MarineSensitivity/msens")
  purrr, qfes/rdeck, readr, sf,
  shiny,
  shinyjs,
  shinyWidgets, stringr, tibble, tidyr, tidyselect, viridis)
options(readr.show_col_types = F)

# variables ----
verbose = F

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
  select(name_short, ds_key) |>
  collect() |>
  deframe()

# tbl(con, "sdm_models") |> summarize(n()) # 672

# bounding box ----
# TODO: vary by input$sel_dataset
ds_key <- "gm"
q <- glue(
  "WITH
       g AS (
         SELECT geom FROM sdm_geometries
         WHERE ds_key = '{ds_key}')
       SELECT ST_Extent(geom) AS ext FROM g")
b <- dbGetQuery(con, q) |>
  pull(ext) |>
  str_replace_all("BOX\\((.*)\\)", "\\1") |>
  str_split("[ ,]") %>%
  .[[1]] |>
  as.numeric()
