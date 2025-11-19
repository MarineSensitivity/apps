# setup----

# * libraries ----
librarian::shelf(
  bsicons, dplyr, duckdb, glue, here, htmltools, litedown, readr, sf, shiny, terra)
options(readr.show_col_types = F)

# * raster ----
r0     <- rast(here("mapgl/cache/r_init.tif")) # scores, All USA
bbox0  <- st_bbox(r0) |> as.numeric()
n_cols <- 11
cols_r <- rev(RColorBrewer::brewer.pal(n_cols, "Spectral"))
rng_r0 <- minmax(r0) |> as.numeric() |> signif(digits = 3)

# * storymap ----
script_md <- here("storymap/script.qmd")

# * variables ----
verbose        <- interactive()
is_server      <-  Sys.info()[["sysname"]] == "Linux"
dir_private    <- ifelse(
  is_server,
  "/share/private",
  "~/My Drive/private")
dir_data       <- ifelse(
  is_server,
  "/share/data",
  "~/My Drive/projects/msens/data")
mapbox_tkn_txt <- glue("{dir_private}/mapbox_token_bdbest.txt")
cell_tif       <- glue("{dir_data}/derived/r_bio-oracle_planarea.tif")
sdm_db         <- glue("{dir_data}/derived/sdm.duckdb")
pa_gpkg        <- glue("{dir_data}/derived/ply_planareas_2025.gpkg")
er_gpkg        <- glue("{dir_data}/derived/ply_ecoregions_2025.gpkg")
lyrs_csv       <- glue("{dir_data}/derived/layers.csv")
metrics_tif    <- glue("{dir_data}/derived/r_metrics.tif")
sr_gpkg        <- glue("{dir_data}/derived/ply_subregions_2025.gpkg")
sr_pa_csv      <- glue("{dir_data}/derived/subregion_planareas.csv")
init_tif       <- here("mapgl/cache/r_init.tif")
taxonomy_csv   <- here("../workflows/data/taxonomic_hierarchy_worms_2025-10-30.csv")

# mapbox token ----
Sys.setenv(MAPBOX_PUBLIC_TOKEN=readLines(mapbox_tkn_txt))
librarian::shelf(
  mapgl)

# * db ----
con_sdm <- dbConnect(duckdb(), dbdir = sdm_db, read_only = T)

# * lyrs ----
d_lyrs       <- read_csv(lyrs_csv)

# helper functions ----
md_extract <- function(
    identifier,
    type = c("header", "content"),
    md_file = script_md, md_text = NULL) {

  type <- match.arg(type)

  # Read markdown content
  if (!is.null(md_file)) {
    md_lines <- readLines(md_file, warn = FALSE)
  } else if (!is.null(md_text)) {
    md_lines <- strsplit(md_text, "\n")[[1]]
  } else {
    stop("Must provide either md_file or md_text")
  }

  # Find the line with the identifier
  pattern <- sprintf("^##\\s+(.+?)\\s+\\{#%s\\}", identifier)
  header_idx <- grep(pattern, md_lines)

  if (length(header_idx) == 0) {
    stop(sprintf("Identifier '#%s' not found", identifier))
  }

  # Extract header text if requested
  if (type == "header") {
    header_line <- md_lines[header_idx]
    txt <- sub("^##\\s+(.+?)\\s+\\{#.+\\}\\s*$", "\\1", header_line)
  }

  # Extract content if requested
  if (type == "content") {
    # Find the next ## heading or end of file
    next_header <- grep("^##\\s+", md_lines[(header_idx + 1):length(md_lines)])

    if (length(next_header) > 0) {
      end_idx <- header_idx + next_header[1] - 1
    } else {
      end_idx <- length(md_lines)
    }

    # Get content lines (skip the header line itself)
    txt <- md_lines[(header_idx + 1):end_idx] |>
      paste(collapse = "\n")
  }

  HTML(mark(txt))
}
