# TODO:
# - [ ] move fxns to R package aquamapsduckdb
#   - goal: move to faster duckdb, and raster superseded by terra
#   - [ ] raw-data/create_duckdb.R: generate from aquamapsdata sqlitedb
# - [ ]
# - [ ]
# - [ ]

librarian::shelf(
  DBI, dplyr, duckdb, fs, glue, here,
  leaflet, stringr, terra, tibble,
  quiet = T)  # zeallot

# duckdb connection to AquaMaps ----
dir_data <- "/Users/bbest/My Drive/projects/msens/data"
path_am  <- glue("{dir_data}/derived/aquamaps/am.duckdb")

am_spp <- function(){

  tbl(con_am, "spp") |>
    select(sp_key, genus, species) |>
    collect() |>
    mutate(
      # TODO: add sp_scientific column to tbl spp based on aphia_id in [worrms](https://docs.ropensci.org/worrms/)
      sp_scientific = glue("{genus} {species}")) |>
    select(sp_key, sp_scientific)
}

am_spp_in_ply <- function(ply = NULL){
  # get spp in polygon
  # ply: polygon sf object

  if (!is.null(ply)){
    ply <- st_wrap_dateline(ply)

    r <- am_rast_template(values="cell_idx")

    d_w <- extract(r, ply, ID=F, weights=T)  # nrow(d_w): 80
    # TODO: use weights
    # d_w <- extract(r, ply, weights=T, ID=F)
    # r_w <- am_rast_template()
    # r_w[d$cell_idx] <- d$weight
    # r_w <- trim(r_w); plot(r_w)
    # Alternatively, you can use zonal after using rasterize with a SpatVector (this may be more efficient in some cases).
    # TODO: compare with:
    # - [ ] r_ply = terra::rasterize(ply); terra::zonal(r, r_ply)
    # - [ ] exactextractr::exact_extract()
    # TODO: threshold by probability

    d_1 <- tbl(con_am, "cells") |>
      # TODO: consider cells with ocean_area < 100%
      inner_join(
        d_w,
        copy = T,
        by   = "cell_idx")
  } else {
    d_1 <- tbl(con_am, "cells") |>
      mutate(
        weight = 1.0)
  }

  d <- d_1 |>
    select(cell_id, weight) |>
    left_join(
      tbl(con_am, "spp_cells") |>
        # TODO: consider filtering by:
        #   - binary:     fao_area_yn, bound_box_yn
        #   - continuous: probability (threshold)
        select(cell_id, sp_key, probability),
      by = "cell_id") |>
    group_by(sp_key) |>
    summarize(
      n_cells         = n(),
      avg_weight      = mean(weight, na.rm=T),
      # TODO: consider weighting probability by weight and cell area (∆s w/ lat)
      avg_probability = mean(probability, na.rm=T)) |>
    mutate(
      n               = n_cells * avg_weight * avg_probability) |>
    relocate(n, .after = avg_probability) |>
    left_join(
      tbl(con_am, "spp") |>
        select(sp_key, phylum, class, order, family, genus, species),
      by = "sp_key") |>
    arrange(phylum, class, order, family, genus, species) |>
    collect()

  # TODO: output:
  # - [ ] table of output
  # - [ ] treemap
  # - [ ] ui dynamic filtering by taxonomy

  # length(unique(d$phylum)): 15

  attr(d, "n_cells_max") <- nrow(d)

  d
}

am_rast_nspp <- function(){
  # get species richness raster

  d <- tbl(con_am, "spp_cells") |>
    # TODO: consider filtering by:
    #   - binary:     fao_area_yn, bound_box_yn
    #   - continuous: probability (threshold)
    # TODO: consider filtering by ui taxonomy:
    #   - phylum, class,...
    select(cell_id, sp_key, probability) |>
    group_by(cell_id) |>
    summarize(
      n_spp = n() |> as.integer()) |>
    left_join(
      tbl(con_am, "cells") |>
        select(cell_id, cell_idx),
      by = "cell_id") |>
    select(cell_idx, n_spp) |>
    collect()

  r <- am_rast_template()
  r[d$cell_idx] <- d$n_spp
  names(r) <- "n_spp"
  # mapview::mapView(r)

  r
}

am_rast_sp <- function(sp_key){
  # function for getting raster of species probability

  # TODO: filter also by
  # - fao_area_yn: Does this cell fall within an FAO area where the species is known to occur (endemic/native)? 0=No, 1=Yes
  # - bound_box_yn: Does this cell fall within the geographical bounding box known for the species? 0=No, 1=Yes

  # get sp cells
  d <- tbl(con_dd, "spp_cells") |>
    filter(sp_key == !!sp_key) |>
    left_join(
      tbl(con_dd, "cells") |>
        select(cell_id, cell_idx),
      by = "cell_id") |>
    filter(
      !is.na(cell_idx),
      !is.na(probability)) |>
    select(cell_idx, probability) |>
    collect()

  r <- am_rast_template()

  # substitute raster values
  r[d$cell_idx] <- d$probability

  names(r) <- sp_key
  r
}

am_rast_template <- function(values="NA"){
  values <- values[1]
  stopifnot(values %in% c("NA","cell_idx"))

  # create template raster from global dimensions and resolution
  r <- rast(
    xmin = -180, xmax = 180,
    ymin = -90,  ymax = 90,
    resolution = 0.5)

  if (values == "cell_idx"){
    r[] <- 1:ncell(r)
    names(r) <- "cell_idx"
    # TODO: migrate cell_idx to overwrite cell_id throughout tables
  }

  r
}
