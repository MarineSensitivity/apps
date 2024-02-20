library(dplyr)
library(glue)
library(here)
library(leaflet)
library(mapedit)
library(mapview)
library(sf)
library(shiny)
library(tibble)

# helper functions ----

get_basemap <- function(){
  leaflet::leaflet() |>
  # add base: blue bathymetry and light brown/green topography
  addProviderTiles(
    "Esri.OceanBasemap",
    options = providerTileOptions(
      variant = "Ocean/World_Ocean_Base")) |>
  # add reference: placename labels and borders
  addProviderTiles(
    "Esri.OceanBasemap",
    options = providerTileOptions(
      variant = "Ocean/World_Ocean_Reference"))
}

# load data ----

# load(here("bird_hotspots/data/bird_hotspots.RData"))
load("data/bird_hotspots.RData")

hhvh <- single_sf |>
  filter(
    probability_threshold == "hh_thru_very_high") |>
  select(ID, probability_threshold, numhot) |>
  st_transform(4326)  # geographic projection
