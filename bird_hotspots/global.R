librarian::shelf(
  bslib, dplyr, glue, here, leaflet, mapedit, mapview, plotly, sf, shiny,
  tibble, tidyr)

conf_level = 0.95

# helper functions ----

# Background:
# - https://rstudio.github.io/leaflet/articles/choropleths.html
# - https://github.com/r-spatial/mapedit/blob/35ae8ccfb90b682bf98a82e85c3047f098857db3/inst/examples/shiny_modules.R#L46C1-L71C20

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
load("data/bird_hotspots.RData") # single_sf

hhvh <- single_sf |>
  filter(
    probability_threshold == "hh_thru_very_high") |>
  select(ID, numhot, ends_with("hotspot_probability")) |>
  rename_at(vars(-ID, -numhot), ~gsub("_hotspot_probability", "", .x)) |>
  st_transform(4326)  # geographic projection


