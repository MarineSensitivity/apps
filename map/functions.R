# TODO: migrate reusable functions into msens R package

ms_basemap <- function(base_opacity = 0.5){

  m <- leaflet() |>
    # add base: blue bathymetry and light brown/green topography
    leaflet::addProviderTiles(
      "Esri.OceanBasemap",
      options = providerTileOptions(
        variant = "Ocean/World_Ocean_Base",
        opacity = base_opacity)) |>
    # add reference: placename labels and borders
    leaflet::addProviderTiles(
      "Esri.OceanBasemap",
      options = providerTileOptions(
        variant = "Ocean/World_Ocean_Reference",
        opacity = base_opacity))
}
