## 2025-06-10.a map pa vs cell, flower plot absolute panel

In @apps/mapgl/app.R the `output$map` displays both raster (`add_raster_layer()`)
and vector data (`add_fill_layer()`) using the new mapgl R package. Seperate
these so one or the other is displayed depending on `input$sel_unit` by
using preferably 
[`shiny::bindEvent()`](https://shiny.posit.co/r/reference/shiny/latest/bindevent.html)
(or [`shiny::observeEvent()`](https://shiny.posit.co/r/reference/shiny/latest/observeEvent.html))
to trigger the rendering of the proper map using
[`mapgl::mapboxgl_proxy()`](https://walker-data.com/mapgl/reference/mapboxgl_proxy.html)
(see also [Using mapgl with Shiny • mapgl](https://walker-data.com/mapgl/articles/shiny.html)) based on `input$sel_unit`.

Add a draggable `shiny::absolutePanel()` to the map with a map click event and
display the flower plot like in @apps/scores/ based on the `*_ecoregion_rescaled`
layers of the clicked planning area or raster cell.

### Response

⏺ I've successfully modified the apps/mapgl/app.R file to:

  1. Separated raster and vector display based on input$sel_unit:
    - When "Raster cells (0.05°)" is selected, it displays the raster layer
    - When "Planning areas" is selected, it displays the vector fill layer
    - Used observeEvent() with mapboxgl_proxy() to dynamically update the map
  2. Added a draggable absolutePanel that:
    - Is positioned in the top-right corner
    - Contains a flower plot visualization
    - Shows location information when clicked
    - Updates based on whether a raster cell or planning area is clicked
  3. Implemented map click handling:
    - For raster cells: extracts the cell ID at the clicked location
    - For planning areas: captures the clicked feature properties
    - Queries the appropriate *_ecoregion_rescaled metrics from the database
    - Displays the flower plot with species group scores

  The implementation uses bindEvent implicitly through observeEvent to trigger map updates when the spatial unit selection changes. The flower plot shows the extinction risk
  scores for different species groups (taxa) at the clicked location, similar to the scores app.
