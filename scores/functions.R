librarian::shelf(
  dplyr, ggiraph, ggplot2)

# Function to create the flower plot
plot_flower <- function(
    data,
    fld_category,
    fld_height,
    fld_width,
    tooltip_expr = NULL,
    score        = NULL,
    title        = NULL,
    colors       = "Set2"){

  stopifnot(is.numeric(data |> pull({{ fld_height }})))
  stopifnot(is.numeric(data |> pull({{ fld_width }})))

  if (is.null(score)){
    score <- data |>
      # ensure both are not just integer (weighted.mean goes to 0)
      mutate(
        "{{fld_height}}" := as.double({{ fld_height }}),
        "{{fld_width}}"  := as.double({{ fld_width  }}) ) |>
      summarize(
        score = weighted.mean({{ fld_height }}, {{ fld_width }}, na.rm = T)) |>
      pull(score)
  }

  # Calculate positions
  d <- data |>
    arrange({{ fld_category }}) |>
    mutate(across(!where(is.character), as.double)) |>
    mutate(
      # Calculate angles for plotting
      ymax    = cumsum({{ fld_width }}),
      ymin    = lag(ymax, default=0), # ,  c(0, head(ymax, n=-1)),
      xmax    = {{ fld_height }},
      xmin    = 0)

  sym_category <- ensym(fld_category)
  sym_height   <- ensym(fld_height)
  sym_width    <- ensym(fld_width)

  if (!is.null(tooltip_expr)){
    d <- d |>
      mutate(
        tooltip = glue(tooltip_expr))
  } else {
    d <- d |>
      mutate(
        tooltip = glue("{!!fld_category}"))
  }

  g <- ggplot(d) +
    geom_rect_interactive(aes(
      xmin    = xmin,
      xmax    = xmax,
      ymin    = ymin,
      ymax    = ymax,
      fill    = {{ fld_category }},
      color   = "white",
      data_id = {{ fld_category }},
      tooltip = tooltip),
      color = "white",
      alpha = 0.5) +
    coord_polar(theta = "y") +
    # Create donut hole
    xlim(c(-10, max(data |> pull({{ fld_height }})))) +
    # Add center score
    annotate(
      "text", x = -10, y = 0,
      label = round(score),
      size = 8,
      fontface = "bold") +
    # scale_fill_brewer(
    #   palette = colors) +
    # scale_fill_brewer() +
    theme_minimal() +
    # theme_void() +
    theme(
      legend.position = "bottom",
      plot.margin = unit(c(20, 20, 20, 20), "pt"))

  if (!is.null(title))
    g <- g +
      ggtitle(title)

  girafe(ggobj = g)
}
