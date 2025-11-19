fluidPage(
  tags$head(
    tags$style(
      HTML("
        @keyframes bounce {
          0%, 20%, 50%, 80%, 100% { transform: translateY(0);}
          40% { transform: translateY(-10px); }
          60% { transform: translateY(-5px); }}
        .scroll-arrow { animation: bounce 2s infinite; }"))),
  story_map(
    map_id = "map",
    sections = list(
      "welcome" = story_section(
        md_extract("welcome", "header"),
        tagList(
          md_extract("welcome", "content"),
          bs_icon("arrow-down", size = "1em", class = "scroll-arrow") )),
      "scores_cell" = story_section(
        md_extract("scores_cell", "header"),
        md_extract("scores_cell", "content") ),
      "scores_pa" = story_section(
        md_extract("scores_pa", "header"),
        md_extract("scores_pa", "content") ))))
