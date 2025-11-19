fluidPage(
  story_map(
    map_id = "map",
    sections = list(
      "welcome" = story_section(
        md_extract("welcome", "header"),
        md_extract("welcome", "content") ),
      "scores_cell" = story_section(
        md_extract("scores_cell", "header"),
        md_extract("scores_cell", "content") ),
      "scores_pa" = story_section(
        md_extract("scores_pa", "header"),
        md_extract("scores_pa", "content") ))))
