
library(tidyverse)

allocate_tp <- function(tp_rankings, places = 8) {
  
  # --------------------------------
  # STEP 1: Allocate separately by gender
  # --------------------------------
  
  tp_allocation <- tp_rankings |>
    group_by(gender) |>
    arrange(desc(total_points), country, .by_group = TRUE) |>
    mutate(
      rank = min_rank(desc(total_points))
    ) |>
    group_modify(~ {
      
      gender_data <- .x
      
      # Identify qualification cutoff
      cutoff_points <- gender_data |>
        slice_head(n = places) |>
        summarise(cutoff = min(total_points)) |>
        pull(cutoff)
      
      # Check for ties at the cutoff
      if (
        sum(gender_data$total_points == cutoff_points) > 1
      ) {
        warning(
          "Team Pursuit tie detected at qualification cutoff for ",
          .y$gender,
          "."
        )
      }
      
      gender_data |>
        mutate(
          qualification_status = if_else(
            rank <= places,
            "Qualified",
            "Not qualified"
          ),
          qualification_margin = total_points - cutoff_points,
          qualification_route = if_else(
            rank <= places,
            "Team Pursuit ranking",
            "Not qualified"
          ),
          discipline = "TP"
        )
      
    }) |>
    ungroup()
  
  return(tp_allocation)
}

