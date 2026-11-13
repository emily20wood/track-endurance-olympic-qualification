
library(tidyverse)

# Convert result placings to numeric
results <- results |> 
  mutate(placing = as.integer(placing))

# Separate exact placings from catch-all placings
exact_points <- points_table |> 
  filter(!str_detect(placing, regex("to\\s*X", ignore_case = TRUE))) |> 
  mutate(placing = as.integer(placing))

catch_all_points <- points_table |> 
  filter(str_detect(placing, regex("to\\s*X", ignore_case = TRUE))) |> 
  mutate(
    minimum_placing = as.integer(
      str_extract(placing, "^\\d+")
    )
  )

# Add competition type to results
results_with_points <- results |> 
  left_join(
    competitions |> 
      select(competition_id, competition_type),
    by = "competition_id"
  )

# Match exact placings
results_with_points <- results_with_points |> 
  left_join(
    exact_points |> 
      select(competition_type, discipline, placing, points),
    by = c(
      "competition_type",
      "discipline",
      "placing"
    )
  )

# Apply catch-all points where exact placing wasn't found
results_with_points <- results_with_points |> 
  rowwise() |> 
  mutate(
    points = if (is.na(placing)) {
      0
    } else if (!is.na(points)) {
      points
    } else {
      fallback <- catch_all_points |> 
        filter(
          competition_type == cur_data()$competition_type,
          discipline == cur_data()$discipline,
          placing >= minimum_placing
        )
      
      if (nrow(fallback) == 1) {
        fallback$points
      } else {
        NA_real_
      }
    }
  ) |> 
  ungroup()

results_with_points <- results_with_points |> 
  mutate(
    points_multiplier = case_when(
      competition_type == "Continental Championships" ~ 0.75,
      TRUE ~ 1
    ),
    points = points * points_multiplier
  )

# Check for missing points allocations
missing_points <- results_with_points |> 
  filter(!is.na(placing), is.na(points))

if (nrow(missing_points) > 0) {
  print(missing_points)
  stop("Some placings do not have a points allocation.")
}

# View results
print(results_with_points)
