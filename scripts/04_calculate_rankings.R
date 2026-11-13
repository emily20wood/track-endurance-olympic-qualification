

library(tidyverse)

# --------------------------------
# STEP 1: Identify World Cup results
# --------------------------------

world_cup_results <- results_with_points |>
  filter(str_starts(competition_id, "WCU")) |>
  mutate(
    world_cup_year = as.integer(
      str_extract(competition_id, "20\\d{2}")
    )
  )

# Check that all World Cup IDs have been recognised
if (any(is.na(world_cup_results$world_cup_year))) {
  stop("Could not identify the year for some World Cup results.")
}

# --------------------------------
# STEP 2: Check expected World Cups
# --------------------------------

expected_world_cups <- c(
  "WCU2027_1", "WCU2027_2", "WCU2027_3",
  "WCU2028_1", "WCU2028_2", "WCU2028_3"
)

# Check availability separately for each gender
expected_gender_cups <- crossing(
  gender = c("Men", "Women"),
  competition_id = expected_world_cups
)

available_gender_cups <- results_with_points |>
  filter(competition_id %in% expected_world_cups) |>
  distinct(gender, competition_id)

missing_gender_cups <- expected_gender_cups |>
  anti_join(
    available_gender_cups,
    by = c("gender", "competition_id")
  )

message(
  nrow(available_gender_cups),
  " of 12 expected gender-specific World Cups currently available."
)

if (nrow(missing_gender_cups) > 0) {
  message("Some World Cups are still missing for one or both genders.")
}

# --------------------------------
# STEP 3: Keep best two World Cups per year
# --------------------------------

counting_world_cup_results <- world_cup_results |>
  arrange(
    gender,
    country,
    discipline,
    world_cup_year,
    desc(points),
    competition_id
  ) |>
  group_by(gender, country, discipline, world_cup_year) |>
  slice_head(n = 2) |>
  ungroup()

# --------------------------------
# STEP 4: Include all non-World Cup results
# --------------------------------

other_results <- results_with_points |>
  filter(!str_starts(competition_id, "WCU"))

# Combine all counting results
counting_results <- bind_rows(
  other_results,
  counting_world_cup_results
)

# --------------------------------
# STEP 5: Calculate qualification points
# --------------------------------

qualification_points <- counting_results |>
  group_by(gender, country, discipline) |>
  summarise(
    total_points = sum(points),
    .groups = "drop"
  )

# --------------------------------
# STEP 6: Create separate rankings
# --------------------------------

qualification_rankings <- qualification_points |>
  group_by(gender, discipline) |>
  arrange(desc(total_points), country, .by_group = TRUE) |>
  mutate(
    rank = min_rank(desc(total_points))
  ) |>
  ungroup()

# --------------------------------
# STEP 7: Set ranking status
# --------------------------------

qualification_rankings <- qualification_rankings |>
  mutate(
    ranking_status = if_else(
      nrow(missing_gender_cups) == 0,
      "Complete",
      "Provisional"
    )
  )

# --------------------------------
# STEP 8: Separate discipline rankings
# --------------------------------

tp_rankings <- qualification_rankings |>
  filter(discipline == "TP")

madison_rankings <- qualification_rankings |>
  filter(discipline == "MAD")

omnium_rankings <- qualification_rankings |>
  filter(discipline == "OMN")

# --------------------------------
# STEP 9: View rankings
# --------------------------------

print(tp_rankings)
print(madison_rankings)
print(omnium_rankings)

# --------------------------------
# STEP 10: Save outputs
# --------------------------------

write_csv(
  qualification_rankings,
  here::here("data", "processed", "qualification_rankings.csv")
)

write_csv(
  counting_results,
  here::here("data", "processed", "counting_results.csv")
)
