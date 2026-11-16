
library(tidyverse)

# Load the existing qualification allocation functions
source(here::here("scripts", "05_allocate_tp.R"))
source(here::here("scripts", "06_allocate_madison.R"))
source(here::here("scripts", "07_allocate_omnium.R"))


calculate_scenario <- function(
    scenario_results,
    scenario_gender,
    scenario_mode
) {
  
  # Check the scenario mode
  if (!scenario_mode %in% c("TP only", "Full scenario")) {
    stop("Invalid scenario mode.")
  }
  
  # Check required columns
  required_columns <- c(
    "country", "discipline", "competition_id", "placing", "gender"
  )
  
  if (!all(required_columns %in% names(scenario_results))) {
    stop("Scenario results are missing required columns.")
  }
  
  # Keep only the selected gender
  scenario_results <- scenario_results |>
    filter(gender == scenario_gender) |>
    select(all_of(required_columns)) |>
    mutate(placing = as.integer(placing))
  
  # Determine which disciplines may be simulated
  scenario_disciplines <- if (scenario_mode == "TP only") {
    "TP"
  } else {
    c("TP", "MAD", "OMN")
  }
  
  # Check disciplines
  if (any(!scenario_results$discipline %in% scenario_disciplines)) {
    stop("Scenario results contain an unexpected discipline.")
  }
  
  # Check competition IDs
  if (any(!scenario_results$competition_id %in% competitions$competition_id)) {
    stop("Scenario results contain an unknown competition ID.")
  }
  
  # Check countries
  unknown_countries <- scenario_results |>
    anti_join(countries |> select(country), by = "country")
  
  if (nrow(unknown_countries) > 0) {
    stop("Scenario results contain an unknown country.")
  }
  
  # Check that placings are valid
  if (any(is.na(scenario_results$placing) | scenario_results$placing < 1)) {
    stop("Scenario placings must be positive whole numbers.")
  }
  
  # Check that each country has only one placing per event and discipline
  duplicate_results <- scenario_results |>
    count(country, discipline, competition_id, gender) |>
    filter(n > 1)
  
  if (nrow(duplicate_results) > 0) {
    stop("A country has more than one placing in the same competition and discipline.")
  }
  
  # Check Continental Championship country eligibility
  continental_results <- scenario_results |>
    inner_join(
      competitions |>
        filter(competition_type == "Continental Championships") |>
        select(competition_id),
      by = "competition_id"
    ) |>
    left_join(
      countries |> select(country, continent),
      by = "country"
    )
  
  invalid_continental_results <- continental_results |>
    filter(is.na(continent))
  
  if (nrow(invalid_continental_results) > 0) {
    stop("A Continental Championship result could not be matched to a continent.")
  }
  
  # Start with actual results.
  # Replace only the exact country/event/discipline combinations
  # for which hypothetical placings have been entered.
  scenario_data <- results |>
    filter(gender == scenario_gender) |>
    anti_join(
      scenario_results |>
        select(country, discipline, competition_id, gender),
      by = c("country", "discipline", "competition_id", "gender")
    ) |>
    bind_rows(scenario_results)
  
  # Calculate points for the combined scenario results
  
  scenario_data <- scenario_data |>
    mutate(placing = as.integer(placing))
  
  exact_points <- points_table |>
    filter(!str_detect(placing, regex("to\\s*X", ignore_case = TRUE))) |>
    mutate(placing = as.integer(placing))
  
  catch_all_points <- points_table |>
    filter(str_detect(placing, regex("to\\s*X", ignore_case = TRUE))) |>
    mutate(
      minimum_placing = as.integer(str_extract(placing, "^\\d+"))
    )
  
  results_with_points <- scenario_data |>
    left_join(
      competitions |> select(competition_id, competition_type),
      by = "competition_id"
    ) |>
    left_join(
      exact_points |>
        select(competition_type, discipline, placing, points),
      by = c("competition_type", "discipline", "placing")
    ) |>
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
        
        if (nrow(fallback) == 1) fallback$points else NA_real_
      }
    ) |>
    ungroup() |>
    mutate(
      points_multiplier = case_when(
        competition_type == "Continental Championships" ~ 0.75,
        TRUE ~ 1
      ),
      points = points * points_multiplier
    )
  
  # Stop if any result is missing a points allocation
  missing_points <- results_with_points |>
    filter(!is.na(placing), is.na(points))
  
  if (nrow(missing_points) > 0) {
    print(missing_points)
    stop("Some scenario results do not have a points allocation.")
  }
  
  # Identify World Cup results
  world_cup_results <- results_with_points |>
    filter(str_starts(competition_id, "WCU")) |>
    mutate(
      year = as.integer(str_extract(competition_id, "20\\d{2}"))
    )
  
  # Identify non-World Cup results
  other_results <- results_with_points |>
    filter(!str_starts(competition_id, "WCU"))
  
  # Keep the best two World Cup results per year
  counting_world_cup_results <- world_cup_results |>
    group_by(gender, country, discipline, year) |>
    arrange(desc(points), competition_id, .by_group = TRUE) |>
    slice_head(n = 2) |>
    ungroup()
  
  # Combine all counting results
  counting_results <- bind_rows(
    other_results,
    counting_world_cup_results
  )
  
  # Calculate total points and rankings
  qualification_rankings <- counting_results |>
    group_by(gender, country, discipline) |>
    summarise(
      total_points = sum(points, na.rm = TRUE),
      .groups = "drop"
    ) |>
    group_by(gender, discipline) |>
    arrange(desc(total_points), country, .by_group = TRUE) |>
    mutate(
      rank = min_rank(desc(total_points))
    ) |>
    ungroup()
  
  # Check whether all expected World Cups are represented
  expected_world_cups <- c(
    "WCU2027_1", "WCU2027_2", "WCU2027_3",
    "WCU2028_1", "WCU2028_2", "WCU2028_3"
  )
  
  world_cups_complete <- results_with_points |>
    filter(competition_id %in% expected_world_cups) |>
    distinct(gender, competition_id) |>
    count(gender, name = "events_entered") |>
    filter(events_entered == length(expected_world_cups)) |>
    pull(gender)
  
  qualification_rankings <- qualification_rankings |>
    mutate(
      ranking_status = if_else(
        gender %in% world_cups_complete,
        "Complete",
        "Provisional"
      )
    )
  
  # Separate discipline rankings
  tp_rankings <- qualification_rankings |>
    filter(discipline == "TP")
  
  madison_rankings <- qualification_rankings |>
    filter(discipline == "MAD")
  
  omnium_rankings <- qualification_rankings |>
    filter(discipline == "OMN")
  
  # Calculate Team Pursuit qualification
  scenario_tp_allocation <- allocate_tp(
    tp_rankings,
    places = 8
  )
  
  # Calculate Madison qualification
  scenario_madison_allocation <- allocate_madison(
    scenario_tp_allocation,
    madison_rankings,
    additional_places = 6
  )
  
  # Calculate Omnium qualification
  scenario_omnium_allocation <- allocate_omnium(
    scenario_madison_allocation,
    omnium_rankings,
    countries,
    additional_places = 13,
    madison_automatic_places = 6
  )
  
  # Combine all qualification outcomes
  scenario_qualification <- bind_rows(
    scenario_tp_allocation,
    scenario_madison_allocation,
    scenario_omnium_allocation
  )
  
  return(list(
    results_with_points = results_with_points,
    counting_results = counting_results,
    qualification_rankings = qualification_rankings,
    tp_rankings = tp_rankings,
    madison_rankings = madison_rankings,
    omnium_rankings = omnium_rankings,
    tp_allocation = scenario_tp_allocation,
    madison_allocation = scenario_madison_allocation,
    omnium_allocation = scenario_omnium_allocation,
    qualification = scenario_qualification
  ))
}
