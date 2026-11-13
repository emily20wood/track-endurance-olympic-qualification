
library(tidyverse)

allocate_madison <- function(
    tp_allocation,
    madison_rankings,
    additional_places = 6
) {
  
  # --------------------------------
  # STEP 1: Allocate separately by gender
  # --------------------------------
  
  madison_allocation <- map_dfr(
    unique(madison_rankings$gender),
    function(current_gender) {
      
      # Filter data for current gender
      gender_tp <- tp_allocation |>
        filter(gender == current_gender)
      
      gender_madison <- madison_rankings |>
        filter(gender == current_gender) |>
        arrange(desc(total_points), country)
      
      # --------------------------------
      # STEP 2: Identify TP automatic qualifiers
      # --------------------------------
      
      tp_qualifiers <- gender_tp |>
        filter(qualification_status == "Qualified") |>
        pull(country)
      
      madison_automatic <- gender_madison |>
        filter(country %in% tp_qualifiers) |>
        mutate(
          qualification_route = "Team Pursuit automatic",
          qualification_status = "Qualified"
        )
      
      # --------------------------------
      # STEP 3: Identify additional Madison candidates
      # --------------------------------
      
      additional_candidates <- gender_madison |>
        filter(!country %in% tp_qualifiers) |>
        arrange(desc(total_points), country)
      
      if (nrow(additional_candidates) < additional_places) {
        stop(
          "Not enough eligible Madison countries for ",
          current_gender, "."
        )
      }
      
      # --------------------------------
      # STEP 4: Identify qualification cutoff
      # --------------------------------
      
      ranking_cutoff_points <- additional_candidates |>
        slice_head(n = additional_places) |>
        summarise(cutoff = min(total_points)) |>
        pull(cutoff)
      
      # Check for ties at the cutoff
      if (
        sum(additional_candidates$total_points == ranking_cutoff_points) > 1
      ) {
        warning(
          "Madison tie detected at additional qualification cutoff for ",
          current_gender,
          "."
        )
      }
      
      # Include all tied countries at cutoff provisionally
      additional_qualifiers <- additional_candidates |>
        filter(total_points >= ranking_cutoff_points) |>
        mutate(
          qualification_route = "Madison ranking",
          qualification_status = "Qualified",
          qualification_margin = total_points - ranking_cutoff_points
        )
      
      # --------------------------------
      # STEP 5: Combine qualification routes
      # --------------------------------
      
      qualified_countries <- bind_rows(
        madison_automatic,
        additional_qualifiers
      ) |>
        distinct(country, .keep_all = TRUE)
      
      # --------------------------------
      # STEP 6: Include every ranked country
      # --------------------------------
      
      gender_madison |>
        left_join(
          qualified_countries |>
            select(
              country,
              qualification_route,
              qualification_status,
              qualification_margin
            ),
          by = "country"
        ) |>
        mutate(
          qualification_route = replace_na(
            qualification_route,
            "Not qualified"
          ),
          qualification_status = replace_na(
            qualification_status,
            "Not qualified"
          ),
          qualification_margin = total_points - ranking_cutoff_points,
          discipline = "MAD"
        )
    }
  )
  
  return(madison_allocation)
}

