
library(tidyverse)

allocate_omnium <- function(
    madison_allocation,
    omnium_rankings,
    countries,
    additional_places = 13,
    madison_automatic_places = 6,
    continents = c(
      "Africa",
      "Asia",
      "Europe",
      "Oceania",
      "Pan America"
    )
) {
  
  # Allocate Men and Women separately
  omnium_allocation <- map_dfr(
    unique(madison_allocation$gender),
    function(current_gender) {
      
      # --------------------------------
      # STEP 1: Filter data by gender
      # --------------------------------
      
      gender_madison <- madison_allocation |>
        filter(gender == current_gender)
      
      gender_omnium <- omnium_rankings |>
        filter(gender == current_gender)
      
      eligible_rankings <- gender_omnium |>
        left_join(
          countries |>
            select(country, continent),
          by = "country"
        ) |>
        arrange(desc(total_points), country)
      
      if (any(is.na(eligible_rankings$continent))) {
        stop(
          "An Omnium ranking country has no continent mapping for ",
          current_gender, "."
        )
      }
      
      # --------------------------------
      # STEP 2: Identify Madison automatic qualifiers
      # --------------------------------
      
      madison_automatic <- gender_madison |>
        filter(
          qualification_route == "Madison ranking",
          qualification_status == "Qualified"
        ) |>
        distinct(country) |>
        select(country) |>
        left_join(
          countries |>
            select(country, continent),
          by = "country"
        ) |>
        left_join(
          gender_omnium |>
            select(country, total_points, rank),
          by = "country"
        )
      
      if (nrow(madison_automatic) != madison_automatic_places) {
        stop(
          "Unexpected number of Madison automatic qualifiers for ",
          current_gender, ". Expected ",
          madison_automatic_places, " but found ",
          nrow(madison_automatic), "."
        )
      }
      
      if (any(is.na(madison_automatic$continent))) {
        stop(
          "A Madison automatic qualifier has no continent mapping for ",
          current_gender, "."
        )
      }
      
      # --------------------------------
      # STEP 3: Identify original ranking qualifiers
      # --------------------------------
      
      # These are the countries that would qualify through
      # the overall ranking before continental representation
      # is applied. We use this to identify displaced countries
      # and distinguish ranking qualifiers from continental ones.
      
      ranking_only_countries <- eligible_rankings |>
        filter(!country %in% madison_automatic$country) |>
        slice_head(n = additional_places) |>
        pull(country)
      
      # --------------------------------
      # STEP 4: Identify continents already represented
      # --------------------------------
      
      represented_continents <- madison_automatic |>
        distinct(continent) |>
        pull(continent)
      
      missing_continents <- setdiff(
        continents,
        represented_continents
      )
      
      # --------------------------------
      # STEP 5: Find eligible countries from missing continents
      # --------------------------------
      
      continental_candidates <- eligible_rankings |>
        filter(
          !country %in% madison_automatic$country,
          continent %in% missing_continents
        ) |>
        arrange(desc(total_points), country)
      
      # Identify continents with no eligible country
      available_continents <- continental_candidates |>
        distinct(continent) |>
        pull(continent)
      
      unavailable_continents <- setdiff(
        missing_continents,
        available_continents
      )
      
      # Check for ties for continental places
      continental_ties <- continental_candidates |>
        group_by(continent) |>
        filter(total_points == max(total_points)) |>
        summarise(n = n(), .groups = "drop") |>
        filter(n > 1)
      
      if (nrow(continental_ties) > 0) {
        warning(
          "Continental representation tie detected for ",
          current_gender, "."
        )
      }
      
      # Select the highest-ranked eligible country
      # from each continent that needs representation
      continental_places <- continental_candidates |>
        group_by(continent) |>
        arrange(desc(total_points), country, .by_group = TRUE) |>
        slice_head(n = 1) |>
        ungroup() |>
        mutate(
          qualification_route = if_else(
            country %in% ranking_only_countries,
            "Omnium ranking",
            "Continental representation"
          ),
          qualification_status = "Qualified"
        )
      
      # Calculate continental qualification margins
      continental_margins <- continental_candidates |>
        group_by(continent) |>
        arrange(desc(total_points), country, .by_group = TRUE) |>
        summarise(
          next_points = if (n() >= 2) total_points[2] else NA_real_,
          .groups = "drop"
        )
      
      continental_places <- continental_places |>
        left_join(continental_margins, by = "continent") |>
        mutate(
          qualification_margin = total_points - next_points,
          qualification_note = if_else(
            country %in% ranking_only_countries,
            "Qualified through the overall Omnium ranking.",
            paste0(
              "Selected as the highest-ranked eligible country from ",
              continent,
              " to ensure continental representation."
            )
          )
        ) |>
        select(-next_points)
      
      # --------------------------------
      # STEP 6: Create reserved places
      # --------------------------------
      
      if (length(unavailable_continents) > 0) {
        
        reserved_places <- tibble(
          country = paste("Reserved -", unavailable_continents),
          total_points = NA_real_,
          rank = NA_integer_,
          continent = unavailable_continents,
          qualification_route = "Reserved continental place",
          qualification_status = "Reserved",
          qualification_margin = NA_real_,
          qualification_note = paste(
            "No eligible country was available from this continent;",
            "the place remains reserved."
          )
        )
        
      } else {
        
        reserved_places <- tibble(
          country = character(),
          total_points = double(),
          rank = integer(),
          continent = character(),
          qualification_route = character(),
          qualification_status = character(),
          qualification_margin = double(),
          qualification_note = character()
        )
      }
      
      # Calculate ranking cutoff accounting for reserved places
      original_ranking_cutoff_points <- eligible_rankings |>
        filter(!country %in% madison_automatic$country) |>
        slice_head(n = additional_places - nrow(reserved_places)) |>
        summarise(cutoff = min(total_points)) |>
        pull(cutoff)
      
      # --------------------------------
      # STEP 7: Calculate remaining places
      # --------------------------------
      
      remaining_places <- additional_places -
        nrow(continental_places) -
        nrow(reserved_places)
      
      if (remaining_places < 0) {
        stop(
          "Continental places exceed available additional places for ",
          current_gender, "."
        )
      }
      
      # --------------------------------
      # STEP 8: Fill remaining places from overall ranking
      # --------------------------------
      
      already_allocated <- c(
        madison_automatic$country,
        continental_places$country
      )
      
      ranking_candidates <- eligible_rankings |>
        filter(!country %in% already_allocated) |>
        arrange(desc(total_points), country)
      
      if (nrow(ranking_candidates) < remaining_places) {
        stop(
          "Not enough eligible countries to fill Omnium places for ",
          current_gender, "."
        )
      }
      
      if (remaining_places > 0) {
        
        ranking_cutoff_points <- ranking_candidates |>
          slice_head(n = remaining_places) |>
          summarise(cutoff = min(total_points)) |>
          pull(cutoff)
        
        if (
          sum(ranking_candidates$total_points == ranking_cutoff_points) > 1
        ) {
          warning(
            "Tie detected at Omnium ranking cutoff for ",
            current_gender, "."
          )
        }
        
      } else {
        ranking_cutoff_points <- NA_real_
      }
      
      ranking_places <- ranking_candidates |>
        slice_head(n = remaining_places) |>
        mutate(
          qualification_route = "Omnium ranking",
          qualification_status = "Qualified",
          qualification_margin = total_points - ranking_cutoff_points,
          qualification_note = "Qualified through the overall Omnium ranking."
        )
      
      # --------------------------------
      # STEP 9: Identify displaced countries
      # --------------------------------
      
      # Compare the original ranking top 13 with the final
      # additional allocation. Any original ranking qualifier
      # missing from the final allocation was displaced by
      # continental representation.
      
      final_additional_countries <- c(
        continental_places$country,
        ranking_places$country
      )
      
      displaced_countries <- setdiff(
        ranking_only_countries,
        final_additional_countries
      )
      
      # --------------------------------
      # STEP 10: Combine all allocated places
      # --------------------------------
      
      qualified_countries <- bind_rows(
        
        madison_automatic |>
          mutate(
            qualification_route = "Madison automatic",
            qualification_status = "Qualified",
            qualification_margin = NA_real_,
            qualification_note = "Qualified automatically through Madison."
          ),
        
        continental_places,
        
        ranking_places
        
      ) |>
        mutate(
          gender = current_gender,
          is_automatic = qualification_route == "Madison automatic",
          discipline = "OMN"
        ) |>
        distinct(country, .keep_all = TRUE)
      
      # --------------------------------
      # STEP 11: Include every ranked country
      # --------------------------------
      
      # Include automatic qualifiers even if they have
      # no Omnium ranking result.
      missing_automatic <- madison_automatic |>
        filter(!country %in% eligible_rankings$country) |>
        transmute(
          gender = current_gender,
          country,
          total_points = NA_real_,
          rank = NA_integer_,
          continent
        )
      
      all_ranked_countries <- bind_rows(
        eligible_rankings,
        missing_automatic
      )
      
      omnium_gender_allocation <- all_ranked_countries |>
        left_join(
          qualified_countries |>
            select(
              country,
              qualification_route,
              qualification_status,
              qualification_margin,
              is_automatic,
              qualification_note
            ),
          by = "country"
        ) |>
        mutate(
          gender = current_gender,
          
          qualification_route = replace_na(
            qualification_route,
            "Not qualified"
          ),
          
          qualification_status = replace_na(
            qualification_status,
            "Not qualified"
          ),
          
          is_automatic = replace_na(is_automatic, FALSE),
          
          qualification_note = case_when(
            qualification_status == "Not qualified" &
              country %in% displaced_countries ~
              paste(
                "Would have qualified through the overall Omnium ranking,",
                "but was displaced to ensure continental representation."
              ),
            
            qualification_status == "Not qualified" ~
              "Did not secure an Omnium qualification place.",
            
            TRUE ~ qualification_note
          ),
          
          qualification_margin = case_when(
            qualification_route == "Reserved continental place" ~ NA_real_,
            
            !is.na(total_points) ~
              total_points - original_ranking_cutoff_points,
            
            TRUE ~ NA_real_
          ),
          
          discipline = "OMN"
        )
      
      # Add reserved places to output
      reserved_places <- reserved_places |>
        mutate(
          gender = current_gender,
          is_automatic = FALSE,
          discipline = "OMN"
        )
      
      bind_rows(
        omnium_gender_allocation,
        reserved_places
      )
    }
  )
  
  # --------------------------------
  # STEP 12: Final allocation checks
  # --------------------------------
  
  if (anyDuplicated(
    omnium_allocation |> select(gender, country)
  )) {
    stop("Duplicate countries in Omnium allocation.")
  }
  
  expected_places <- madison_automatic_places +
    additional_places
  
  allocation_check <- omnium_allocation |>
    filter(
      qualification_status %in% c("Qualified", "Reserved")
    ) |>
    count(gender, name = "allocated_places")
  
  if (
    nrow(allocation_check) != length(unique(madison_allocation$gender)) ||
    any(allocation_check$allocated_places != expected_places)
  ) {
    stop(
      "Omnium allocation does not match the expected ",
      expected_places, " places per gender."
    )
  }
  
  # Check that all five continents are represented
  for (current_gender in unique(omnium_allocation$gender)) {
    
    represented_final_continents <- omnium_allocation |>
      filter(
        gender == current_gender,
        qualification_status %in% c("Qualified", "Reserved")
      ) |>
      pull(continent) |>
      unique()
    
    missing_final_continents <- setdiff(
      continents,
      represented_final_continents
    )
    
    if (length(missing_final_continents) > 0) {
      stop(
        "Final Omnium allocation does not represent every continent for ",
        current_gender, ": ",
        paste(missing_final_continents, collapse = ", ")
      )
    }
  }
  
  return(
    omnium_allocation |>
      arrange(gender, desc(total_points), country)
  )
}
