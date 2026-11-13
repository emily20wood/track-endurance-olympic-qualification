
library(tidyverse)

# 1. Check for duplicate competition IDs
stopifnot(
  !anyDuplicated(competitions$competition_id)
)

# 2. Check for duplicate country results
duplicate_results <- results |> 
  count(gender, country, discipline, competition_id) |> 
  filter(n > 1)

if (nrow(duplicate_results) > 0) {
  print(duplicate_results)
  stop("Duplicate country results found.")
}

# 3. Check that all countries exist in countries.csv
unknown_countries <- results |> 
  distinct(country) |> 
  anti_join(countries, by = "country")

if (nrow(unknown_countries) > 0) {
  print(unknown_countries)
  stop("Unknown countries found in results.")
}

# 4. Check that all competitions exist
unknown_competitions <- results |> 
  distinct(competition_id) |> 
  anti_join(competitions, by = "competition_id")

if (nrow(unknown_competitions) > 0) {
  print(unknown_competitions)
  stop("Unknown competitions found in results.")
}

# 5. Check discipline names
valid_disciplines <- c("TP", "MAD", "OMN")

invalid_disciplines <- results |> 
  filter(!discipline %in% valid_disciplines)

if (nrow(invalid_disciplines) > 0) {
  print(invalid_disciplines)
  stop("Invalid discipline names found.")
}

# Validate gender
valid_genders <- c("Men", "Women")

invalid_genders <- results |>
  filter(!gender %in% valid_genders | is.na(gender))

if (nrow(invalid_genders) > 0) {
  stop("Invalid or missing gender values found in results.")
}

# 6. Check missing placings
# Blank placings are allowed and treated as zero-point results
message(
  sum(is.na(results$placing)),
  " results have blank placings and will receive 0 points."
)

invalid_placings <- results |> 
  filter(!is.na(placing), placing < 1)

if (nrow(invalid_placings) > 0) {
  print(invalid_placings)
  stop("Invalid placings found.")
}

message("All basic data validation checks passed!")