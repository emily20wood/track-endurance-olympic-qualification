
# ==========================================
# OLYMPIC CYCLING QUALIFICATION TRACKER
# MASTER SCRIPT
# ==========================================

library(tidyverse)
library(here)

# ------------------------------------------
# STEP 1: Create output folders if needed
# ------------------------------------------

dir.create(
  here("data", "processed"),
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  here("outputs", "rankings"),
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  here("outputs", "qualification"),
  recursive = TRUE,
  showWarnings = FALSE
)

# ------------------------------------------
# STEP 2: Load data
# ------------------------------------------

source(here("scripts", "01_load_data.R"))

# ------------------------------------------
# STEP 3: Validate data
# ------------------------------------------

source(here("scripts", "02_validate_data.R"))

# ------------------------------------------
# STEP 4: Calculate competition points
# ------------------------------------------

source(here("scripts", "03_calculate_points.R"))

# ------------------------------------------
# STEP 5: Calculate country rankings
# ------------------------------------------

source(here("scripts", "04_calculate_rankings.R"))

# ------------------------------------------
# STEP 6: Load allocation functions
# ------------------------------------------

source(here("scripts", "05_allocate_tp.R"))
source(here("scripts", "06_allocate_madison.R"))
source(here("scripts", "07_allocate_omnium.R"))

# ------------------------------------------
# STEP 7: Allocate Team Pursuit places
# ------------------------------------------

tp_allocation <- allocate_tp(
  tp_rankings,
  places = 8
)

# ------------------------------------------
# STEP 8: Allocate Madison places
# ------------------------------------------

madison_allocation <- allocate_madison(
  tp_allocation,
  madison_rankings,
  additional_places = 6
)

# ------------------------------------------
# STEP 9: Allocate Omnium places
# ------------------------------------------

omnium_allocation <- allocate_omnium(
  madison_allocation,
  omnium_rankings,
  countries,
  additional_places = 13,
  madison_automatic_places = 6
)

# ------------------------------------------
# STEP 10: Save qualification outputs
# ------------------------------------------

write_csv(
  tp_allocation,
  here("outputs", "qualification", "tp_allocation.csv")
)

write_csv(
  madison_allocation,
  here("outputs", "qualification", "madison_allocation.csv")
)

write_csv(
  omnium_allocation,
  here("outputs", "qualification", "omnium_allocation.csv")
)

# ------------------------------------------
# STEP 11: Save combined qualification data
# ------------------------------------------

all_qualification <- bind_rows(
  tp_allocation,
  madison_allocation,
  omnium_allocation
)

write_csv(
  all_qualification,
  here("outputs", "qualification", "all_qualification.csv")
)

# ------------------------------------------
# STEP 12: Completion message
# ------------------------------------------

cat("\n")
cat("==========================================\n")
cat("QUALIFICATION CALCULATIONS COMPLETE\n")
cat("==========================================\n")

cat("\nTeam Pursuit countries:",
    sum(tp_allocation$qualification_status == "Qualified"))

cat("\nMadison countries:",
    sum(madison_allocation$qualification_status == "Qualified"))

cat("\nOmnium qualified countries:",
    sum(omnium_allocation$qualification_status == "Qualified"))

cat("\nOmnium reserved quotas:",
    sum(omnium_allocation$qualification_status == "Reserved"))

cat("\n\nOutput files saved successfully.\n")

