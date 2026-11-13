# Import raw CSV files
competitions <- read.csv(
  here::here("data", "raw", "competitions.csv")
) |> 
  janitor::clean_names()

results <- read.csv(
  here::here("data", "raw", "results.csv")
) |> 
  janitor::clean_names()

points_table <- read.csv(
  here::here("data", "raw", "points_table.csv")
) |> 
  janitor::clean_names()

countries <- read.csv(
  here::here("data", "raw", "countries.csv")
) |> 
  janitor::clean_names()

qualification_config <- read.csv(
  here::here("data", "raw", "qualification_config.csv")
) |> 
  janitor::clean_names()
