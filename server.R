
# ==========================================
# OLYMPIC CYCLING QUALIFICATION TRACKER
# SERVER
# ==========================================

library(shiny)
library(tidyverse)
library(DT)
library(here)

format_status <- function(table) {
  formatStyle(
    table,
    "qualification_status",
    target = "row",
    backgroundColor = styleEqual(
      c("Qualified", "Reserved", "Not qualified"),
      c("#DDF2E3", "#FFF0D2", "#FBE3E3")
    ),
    color = "black"
  )
}


# ------------------------------------------
# STEP 1: LOAD QUALIFICATION DATA
# ------------------------------------------

read_output <- function(filename) {
  
  file_path <- here("outputs", "qualification", filename)
  
  if (!file.exists(file_path)) {
    stop(
      paste(
        "Missing output file:",
        file_path,
        "- Please run 08_run_qualification.R first."
      )
    )
  }
  
  read_csv(file_path, show_col_types = FALSE)
}

tp_data <- read_output("tp_allocation.csv")

madison_data <- read_output("madison_allocation.csv")

omnium_data <- read_output("omnium_allocation.csv")


# ------------------------------------------
# STEP 2: LOAD COMPETITION RESULTS
# ------------------------------------------

results_file <- here(
  "data",
  "processed",
  "counting_results.csv"
)

if (file.exists(results_file)) {
  
  results_data <- read_csv(
    results_file,
    show_col_types = FALSE
  )
  
} else {
  
  results_data <- tibble()
  
  warning(
    "counting_results.csv not found. Competition Results page will be empty."
  )
}


# Load competition details if available

competitions_file <- here(
  "data",
  "raw",
  "competitions.csv"
)

if (file.exists(competitions_file)) {
  
  competitions_data <- read_csv(
    competitions_file,
    show_col_types = FALSE
  )
  
  if (
    "competition_id" %in% names(results_data) &&
    "competition_id" %in% names(competitions_data)
  ) {
    
    competition_columns <- c(
      "competition_id",
      "competition_type",
      "year"
    )
    
    competition_columns <- intersect(
      competition_columns,
      names(competitions_data)
    )
    
    competition_details <- competitions_data |>
      select(all_of(competition_columns))
    
    for (column in setdiff(
      competition_columns,
      "competition_id"
    )) {
      
      if (column %in% names(results_data)) {
        competition_details <- competition_details |>
          select(-all_of(column))
      }
      
    }
    
    results_data <- results_data |>
      left_join(
        competition_details,
        by = "competition_id"
      )
    
  }
}


# ------------------------------------------
# LOAD SCENARIO TRACKER REFERENCE DATA
# ------------------------------------------

countries_file <- here(
  "data",
  "raw",
  "countries.csv"
)

if (!file.exists(countries_file)) {
  stop("countries.csv not found in data/raw.")
}

countries_data <- read_csv(
  countries_file,
  show_col_types = FALSE
)

# ------------------------------------------
# LOAD COMPETITION STATUS
# ------------------------------------------

competition_status_file <- here(
  "data",
  "processed",
  "competition_status.csv"
)

if (!file.exists(competition_status_file)) {
  stop("Competition status file not found. Please run 02_validate_data.R.")
}

competition_status_data <- read_csv(
  competition_status_file,
  show_col_types = FALSE
)


# ------------------------------------------
# SCENARIO MATRIX SETUP
# ------------------------------------------

# Define the order and continent columns for competitions
scenario_competition_columns <- bind_rows(
  
  # Regular competitions
  competitions_data |>
    filter(competition_type != "Continental Championships") |>
    mutate(column_continent = NA_character_),
  
  # Continental Championships, split by continent
  competitions_data |>
    filter(competition_type == "Continental Championships") |>
    crossing(
      column_continent = c(
        "Africa",
        "Asia",
        "Europe",
        "Oceania",
        "Pan America"
      )
    )
  
) |>
  mutate(
    column_key = if_else(
      is.na(column_continent),
      competition_id,
      paste(competition_id, column_continent, sep = "_")
    ),
    
    column_label = case_when(
      competition_id == "WCH2026" ~ "WCH 26",
      competition_id == "WCH2027" ~ "WCH 27",
      
      str_starts(competition_id, "WCU") ~ paste0(
        "WCU ",
        str_sub(str_extract(competition_id, "WCU\\d{4}"), 6, 7),
        " R",
        str_extract(competition_id, "\\d+$")
      ),
      
      competition_type == "Continental Championships" ~ paste0(
        "CC ",
        str_sub(as.character(year), 3, 4),
        " ",
        case_when(
          column_continent == "Africa" ~ "AFR",
          column_continent == "Asia" ~ "ASI",
          column_continent == "Europe" ~ "EUR",
          column_continent == "Oceania" ~ "OCE",
          column_continent == "Pan America" ~ "PAN"
        )
      )
    ),
    competition_order = match(
      competition_id,
      c(
        "WCH2026",
        "CC2027",
        "WCU2027_1",
        "WCU2027_2",
        "WCU2027_3",
        "WCH2027",
        "CC2028",
        "WCU2028_1",
        "WCU2028_2",
        "WCU2028_3"
      )
    ),
    continent_order = match(
      column_continent,
      c("Africa", "Asia", "Europe", "Oceania", "Pan America")
    )
  ) |>
  left_join(
    competition_status_data |>
      select(competition_id, continent, status),
    by = c(
      "competition_id",
      "column_continent" = "continent"
    )
  ) |>
  arrange(competition_order, continent_order)


# Function to build one discipline's scenario matrix
build_scenario_matrix <- function(
    selected_discipline,
    selected_gender,
    added_countries = character(),
    hide_completed = FALSE
) {
  
  # Identify countries already present in this discipline
  existing_countries <- results |>
    filter(
      discipline == selected_discipline,
      gender == selected_gender
    ) |>
    distinct(country) |>
    pull(country)
  
  # Combine existing and manually added countries
  matrix_countries <- sort(unique(c(
    existing_countries,
    added_countries
  )))
  
  country_rows <- tibble(
    country = matrix_countries
  )
  
  # Identify the columns to display
  visible_columns <- scenario_competition_columns
  
  if (hide_completed) {
    visible_columns <- visible_columns |>
      filter(status != "Completed")
  }
  
  # Get actual results for this discipline and gender
  actual_cells <- results |>
    filter(
      discipline == selected_discipline,
      gender == selected_gender
    ) |>
    left_join(
      countries_data |>
        select(country, country_continent = continent),
      by = "country"
    ) |>
    inner_join(
      visible_columns,
      by = "competition_id"
    ) |>
    filter(
      is.na(column_continent) |
        country_continent == column_continent
    ) |>
    select(
      country,
      column_key,
      placing
    )
  
  # Convert actual results into matrix columns
  actual_matrix <- actual_cells |>
    pivot_wider(
      names_from = column_key,
      values_from = placing
    )
  
  # Add actual results to the country rows
  scenario_matrix <- country_rows |>
    left_join(actual_matrix, by = "country")
  
  # Ensure every visible competition has a column
  for (column in visible_columns$column_key) {
    if (!column %in% names(scenario_matrix)) {
      scenario_matrix[[column]] <- NA_integer_
    }
  }
  
  # Keep columns in the correct competition order
  scenario_matrix <- scenario_matrix |>
    select(
      country,
      all_of(visible_columns$column_key)
    )
  
  names(scenario_matrix) <- c("Country", visible_columns$column_label)
  
  scenario_matrix <- scenario_matrix |>
    mutate(
      across(-Country, as.integer)
    )
  
  scenario_matrix
}


# ------------------------------------------
# LOAD RAW DATA FOR SCENARIO CALCULATIONS
# ------------------------------------------

# Original competition results
results <- read_csv(
  here("data", "raw", "results.csv"),
  show_col_types = FALSE
)

# Competition reference data
competitions <- competitions_data

# Points allocation table
points_table <- read_csv(
  here("data", "raw", "points_table.csv"),
  show_col_types = FALSE
)

# Country and continent reference data
countries <- countries_data

# Load scenario calculation functions
source(
  here("scripts", "09_scenario_calculations.R"),
  local = TRUE
)

# ------------------------------------------
# STEP 3: SERVER FUNCTION
# ------------------------------------------

server <- function(input, output, session) {
  
  
  # ----------------------------------------
  # GENDER FILTER
  # ----------------------------------------
  
  filter_gender <- function(data) {
    
    if (is.null(input$gender_filter)) {
      return(data)
    }
    
    data |>
      filter(gender == input$gender_filter)
    
  }
  
  
  # ----------------------------------------
  # GLOBAL COUNTRY SEARCH
  # ----------------------------------------
  
  observeEvent(input$gender_filter, {
    
    gender_countries <- bind_rows(
      
      filter_gender(tp_data) |>
        select(country),
      
      filter_gender(madison_data) |>
        select(country),
      
      filter_gender(omnium_data) |>
        filter(!str_starts(country, "Reserved -")) |>
        select(country)
      
    ) |>
      distinct() |>
      arrange(country) |>
      pull(country)
    
    updateSelectInput(
      session,
      "country_filter",
      choices = c("All countries", gender_countries),
      selected = "All countries"
    )
    
  }, ignoreInit = FALSE)
  
  
  # Helper function for country filtering
  
  filter_country <- function(data) {
    
    data <- filter_gender(data)
    
    if (
      is.null(input$country_filter) ||
      input$country_filter == "All countries"
    ) {
      return(data)
    }
    
    data |>
      filter(country == input$country_filter)
    
  }
  
  
  # ----------------------------------------
  # DATA STATUS
  # ----------------------------------------
  
  output$data_status <- renderUI({
    
    tags$span(
      style = "color: #D99A24; font-weight: 600;",
      icon("circle-exclamation"),
      " PROVISIONAL RANKINGS"
    )
    
  })
  
  
  # ----------------------------------------
  # STEP 4: OVERVIEW METRICS
  # ----------------------------------------
  
  
  output$overview_metrics <- renderUI({
    
    current_tp <- filter_gender(tp_data)
    current_madison <- filter_gender(madison_data)
    current_omnium <- filter_gender(omnium_data)
    
    usa_tp <- current_tp |>
      filter(country == "USA") |>
      slice(1)
    
    usa_madison <- current_madison |>
      filter(country == "USA") |>
      slice(1)
    
    usa_omnium <- current_omnium |>
      filter(country == "USA") |>
      slice(1)
    
    
    # Helper to create qualification cards
    
    make_usa_card <- function(data, discipline) {
      
      if (nrow(data) == 0) {
        
        status <- "No data"
        rank <- "—"
        card_colour <- "#F5F6F7"
        text_colour <- "#555555"
        
      } else {
        
        status <- data$qualification_status[[1]]
        
        rank <- if (!is.na(data$rank[[1]])) {
          paste0(data$rank[[1]], "th")
        } else {
          "—"
        }
        
        card_colour <- switch(
          status,
          "Qualified" = "#DDF2E3",
          "Not qualified" = "#FBE3E3",
          "Reserved" = "#FFF0D2",
          "#F5F6F7"
        )
        
        text_colour <- switch(
          status,
          "Qualified" = "#176B36",
          "Not qualified" = "#A12626",
          "Reserved" = "#8A5A00",
          "#555555"
        )
        
      }
      
      div(
        style = paste0(
          "background-color: ", card_colour, ";",
          "color: ", text_colour, ";",
          "padding: 20px;",
          "border-radius: 10px;",
          "min-height: 130px;",
          "text-align: center;"
        ),
        
        h4(discipline, style = "font-weight: 700;"),
        
        h2(rank, style = "font-weight: 700;"),
        
        strong(toupper(status))
        
      )
      
    }
    
    
    fluidRow(
      
      column(
        4,
        make_usa_card(usa_tp, "TEAM PURSUIT")
      ),
      
      column(
        4,
        make_usa_card(usa_madison, "MADISON")
      ),
      
      column(
        4,
        make_usa_card(usa_omnium, "OMNIUM")
      )
      
    )
    
  })
  
  
  
  # ----------------------------------------
  # STEP 5: OVERVIEW TABLE
  # ----------------------------------------
  
  
  output$overview_table <- renderUI({
    
    current_tp <- filter_gender(tp_data)
    current_madison <- filter_gender(madison_data)
    current_omnium <- filter_gender(omnium_data)
    
    
    get_usa <- function(data) {
      
      data |>
        filter(country == "USA") |>
        slice(1)
      
    }
    
    
    usa_tp <- get_usa(current_tp)
    usa_madison <- get_usa(current_madison)
    usa_omnium <- get_usa(current_omnium)
    
    
    # Helper to generate discipline summaries
    
    
    get_summary <- function(data, discipline) {
      
      if (nrow(data) == 0) {
        return(
          paste(discipline, "ranking data is not currently available.")
        )
      }
      
      rank <- data$rank[[1]]
      status <- data$qualification_status[[1]]
      
      route <- if ("qualification_route" %in% names(data)) {
        data$qualification_route[[1]]
      } else {
        NA_character_
      }
      
      rank_text <- if (!is.na(rank)) {
        paste0("currently ranked ", rank)
      } else {
        "has no current ranking"
      }
      
      if (status == "Qualified") {
        
        pathway <- case_when(
          
          route == "Team Pursuit automatic" ~
            "qualifies automatically through Team Pursuit",
          
          route == "Madison automatic" ~
            "qualifies automatically through Madison",
          
          route == "Madison ranking" ~
            "qualifies through the Madison ranking",
          
          route == "Omnium ranking" ~
            "qualifies through the overall Omnium ranking",
          
          route == "Continental representation" ~
            "qualifies through continental representation",
          
          discipline == "Team Pursuit" ~
            "qualifies through the Team Pursuit ranking",
          
          TRUE ~
            "is currently qualified"
        )
        
        paste0(
          discipline, ": USA is ",
          rank_text, " and ",
          pathway, "."
        )
        
      } else if (status == "Reserved") {
        
        paste0(
          discipline, ": USA is ",
          rank_text,
          " but its place is currently reserved."
        )
        
      } else {
        
        paste0(
          discipline, ": USA is ",
          rank_text,
          " and is currently outside the qualification places."
        )
        
      }
      
    }
    
   
    tagList(
      
      h4("USA qualification outlook", class = "section-heading"),
      
      p(
        get_summary(usa_tp, "Team Pursuit")
      ),
      
      p(
        get_summary(usa_madison, "Madison")
      ),
      
      p(
        get_summary(usa_omnium, "Omnium")
      )
      
    )
    
  })
  
  
  
  # ----------------------------------------
  # STEP 6: TEAM PURSUIT
  # ----------------------------------------
  
  output$tp_summary <- renderUI({
    
    current_tp <- filter_gender(tp_data)
    
    qualified <- sum(
      current_tp$qualification_status == "Qualified",
      na.rm = TRUE
    )
    
    tags$p(
      paste(
        qualified,
        "of 8 qualification places currently allocated."
      )
    )
    
  })
  
  
  output$tp_table <- renderDT({
    
    display_data <- filter_country(tp_data)
    
    columns_to_show <- c(
      "rank",
      "country",
      "total_points",
      "qualification_status",
      "qualification_margin"
    )
    
    display_data <- display_data |>
      select(any_of(columns_to_show))
    
    table <- datatable(
      display_data,
      rownames = FALSE,
      colnames = c(
        "Rank",
        "Country",
        "Points",
        "Qualification status",
        "Points from cutoff"
      ),
      options = list(
        pageLength = 25,
        scrollX = TRUE
      )
    )
    
    format_status(table)
    
  })
  
  
  # ----------------------------------------
  # STEP 7: MADISON
  # ----------------------------------------
  
  output$madison_summary <- renderUI({
    
    current_madison <- filter_gender(madison_data)
    
    qualified <- sum(
      current_madison$qualification_status == "Qualified",
      na.rm = TRUE
    )
    
    automatic <- sum(
      current_madison$qualification_route == "TP automatic",
      na.rm = TRUE
    )
    
    additional <- sum(
      current_madison$qualification_route == "Madison ranking",
      na.rm = TRUE
    )
    
    tags$p(
      paste0(
        qualified, " of 14 places allocated: ",
        automatic, " Team Pursuit automatic and ",
        additional, " Madison ranking."
      )
    )
    
  })
  
  
  output$madison_table <- renderDT({
    
    display_data <- filter_country(madison_data)
    
    columns_to_show <- c(
      "rank",
      "country",
      "total_points",
      "qualification_route",
      "qualification_status",
      "qualification_margin"
    )
    
    display_data <- display_data |>
      select(any_of(columns_to_show))
    
    table <- datatable(
      display_data,
      rownames = FALSE,
      colnames = c(
        "Rank",
        "Country",
        "Points",
        "Qualification route",
        "Qualification status",
        "Points from cutoff"
      ),
      options = list(
        pageLength = 25,
        scrollX = TRUE
      )
    )
    
    format_status(table)
    
  })
  
  
  # ----------------------------------------
  # STEP 8: OMNIUM
  # ----------------------------------------
  
  output$omnium_summary <- renderUI({
    
    current_omnium <- filter_gender(omnium_data)
    
    qualified <- sum(
      current_omnium$qualification_status == "Qualified",
      na.rm = TRUE
    )
    
    reserved <- sum(
      current_omnium$qualification_status == "Reserved",
      na.rm = TRUE
    )
    
    tags$p(
      paste0(
        qualified, " countries qualified and ",
        reserved, " continental quotas reserved."
      )
    )
    
  })
  
  
  # Continental representation table
  
  output$continent_table <- renderDT({
    
    continent_summary <- filter_gender(omnium_data) |>
      group_by(continent) |>
      summarise(
        
        `Countries qualified` = paste(
          country[qualification_status == "Qualified"],
          collapse = ", "
        ),
        
        `Reserved quota` = any(
          qualification_status == "Reserved"
        ),
        
        Status = case_when(
          any(qualification_status == "Reserved") ~ "Reserved",
          any(qualification_status == "Qualified") ~ "Represented",
          TRUE ~ "Not represented"
        ),
        
        .groups = "drop"
        
      ) |>
      arrange(continent)
    
    datatable(
      continent_summary,
      rownames = FALSE,
      options = list(
        dom = "t",
        paging = FALSE,
        ordering = FALSE,
        scrollX = TRUE
      )
    )
    
  })
  
  
  # Omnium country rankings
  
  output$omnium_table <- renderDT({
    
    display_data <- filter_country(omnium_data)
    
    columns_to_show <- c(
      "rank",
      "country",
      "continent",
      "total_points",
      "qualification_route",
      "qualification_status",
      "qualification_margin",
      "qualification_note"
    )
    
    display_data <- display_data |>
      select(any_of(columns_to_show))
    
    display_data <- display_data |>
      arrange(
        match(
          qualification_status,
          c("Qualified", "Reserved", "Not qualified")
        ),
        rank
      )
    
    table <- datatable(
      display_data,
      rownames = FALSE,
      colnames = c(
        "Rank",
        "Country",
        "Continent",
        "Points",
        "Qualification route",
        "Qualification status",
        "Points from cutoff",
        "Qualification explanation"
      ),
      options = list(
        pageLength = 25,
        scrollX = TRUE
      )
    )
    
    format_status(table)
    
  })
  
  
  # ----------------------------------------
  # STEP 9: COUNTRY PROFILE
  # ----------------------------------------
  
  output$country_profile <- renderUI({
    
    selected_country <- input$country_filter
    
    if (
      is.null(selected_country) ||
      selected_country == "" ||
      selected_country == "All countries"
    ) {
      return(p("Please select a country using the country search above."))
    }
    
    
    # Helper to retrieve the country's allocation row
    
    get_country <- function(data) {
      
      result <- filter_gender(data) |>
        filter(country == selected_country) |>
        slice(1)
      
      if (nrow(result) == 0) {
        return(NULL)
      }
      
      result
      
    }
    
    
    # Helper to display missing values cleanly
    
    show_value <- function(x) {
      
      if (is.null(x) || length(x) == 0 || is.na(x)) {
        return("—")
      }
      
      as.character(x)
      
    }
    
    
    # Create a profile card for each discipline
    
    make_card <- function(title, data) {
      
      country_data <- get_country(data)
      
      if (is.null(country_data)) {
        return(
          wellPanel(
            h4(title),
            p("No ranking data available for this country.")
          )
        )
      }
      
      status <- show_value(country_data$qualification_status)
      
      rank <- show_value(country_data$rank)
      
      points <- show_value(round(country_data$total_points, 1))
      
      route <- if ("qualification_route" %in% names(country_data)) {
        show_value(country_data$qualification_route)
      } else {
        "—"
      }
      
      margin <- if ("qualification_margin" %in% names(country_data)) {
        show_value(round(country_data$qualification_margin, 1))
      } else {
        "—"
      }
      
      note <- if ("qualification_note" %in% names(country_data)) {
        show_value(country_data$qualification_note)
      } else {
        NULL
      }
      
      
      status_colour <- switch(
        status,
        "Qualified" = "#DDF2E3",
        "Reserved" = "#FFF0D2",
        "Not qualified" = "#FBE3E3",
        "#F5F6F7"
      )
      
      
      wellPanel(
        style = paste0(
          "background-color: ", status_colour, ";",
          "color: black;",
          "border: 1px solid #E0E0E0;",
          "border-radius: 8px;"
        ),
        
        h4(title, style = "color: black;"),
        
        tags$p(strong("Status: "), status),
        
        tags$p(strong("Current rank: "), rank),
        
        tags$p(strong("Points: "), points),
        
        tags$p(strong("Qualification route: "), route),
        
        tags$p(strong("Points margin to cutoff: "), margin),
        
        if (!is.null(note)) {
          tags$p(strong("Qualification explanation: "), note)
        }
        
      )
      
    }
    
    
    tagList(
      
      h2(selected_country),
      
      fluidRow(
        column(4, make_card("Team Pursuit", tp_data)),
        column(4, make_card("Madison", madison_data)),
        column(4, make_card("Omnium", omnium_data))
      )
      
    )
    
  })
  
  
  # ----------------------------------------
  # STEP 10: COMPETITION RESULTS
  # ----------------------------------------
  
  # Update competition choices when gender changes
  
  observeEvent(input$gender_filter, {
    
    gender_results <- filter_gender(results_data)
    
    competition_choices <- if (
      "competition_id" %in% names(gender_results)
    ) {
      sort(unique(na.omit(gender_results$competition_id)))
    } else {
      character(0)
    }
    
    updateSelectInput(
      session,
      "results_competition",
      choices = c("All competitions", competition_choices),
      selected = "All competitions"
    )
    
  }, ignoreInit = FALSE)
  
  
  output$results_table <- renderDT({
    
    req(nrow(results_data) > 0)
    
    display_data <- filter_gender(results_data)
    
    
    # Discipline filter
    
    if (
      !is.null(input$results_discipline) &&
      input$results_discipline != "All disciplines"
    ) {
      
      display_data <- display_data |>
        filter(
          discipline == input$results_discipline
        )
      
    }
    
    
    # Competition filter
    
    if (
      !is.null(input$results_competition) &&
      input$results_competition != "All competitions" &&
      "competition_id" %in% names(display_data)
    ) {
      
      display_data <- display_data |>
        filter(
          competition_id == input$results_competition
        )
      
    }
    
    
    # Country filter
    
    display_data <- filter_country(display_data)
    
    
    columns_to_show <- c(
      "country",
      "discipline",
      "competition_id",
      "competition_type",
      "year",
      "placing",
      "points"
    )
    
    display_data <- display_data |>
      select(any_of(columns_to_show))
    
    datatable(
      display_data,
      rownames = FALSE,
      options = list(
        pageLength = 25,
        scrollX = TRUE
      )
    )
    
  })
  
  
  
  # ----------------------------------------
  # STEP 11: SCENARIO TRACKER - COMPETITIONS
  # ----------------------------------------
  
  # Build readable competition choices
  
  scenario_competitions <- competitions_data |>
    distinct(competition_id, competition_type, year) |>
    arrange(year, competition_type, competition_id) |>
    mutate(
      display_name = paste0(
        competition_id,
        " - ",
        competition_type,
        " (",
        year,
        ")"
      )
    )
  
  # Populate competition selector
  
  observe({
    
    choices <- setNames(
      scenario_competitions$competition_id,
      scenario_competitions$display_name
    )
    
    updateSelectInput(
      session,
      "scenario_competition",
      choices = choices
    )
    
  })
  
  # Display selected competition details
  
  output$scenario_competition_details <- renderUI({
    
    req(input$scenario_competition)
    
    selected_competition <- scenario_competitions |>
      filter(
        competition_id == input$scenario_competition
      )
    
    req(nrow(selected_competition) > 0)
    
    tagList(
      p(
        strong("Competition type: "),
        selected_competition$competition_type[[1]]
      ),
      p(
        strong("Year: "),
        selected_competition$year[[1]]
      )
    )
    
  })
  
  
  
  
  # ----------------------------------------
  # STEP 12: CONTINENT SELECTOR
  # ----------------------------------------
  
  output$scenario_continent_selector <- renderUI({
    
    req(input$scenario_competition)
    
    selected_competition <- scenario_competitions |>
      filter(competition_id == input$scenario_competition)
    
    req(nrow(selected_competition) > 0)
    
    if (
      selected_competition$competition_type[[1]] !=
      "Continental Championships"
    ) {
      return(NULL)
    }
    
    continent_choices <- sort(
      unique(na.omit(countries_data$continent))
    )
    
    selectInput(
      "scenario_continent",
      "Select continent",
      choices = continent_choices,
      selected = NULL,
      width = "300px"
    )
    
  })
  
  
  # ----------------------------------------
  # STEP 13: TEAM PURSUIT RESULTS ENTRY
  # ----------------------------------------
  
  output$scenario_tp_entry <- renderUI({
    
    req(input$scenario_competition)
    
    selected_competition <- scenario_competitions |>
      filter(competition_id == input$scenario_competition)
    
    req(nrow(selected_competition) > 0)
    
    is_continental <-
      selected_competition$competition_type[[1]] ==
      "Continental Championships"
    
    if (is_continental) {
      req(input$scenario_continent)
      number_of_rows <- 12
      
      eligible_countries <- countries_data |>
        filter(continent == input$scenario_continent)
      
    } else {
      number_of_rows <- 18
      eligible_countries <- countries_data
    }
    
    country_choices <- setNames(
      eligible_countries$country,
      paste0(
        eligible_countries$country,
        " - ",
        eligible_countries$name
      )
    )
    
    tagList(
      
      h4("Team Pursuit results"),
      
      p(
        paste(
          "Enter up to",
          number_of_rows,
          "finishing positions."
        )
      ),
      
      lapply(seq_len(number_of_rows), function(i) {
        
        fluidRow(
          
          column(
            1,
            tags$p(
              i,
              style = "padding-top: 7px; font-weight: 600;"
            )
          ),
          
          column(
            6,
            selectInput(
              inputId = paste0("scenario_tp_country_", i),
              label = NULL,
              choices = c(
                "Select country" = "",
                country_choices
              ),
              selected = "",
              width = "100%"
            )
          ),
          
          column(
            3,
            numericInput(
              inputId = paste0("scenario_tp_placing_", i),
              label = NULL,
              value = i,
              min = 1,
              max = number_of_rows,
              step = 1,
              width = "100%"
            )
          )
          
        )
        
      })
      
    )
    
  })
  
  
  # ----------------------------------------
  # STEP 14: TEAM PURSUIT INPUT VALIDATION
  # ----------------------------------------
  
  output$scenario_tp_validation <- renderUI({
    
    req(input$scenario_competition)
    
    selected_competition <- scenario_competitions |>
      filter(competition_id == input$scenario_competition)
    
    req(nrow(selected_competition) > 0)
    
    is_continental <-
      selected_competition$competition_type[[1]] ==
      "Continental Championships"
    
    number_of_rows <- if (is_continental) 12 else 18
    
    # Collect entered results
    
    entered_results <- tibble(
      country = map_chr(
        seq_len(number_of_rows),
        function(i) {
          value <- input[[paste0("scenario_tp_country_", i)]]
          if (is.null(value)) "" else value
        }
      ),
      placing = map_dbl(
        seq_len(number_of_rows),
        function(i) {
          value <- input[[paste0("scenario_tp_placing_", i)]]
          if (is.null(value) || length(value) == 0) {
            NA_real_
          } else {
            as.numeric(value)
          }
        }
      )
    ) |>
      filter(country != "")
    
    if (nrow(entered_results) == 0) {
      return(
        tags$p(
          "No results entered yet.",
          style = "color: #777777;"
        )
      )
    }
    
    # Check for duplicate countries
    
    duplicate_countries <- entered_results |>
      count(country) |>
      filter(n > 1) |>
      pull(country)
    
    # Check for duplicate placings
    
    duplicate_placings <- entered_results |>
      filter(!is.na(placing)) |>
      count(placing) |>
      filter(n > 1) |>
      pull(placing)
    
    # Check for invalid placings
    
    invalid_placings <- entered_results |>
      filter(
        is.na(placing) |
          placing < 1 |
          placing > number_of_rows |
          placing != floor(placing)
      )
    
    # Build validation messages
    
    messages <- character(0)
    
    if (length(duplicate_countries) > 0) {
      messages <- c(
        messages,
        paste(
          "Country entered more than once:",
          paste(duplicate_countries, collapse = ", ")
        )
      )
    }
    
    if (length(duplicate_placings) > 0) {
      messages <- c(
        messages,
        paste(
          "Finishing position used more than once:",
          paste(duplicate_placings, collapse = ", ")
        )
      )
    }
    
    if (nrow(invalid_placings) > 0) {
      messages <- c(
        messages,
        "Every selected country must have a valid finishing position."
      )
    }
    
    if (length(messages) > 0) {
      
      return(
        div(
          style = paste0(
            "background-color: #FBE3E3;",
            "color: #A12626;",
            "padding: 12px;",
            "border-radius: 8px;",
            "margin-top: 15px;"
          ),
          strong("Please correct the following:"),
          tags$ul(
            lapply(messages, tags$li)
          )
        )
      )
      
    }
    
    div(
      style = paste0(
        "background-color: #DDF2E3;",
        "color: #176B36;",
        "padding: 12px;",
        "border-radius: 8px;",
        "margin-top: 15px;"
      ),
      strong("Results look valid."),
      p(
        paste(
          nrow(entered_results),
          "countries entered."
        )
      )
    )
    
  })
  
  
  
  # ----------------------------------------
  # STEP 15: MADISON RESULTS ENTRY
  # ----------------------------------------
  
  output$scenario_madison_entry <- renderUI({
    
    req(input$scenario_competition)
    
    selected_competition <- scenario_competitions |>
      filter(competition_id == input$scenario_competition)
    
    req(nrow(selected_competition) > 0)
    
    is_continental <-
      selected_competition$competition_type[[1]] ==
      "Continental Championships"
    
    if (is_continental) {
      req(input$scenario_continent)
      number_of_rows <- 12
      
      eligible_countries <- countries_data |>
        filter(continent == input$scenario_continent)
      
    } else {
      number_of_rows <- 18
      eligible_countries <- countries_data
    }
    
    country_choices <- setNames(
      eligible_countries$country,
      paste0(
        eligible_countries$country,
        " - ",
        eligible_countries$name
      )
    )
    
    tagList(
      
      p(
        paste(
          "Enter up to",
          number_of_rows,
          "Madison finishing positions."
        )
      ),
      
      lapply(seq_len(number_of_rows), function(i) {
        
        fluidRow(
          
          column(
            1,
            tags$p(
              i,
              style = "padding-top: 7px; font-weight: 600;"
            )
          ),
          
          column(
            6,
            selectInput(
              inputId = paste0("scenario_madison_country_", i),
              label = NULL,
              choices = c(
                "Select country" = "",
                country_choices
              ),
              selected = "",
              width = "100%"
            )
          ),
          
          column(
            3,
            numericInput(
              inputId = paste0("scenario_madison_placing_", i),
              label = NULL,
              value = i,
              min = 1,
              max = number_of_rows,
              step = 1,
              width = "100%"
            )
          )
          
        )
        
      })
      
    )
    
  })
  
  
  
  # ----------------------------------------
  # STEP 16: MADISON INPUT VALIDATION
  # ----------------------------------------
  
  output$scenario_madison_validation <- renderUI({
    
    req(input$scenario_competition)
    
    selected_competition <- scenario_competitions |>
      filter(competition_id == input$scenario_competition)
    
    req(nrow(selected_competition) > 0)
    
    is_continental <-
      selected_competition$competition_type[[1]] ==
      "Continental Championships"
    
    number_of_rows <- if (is_continental) 12 else 18
    
    entered_results <- tibble(
      country = map_chr(
        seq_len(number_of_rows),
        function(i) {
          value <- input[[paste0("scenario_madison_country_", i)]]
          if (is.null(value)) "" else value
        }
      ),
      placing = map_dbl(
        seq_len(number_of_rows),
        function(i) {
          value <- input[[paste0("scenario_madison_placing_", i)]]
          if (is.null(value) || length(value) == 0) {
            NA_real_
          } else {
            as.numeric(value)
          }
        }
      )
    ) |>
      filter(country != "")
    
    if (nrow(entered_results) == 0) {
      return(tags$p("No Madison results entered yet."))
    }
    
    duplicate_countries <- entered_results |>
      count(country) |>
      filter(n > 1) |>
      pull(country)
    
    duplicate_placings <- entered_results |>
      filter(!is.na(placing)) |>
      count(placing) |>
      filter(n > 1) |>
      pull(placing)
    
    invalid_placings <- entered_results |>
      filter(
        is.na(placing) |
          placing < 1 |
          placing > number_of_rows |
          placing != floor(placing)
      )
    
    messages <- character(0)
    
    if (length(duplicate_countries) > 0) {
      messages <- c(
        messages,
        paste(
          "Country entered more than once:",
          paste(duplicate_countries, collapse = ", ")
        )
      )
    }
    
    if (length(duplicate_placings) > 0) {
      messages <- c(
        messages,
        paste(
          "Finishing position used more than once:",
          paste(duplicate_placings, collapse = ", ")
        )
      )
    }
    
    if (nrow(invalid_placings) > 0) {
      messages <- c(
        messages,
        "Every selected country must have a valid finishing position."
      )
    }
    
    if (length(messages) > 0) {
      
      return(
        div(
          style = paste0(
            "background-color: #FBE3E3;",
            "color: #A12626;",
            "padding: 12px;",
            "border-radius: 8px;"
          ),
          strong("Please correct the following:"),
          tags$ul(
            lapply(messages, tags$li)
          )
        )
      )
      
    }
    
    div(
      style = paste0(
        "background-color: #DDF2E3;",
        "color: #176B36;",
        "padding: 12px;",
        "border-radius: 8px;"
      ),
      strong("Madison results look valid."),
      p(paste(nrow(entered_results), "countries entered."))
    )
    
  })
  
  
  # STEP 17: Omnium scenario entry
  
  output$scenario_omnium_entry <- renderUI({
    
    req(input$scenario_competition)
    
    selected_competition <- scenario_competitions |>
      filter(competition_id == input$scenario_competition)
    
    req(nrow(selected_competition) > 0)
    
    is_continental <- selected_competition$competition_type[[1]] ==
      "Continental Championships"
    
    if (is_continental) {
      req(input$scenario_continent)
      number_of_rows <- 12
      
      eligible_countries <- countries_data |>
        filter(continent == input$scenario_continent)
      
    } else {
      number_of_rows <- 24
      eligible_countries <- countries_data
    }
    
    country_choices <- setNames(
      eligible_countries$country,
      paste0(
        eligible_countries$country,
        " - ",
        eligible_countries$name
      )
    )
    
    tagList(
      h4("Omnium results"),
      p(paste("Enter up to", number_of_rows, "finishing positions.")),
      
      lapply(seq_len(number_of_rows), function(i) {
        
        fluidRow(
          column(
            1,
            tags$p(i, style = "padding-top: 7px; font-weight: 600;")
          ),
          
          column(
            6,
            selectInput(
              inputId = paste0("scenario_omnium_country_", i),
              label = NULL,
              choices = c("Select country" = "", country_choices),
              selected = "",
              width = "100%"
            )
          ),
          
          column(
            3,
            numericInput(
              inputId = paste0("scenario_omnium_placing_", i),
              label = NULL,
              value = i,
              min = 1,
              max = number_of_rows,
              step = 1,
              width = "100%"
            )
          )
        )
      })
    )
  })
  
  # STEP 18: Omnium scenario validation
  
  output$scenario_omnium_validation <- renderUI({
    
    req(input$scenario_competition)
    
    selected_competition <- scenario_competitions |>
      filter(competition_id == input$scenario_competition)
    
    req(nrow(selected_competition) > 0)
    
    is_continental <- selected_competition$competition_type[[1]] ==
      "Continental Championships"
    
    if (is_continental) {
      req(input$scenario_continent)
      number_of_rows <- 12
    } else {
      number_of_rows <- 24
    }
    
    entered_results <- tibble(
      country = sapply(
        seq_len(number_of_rows),
        function(i) input[[paste0("scenario_omnium_country_", i)]]
      ),
      placing = sapply(
        seq_len(number_of_rows),
        function(i) input[[paste0("scenario_omnium_placing_", i)]]
      )
    ) |>
      filter(!is.na(country), country != "")
    
    duplicate_countries <- entered_results |>
      count(country) |>
      filter(n > 1)
    
    duplicate_placings <- entered_results |>
      count(placing) |>
      filter(n > 1)
    
    invalid_placings <- entered_results |>
      filter(
        is.na(placing) |
          placing < 1 |
          placing > number_of_rows |
          placing != floor(placing)
      )
    
    errors <- character()
    
    if (nrow(duplicate_countries) > 0) {
      errors <- c(errors, "A country has been entered more than once.")
    }
    
    if (nrow(duplicate_placings) > 0) {
      errors <- c(errors, "A finishing position has been entered more than once.")
    }
    
    if (nrow(invalid_placings) > 0) {
      errors <- c(errors, "One or more finishing positions are invalid.")
    }
    
    if (length(errors) > 0) {
      div(
        style = "color: #B02A37; font-weight: 600;",
        lapply(errors, p)
      )
    } else {
      div(
        style = "color: #198754; font-weight: 600;",
        "Omnium results look valid."
      )
    }
  })
  
  
  
  # ------------------------------------------
  # SCENARIO TRACKER: RUN SCENARIO
  # ------------------------------------------
  
  scenario_result_store <- reactiveVal(NULL)
  
  
  observeEvent(input$run_scenario, {
    
    # Clear previous results
    #scenario_result_store(NULL)
    
    tryCatch({
      
      req(input$scenario_gender)
      req(input$scenario_mode)
      
      # Determine which disciplines to include
      disciplines <- if (input$scenario_mode == "TP only") {
        "TP"
      } else {
        c("TP", "MAD", "OMN")
      }
      
      # Collect hypothetical results from the matrices
      scenario_results <- scenario_hypothetical_results() |>
        filter(
          gender == input$scenario_gender,
          discipline %in% disciplines
        ) |>
        select(
          country,
          discipline,
          competition_id,
          placing,
          gender
        )
      
      # Require at least one hypothetical result
      if (nrow(scenario_results) == 0) {
        stop("Please enter at least one hypothetical result before running the scenario.")
      }
      
      # Run the scenario calculations
      calculated_scenario <- calculate_scenario(
        scenario_results = scenario_results,
        scenario_gender = input$scenario_gender,
        scenario_mode = input$scenario_mode
      )
      
      
    #message("Calculation returned NULL: ", is.null(calculated_scenario))
      
      message(
        "Calculation result elements: ",
        paste(names(calculated_scenario), collapse = ", ")
      )
      
      # Store results for display and download
      scenario_result_store(calculated_scenario)
      
      message("Observer session: ", session$token)
      message("Stored result is NULL: ", is.null(scenario_result_store()))
      
      message(
        "Stored result is NULL: ",
        is.null(scenario_result_store())
      )
      
      showNotification(
        "Scenario calculated successfully!",
        type = "message"
      )
      
    }, error = function(e) {
      
      showNotification(
        conditionMessage(e),
        type = "error",
        duration = NULL
      )
      
    })
    
  })
  

  # ------------------------------------------
  # SCENARIO RESULTS SUMMARY
  # ------------------------------------------
  
  
  output$scenario_results_summary <- renderUI({
    
    req(scenario_result_store())
    
    if (input$scenario_mode == "TP only") {
      return(
        tagList(
          h4("Scenario Rankings"),
          h5("Team Pursuit"),
          DT::DTOutput("scenario_tp_rankings")
        )
      )
    }
    
    tagList(
      h4("Scenario Rankings"),
      
      h5("Team Pursuit"),
      DT::DTOutput("scenario_tp_rankings"),
      
      h5("Madison"),
      DT::DTOutput("scenario_madison_rankings"),
      
      h5("Omnium"),
      DT::DTOutput("scenario_omnium_rankings")
    )
    
  })
  
  
  
  output$scenario_tp_rankings <- renderDT({
    
    req(scenario_result_store())
    
    display_data <- scenario_result_store()$tp_allocation |>
      filter(gender == input$scenario_gender) |>
      select(
        rank,
        country,
        total_points,
        qualification_status,
        qualification_margin,
        qualification_route
      ) |>
      arrange(rank)
    
    table <- datatable(
      display_data,
      rownames = FALSE,
      colnames = c(
        "Rank",
        "Country",
        "Points",
        "Qualification status",
        "Points from cutoff",
        "Qualification pathway"
      ),
      options = list(
        pageLength = 25,
        scrollX = TRUE
      )
    )
    
    format_status(table)
    
  })
  
  output$scenario_madison_rankings <- renderDT({
    
    req(scenario_result_store())
    
    display_data <- scenario_result_store()$madison_allocation |>
      filter(gender == input$scenario_gender) |>
      select(
        rank,
        country,
        total_points,
        qualification_status,
        qualification_margin,
        qualification_route
      ) |>
      arrange(rank)
    
    table <- datatable(
      display_data,
      rownames = FALSE,
      colnames = c(
        "Rank",
        "Country",
        "Points",
        "Qualification status",
        "Points from cutoff",
        "Qualificatin pathway"
      ),
      options = list(
        pageLength = 25,
        scrollX = TRUE
      )
    )
    
    format_status(table)
    
  })
  
  output$scenario_omnium_rankings <- renderDT({
    
    req(scenario_result_store())
    
    display_data <- scenario_result_store()$omnium_allocation |>
      filter(gender == input$scenario_gender) |>
      select(
        rank,
        country,
        total_points,
        qualification_status,
        qualification_margin,
        qualification_route
      ) |>
      arrange(
        match(
          qualification_status,
          c("Qualified", "Reserved", "Not qualified")
        ),
        rank
      )
    
    table <- datatable(
      display_data,
      rownames = FALSE,
      colnames = c(
        "Rank",
        "Country",
        "Points",
        "Qualification status",
        "Points from cutoff",
        "Qualification pathway"
      ),
      options = list(
        pageLength = 25,
        scrollX = TRUE
      )
    )
    
    format_status(table)
    
  })
  
  
  # ------------------------------------------
  # DOWNLOAD SCENARIO RESULTS
  # ------------------------------------------
  
  output$download_scenario <- downloadHandler(
    
    filename = function() {
      paste0(
        "scenario_qualification_",
        input$scenario_gender,
        ".csv"
      )
    },
    
    content = function(file) {
      
      req(scenario_result_store())
      
      write_csv(
        scenario_result_store()$qualification,
        file
      )
      
    }
    
  )
  
  
  ### new scenario stuff
  
  # ------------------------------------------
  # SCENARIO MATRIX REACTIVE DATA
  # ------------------------------------------
  
  # Store manually added countries separately for each discipline
  scenario_added_countries <- reactiveValues(
    TP = list(Men = character(), Women = character()),
    MAD = list(Men = character(), Women = character()),
    OMN = list(Men = character(), Women = character())
  )
  
  updateSelectizeInput(
    session,
    "add_tp_country",
    choices = stats::setNames(
      countries_data$country,
      countries_data$name
    ),
    selected = ""
  )
  
  updateSelectizeInput(
    session,
    "add_madison_country",
    choices = stats::setNames(
      countries_data$country,
      countries_data$name
    ),
    selected = ""
  )
  
  updateSelectizeInput(
    session,
    "add_omnium_country",
    choices = stats::setNames(
      countries_data$country,
      countries_data$name
    ),
    selected = ""
  )
  
  # Store the three editable matrices
  scenario_tp_data <- reactiveVal(NULL)
  scenario_madison_data <- reactiveVal(NULL)
  scenario_omnium_data <- reactiveVal(NULL)
  
  scenario_tp_refresh <- reactiveVal(0)
  scenario_madison_refresh <- reactiveVal(0)
  scenario_omnium_refresh <- reactiveVal(0)
  
  scenario_hypothetical_results <- reactiveVal(
    tibble(
      country = character(),
      discipline = character(),
      competition_id = character(),
      column_key = character(),
      placing = integer(),
      gender = character()
    )
  )
  
  
  restore_scenario_hypotheticals <- function(
    matrix_data,
    selected_discipline,
    selected_gender
  ) {
    
    saved_results <- scenario_hypothetical_results() |>
      filter(
        discipline == selected_discipline,
        gender == selected_gender
      )
    
    if (nrow(saved_results) == 0) {
      return(matrix_data)
    }
    
    visible_competitions <- scenario_competition_columns
    
    if (input$hide_completed) {
      visible_competitions <- visible_competitions |>
        filter(status != "Completed")
    }
    
    for (i in seq_len(nrow(visible_competitions))) {
      
      competition <- visible_competitions[i, ]
      column_name <- competition$column_label
      
      if (competition$status == "Completed") next
      
      matching_results <- saved_results |>
        filter(column_key == competition$column_key)
      
      if (nrow(matching_results) == 0) next
      
      row_numbers <- match(
        matching_results$country,
        matrix_data$Country
      )
      
      valid_rows <- !is.na(row_numbers)
      
      matrix_data[[column_name]][row_numbers[valid_rows]] <-
        matching_results$placing[valid_rows]
    }
    
    matrix_data
  }
  
  
  # Build or rebuild all matrices for the selected gender
  observeEvent(input$scenario_gender, {
    
    current_gender <- input$scenario_gender
    
    scenario_tp_data(
      restore_scenario_hypotheticals(
        build_scenario_matrix(
          "TP",
          current_gender,
          scenario_added_countries$TP[[current_gender]],
          input$hide_completed
        ),
        "TP",
        current_gender
      )
    )
    
    scenario_madison_data(
      restore_scenario_hypotheticals(
        build_scenario_matrix(
          "MAD",
          current_gender,
          scenario_added_countries$MAD[[current_gender]],
          input$hide_completed
        ),
        "MAD",
        current_gender
      )
    )
    
    scenario_omnium_data(
      restore_scenario_hypotheticals(
        build_scenario_matrix(
          selected_discipline = "OMN",
          selected_gender = current_gender,
          added_countries = scenario_added_countries$OMN[[current_gender]],
          hide_completed = input$hide_completed
        ),
        "OMN",
        current_gender
      )
    )
    
  }, ignoreInit = FALSE)
  
  
  # Rebuild matrices when completed competitions are hidden or shown
  observeEvent(input$hide_completed, {
    
    current_gender <- input$scenario_gender
    
    scenario_tp_data(
      build_scenario_matrix(
        "TP",
        current_gender,
        scenario_added_countries$TP[[current_gender]],
        input$hide_completed
      )
    )
    
    scenario_madison_data(
      build_scenario_matrix(
        "MAD",
        current_gender,
        scenario_added_countries$MAD[[current_gender]],
        input$hide_completed
      )
    )
    
    scenario_omnium_data(
      build_scenario_matrix(
        "OMN",
        current_gender,
        scenario_added_countries$OMN[[current_gender]],
        input$hide_completed
      )
    )
    
  })
  
  
  observeEvent(input$add_tp_country_button, {
    
    req(input$add_tp_country)
    
    current_gender <- input$scenario_gender
    country_to_add <- input$add_tp_country
    
    current_data <- scenario_tp_data()
    req(current_data)
    
    # Prevent duplicate countries
    if (country_to_add %in% current_data$Country) {
      showNotification(
        "This country is already in the Team Pursuit matrix.",
        type = "warning"
      )
      return()
    }
    
    # Add country to the selected gender's country list
    scenario_added_countries$TP[[current_gender]] <-
      unique(c(
        scenario_added_countries$TP[[current_gender]],
        country_to_add
      ))
    
    # Rebuild the matrix
    updated_matrix <- build_scenario_matrix(
      "TP",
      current_gender,
      scenario_added_countries$TP[[current_gender]],
      input$hide_completed
    )
    
    # Restore previously entered hypothetical results
    updated_matrix <- restore_scenario_hypotheticals(
      updated_matrix,
      "TP",
      current_gender
    )
    
    scenario_tp_data(updated_matrix)
    
    # Clear the search box
    updateSelectizeInput(
      session,
      "add_tp_country",
      selected = ""
    )
    
   
    
    showNotification(
      paste(country_to_add, "added to Team Pursuit."),
      type = "message"
    )
    
  })
  
  
  
  observeEvent(input$add_madison_country_button, {
    
    req(input$add_madison_country)
    
    current_gender <- input$scenario_gender
    country_to_add <- input$add_madison_country
    
    current_data <- scenario_madison_data()
    req(current_data)
    
    # Prevent duplicate countries
    if (country_to_add %in% current_data$Country) {
      showNotification(
        "This country is already in the Madison matrix.",
        type = "warning"
      )
      return()
    }
    
    # Remember the country for this gender
    scenario_added_countries$MAD[[current_gender]] <-
      unique(c(
        scenario_added_countries$MAD[[current_gender]],
        country_to_add
      ))
    
    # Rebuild the matrix
    updated_matrix <- build_scenario_matrix(
      "MAD",
      current_gender,
      scenario_added_countries$MAD[[current_gender]],
      input$hide_completed
    )
    
    # Restore previously entered hypothetical results
    updated_matrix <- restore_scenario_hypotheticals(
      updated_matrix,
      "MAD",
      current_gender
    )
    
    scenario_madison_data(updated_matrix)
    
    # Clear the search box
    updateSelectizeInput(
      session,
      "add_madison_country",
      selected = ""
    )
    
    showNotification(
      paste(country_to_add, "added to Madison."),
      type = "message"
    )
    
  })
  
  
  observeEvent(input$add_omnium_country_button, {
    req(input$add_omnium_country)
    
    current_gender <- input$scenario_gender
    country_to_add <- input$add_omnium_country
    current_data <- scenario_omnium_data()
    
    req(current_data)
    
    if (country_to_add %in% current_data$Country) {
      showNotification(
        "This country is already in the Omnium matrix.",
        type = "warning"
      )
      return()
    }
    
    scenario_added_countries$OMN[[current_gender]] <- unique(c(
      scenario_added_countries$OMN[[current_gender]],
      country_to_add
    ))
    
    updated_matrix <- build_scenario_matrix(
      "OMN",
      current_gender,
      scenario_added_countries$OMN[[current_gender]],
      input$hide_completed
    )
    
    updated_matrix <- restore_scenario_hypotheticals(
      updated_matrix,
      "OMN",
      current_gender
    )
    
    scenario_omnium_data(updated_matrix)
    
    updateSelectizeInput(
      session,
      "add_omnium_country",
      selected = ""
    )
    
    showNotification(
      paste(country_to_add, "added to Omnium."),
      type = "message"
    )
  })
  
  
  
  
  # ------------------------------------------
  # SCENARIO MATRIX TABLE OUTPUTS
  # ------------------------------------------
  
  
  
  
  
  
  
  output$scenario_tp_matrix <- rhandsontable::renderRHandsontable({
    
    req(scenario_tp_data())
    
    scenario_tp_refresh()
    
    visible_columns <- scenario_competition_columns
    
    if (input$hide_completed) {
      visible_columns <- visible_columns |>
        filter(status != "Completed")
    }
    
    hot <- rhandsontable::rhandsontable(
      scenario_tp_data(),
      rowHeaders = NULL,
      colHeaders = names(scenario_tp_data()),
      stretchH = "all",
      height = 600
    )
    
    # Country names cannot be edited
    hot <- rhandsontable::hot_col(
      hot,
      "Country",
      readOnly = TRUE
    )
    
    # Configure each competition column
    for (i in seq_len(nrow(visible_columns))) {
      
      column_name <- visible_columns$column_label[i]
      
      hot <- rhandsontable::hot_col(
        hot,
        column_name,
        type = "numeric",
        format = "0",
        readOnly = visible_columns$status[i] == "Completed"
      )
    }
    
    rhandsontable::hot_table(
      hot,
      highlightCol = TRUE,
      highlightRow = TRUE
    )
    
  })
  
  
  
  
  # ------------------------------------------
  # TEAM PURSUIT MATRIX EDITING
  # ------------------------------------------
  
  
  observeEvent(input$scenario_tp_matrix, {
    
    edited_data <- rhandsontable::hot_to_r(
      input$scenario_tp_matrix
    )
    
    current_data <- scenario_tp_data()
    req(current_data)
    
    # Identify the visible competition columns
    competition_columns <- setdiff(
      names(edited_data),
      "Country"
    )
    
    invalid_placing <- FALSE
    
    # Validate whole numbers between 1 and 100
    for (column in competition_columns) {
      
      raw_values <- edited_data[[column]]
      
      blank <- is.na(raw_values) |
        trimws(as.character(raw_values)) == ""
      
      numeric_values <- suppressWarnings(
        as.numeric(as.character(raw_values))
      )
      
      valid <- blank |
        (
          !is.na(numeric_values) &
            is.finite(numeric_values) &
            numeric_values >= 1 &
            numeric_values <= 100 &
            numeric_values == floor(numeric_values)
        )
      
      if (!all(valid)) {
        invalid_placing <- TRUE
        break
      }
      
      numeric_values[blank] <- NA
      edited_data[[column]] <- as.integer(numeric_values)
    }
    
    if (invalid_placing) {
      showNotification(
        "Please enter a whole number between 1 and 100, or leave the cell blank.",
        type = "warning"
      )
      scenario_tp_data(current_data)
      scenario_tp_refresh(scenario_tp_refresh() + 1)
      return()
    }
    
    # Validate continental eligibility
    country_continents <- countries_data$continent[
      match(edited_data$Country, countries_data$country)
    ]
    
    for (i in seq_along(competition_columns)) {
      
      column <- competition_columns[i]
      
      competition_info <- visible_columns <- scenario_competition_columns
      
      if (input$hide_completed) {
        competition_info <- competition_info |>
          filter(status != "Completed")
      }
      
      selected_competition <- competition_info |>
        filter(column_label == column)
      
      if (nrow(selected_competition) == 1 &&
          !is.na(selected_competition$column_continent)) {
        
        has_result <- !is.na(edited_data[[column]])
        
        wrong_continent <- has_result &
          country_continents != selected_competition$column_continent
        
        wrong_continent[is.na(wrong_continent)] <- FALSE
        
        if (any(wrong_continent)) {
          showNotification(
            "A country can only receive a result in its own Continental Championships.",
            type = "warning"
          )
          scenario_tp_data(current_data)
          scenario_tp_refresh(scenario_tp_refresh() + 1)
          return()
        }
      }
    }
    
    # Save valid hypothetical results separately
    current_gender <- input$scenario_gender
    
    visible_competitions <- scenario_competition_columns
    
    if (input$hide_completed) {
      visible_competitions <- visible_competitions |>
        filter(status != "Completed")
    }
    
    editable_competitions <- visible_competitions |>
      filter(status != "Completed")
    
    # Extract hypothetical results from the spreadsheet
    new_hypothetical <- edited_data |>
      select(Country, all_of(editable_competitions$column_label)) |>
      pivot_longer(
        cols = -Country,
        names_to = "column_label",
        values_to = "placing"
      ) |>
      left_join(
        editable_competitions |>
          select(column_label, competition_id, column_key),
        by = "column_label"
      ) |>
      filter(!is.na(placing)) |>
      transmute(
        country = Country,
        discipline = "TP",
        competition_id,
        column_key,
        placing = as.integer(placing),
        gender = current_gender
      )
    
    # Preserve other genders, disciplines and hidden competitions
    existing_hypothetical <- scenario_hypothetical_results() |>
      filter(
        !(discipline == "TP" &
            gender == current_gender &
            competition_id %in% editable_competitions$competition_id)
      )
    
    scenario_hypothetical_results(
      bind_rows(existing_hypothetical, new_hypothetical)
    )
    
    # Update the visible spreadsheet
    scenario_tp_data(edited_data)
    
  })
  
  
  
  
  
  
  
  output$scenario_madison_matrix <- rhandsontable::renderRHandsontable({
    
    req(scenario_madison_data())
    
    scenario_madison_refresh()
    
    visible_columns <- scenario_competition_columns
    
    if (input$hide_completed) {
      visible_columns <- visible_columns |>
        filter(status != "Completed")
    }
    
    hot <- rhandsontable::rhandsontable(
      scenario_madison_data(),
      rowHeaders = NULL,
      colHeaders = names(scenario_madison_data()),
      stretchH = "all",
      height = 600
    )
    
    hot <- rhandsontable::hot_col(
      hot,
      "Country",
      readOnly = TRUE
    )
    
    for (i in seq_len(nrow(visible_columns))) {
      
      column_name <- visible_columns$column_label[i]
      
      hot <- rhandsontable::hot_col(
        hot,
        column_name,
        type = "numeric",
        format = "0",
        readOnly = visible_columns$status[i] == "Completed"
      )
    }
    
    rhandsontable::hot_table(
      hot,
      highlightCol = TRUE,
      highlightRow = TRUE
    )
    
  })
  
  
  observeEvent(input$scenario_madison_matrix, {
    
    edited_data <- rhandsontable::hot_to_r(
      input$scenario_madison_matrix
    )
    
    current_data <- scenario_madison_data()
    req(current_data)
    
    visible_competitions <- scenario_competition_columns
    
    if (input$hide_completed) {
      visible_competitions <- visible_competitions |>
        filter(status != "Completed")
    }
    
    editable_competitions <- visible_competitions |>
      filter(status != "Completed")
    
    competition_columns <- editable_competitions$column_label
    
    # Validate placings
    invalid_placing <- FALSE
    
    for (column in competition_columns) {
      
      raw_values <- edited_data[[column]]
      
      blank <- is.na(raw_values) |
        trimws(as.character(raw_values)) == ""
      
      numeric_values <- suppressWarnings(
        as.numeric(as.character(raw_values))
      )
      
      valid <- blank |
        (
          !is.na(numeric_values) &
            is.finite(numeric_values) &
            numeric_values >= 1 &
            numeric_values <= 100 &
            numeric_values == floor(numeric_values)
        )
      
      if (!all(valid)) {
        invalid_placing <- TRUE
        break
      }
      
      numeric_values[blank] <- NA
      edited_data[[column]] <- as.integer(numeric_values)
    }
    
    if (invalid_placing) {
      showNotification(
        "Please enter a whole number between 1 and 100, or leave the cell blank.",
        type = "warning"
      )
      scenario_madison_data(current_data)
      scenario_madison_refresh(scenario_madison_refresh() + 1)
      return()
    }
    
    # Validate continental eligibility
    country_continents <- countries_data$continent[
      match(edited_data$Country, countries_data$country)
    ]
    
    for (column in competition_columns) {
      
      selected_competition <- editable_competitions |>
        filter(column_label == column)
      
      if (nrow(selected_competition) == 1 &&
          !is.na(selected_competition$column_continent)) {
        
        has_result <- !is.na(edited_data[[column]])
        
        wrong_continent <- has_result &
          country_continents != selected_competition$column_continent
        
        wrong_continent[is.na(wrong_continent)] <- FALSE
        
        if (any(wrong_continent)) {
          showNotification(
            "A country can only receive a result in its own Continental Championships.",
            type = "warning"
          )
          scenario_madison_data(current_data)
          scenario_madison_refresh(scenario_madison_refresh() + 1)
          return()
        }
      }
    }
    
    # Build hypothetical results
    new_hypothetical <- edited_data |>
      select(Country, all_of(competition_columns)) |>
      pivot_longer(
        cols = -Country,
        names_to = "column_label",
        values_to = "placing"
      ) |>
      left_join(
        editable_competitions |>
          select(column_label, competition_id, column_key),
        by = "column_label"
      ) |>
      filter(!is.na(placing)) |>
      transmute(
        country = Country,
        discipline = "MAD",
        competition_id,
        column_key,
        placing = as.integer(placing),
        gender = input$scenario_gender
      )
    
    # Preserve other disciplines, genders and hidden competitions
    existing_hypothetical <- scenario_hypothetical_results() |>
      filter(
        !(discipline == "MAD" &
            gender == input$scenario_gender &
            column_key %in% editable_competitions$column_key)
      )
    
    scenario_hypothetical_results(
      bind_rows(existing_hypothetical, new_hypothetical)
    )
    
    scenario_madison_data(edited_data)
    
  })
  
  
  observeEvent(input$scenario_omnium_matrix, {
    edited_data <- rhandsontable::hot_to_r(input$scenario_omnium_matrix)
    current_data <- scenario_omnium_data()
    req(current_data)
    
    current_gender <- input$scenario_gender
    
    visible_competitions <- scenario_competition_columns
    
    if (input$hide_completed) {
      visible_competitions <- visible_competitions |>
        filter(status != "Completed")
    }
    
    editable_competitions <- visible_competitions |>
      filter(status != "Completed")
    
    # Validate placing values
    edited_values <- edited_data |>
      select(all_of(editable_competitions$column_label)) |>
      unlist(use.names = FALSE)
    
    invalid_values <- !is.na(edited_values) &
      (
        !is.finite(edited_values) |
          edited_values < 1 |
          edited_values > 100 |
          edited_values != floor(edited_values)
      )
    
    if (any(invalid_values)) {
      showNotification(
        "Please enter whole-number placings between 1 and 100.",
        type = "error"
      )
      scenario_omnium_data(current_data)
      scenario_omnium_refresh(scenario_omnium_refresh() + 1)
      return()
    }
    
    # Validate continental eligibility
    for (i in seq_len(nrow(editable_competitions))) {
      competition <- editable_competitions[i, ]
      
      if (is.na(competition$column_continent)) next
      
      invalid_countries <- edited_data |>
        filter(!is.na(.data[[competition$column_label]])) |>
        left_join(
          countries_data |>
            select(country, continent),
          by = c("Country" = "country")
        ) |>
        filter(is.na(continent) | continent != competition$column_continent)
      
      if (nrow(invalid_countries) > 0) {
        showNotification(
          paste(
            "Only countries from",
            competition$column_continent,
            "can have results in this column."
          ),
          type = "error"
        )
        scenario_omnium_data(current_data)
        scenario_omnium_refresh(scenario_omnium_refresh() + 1)
        return()
      }
    }
    
    # Store hypothetical results
    new_hypothetical <- edited_data |>
      select(Country, all_of(editable_competitions$column_label)) |>
      pivot_longer(
        cols = -Country,
        names_to = "column_label",
        values_to = "placing"
      ) |>
      left_join(
        editable_competitions |>
          select(column_label, competition_id, column_key),
        by = "column_label"
      ) |>
      filter(!is.na(placing)) |>
      transmute(
        country = Country,
        discipline = "OMN",
        competition_id,
        column_key,
        placing = as.integer(placing),
        gender = current_gender
      )
    
    existing_hypothetical <- scenario_hypothetical_results() |>
      filter(
        !(
          discipline == "OMN" &
            gender == current_gender &
            column_key %in% editable_competitions$column_key
        )
      )
    
    scenario_hypothetical_results(
      bind_rows(existing_hypothetical, new_hypothetical)
    )
    
    scenario_omnium_data(edited_data)
  })
  
  
  
  
  output$scenario_omnium_matrix <- rhandsontable::renderRHandsontable({
    req(scenario_omnium_data())
    scenario_omnium_refresh()
    
    visible_columns <- scenario_competition_columns
    
    if (input$hide_completed) {
      visible_columns <- visible_columns |>
        filter(status != "Completed")
    }
    
    hot <- rhandsontable::rhandsontable(
      scenario_omnium_data(),
      rowHeaders = NULL,
      colHeaders = names(scenario_omnium_data()),
      stretchH = "all",
      height = 600
    )
    
    hot <- rhandsontable::hot_col(
      hot,
      "Country",
      readOnly = TRUE
    )
    
    for (i in seq_len(nrow(visible_columns))) {
      column_name <- visible_columns$column_label[i]
      
      hot <- rhandsontable::hot_col(
        hot,
        column_name,
        type = "numeric",
        format = "0",
        readOnly = visible_columns$status[i] == "Completed"
      )
    }
    
    rhandsontable::hot_table(
      hot,
      highlightCol = TRUE,
      highlightRow = TRUE
    )
  })
  
  
  
  # Points schedule headings
  output$points_result <- renderUI({
    
    fluidRow(
      column(
        width = 4,
        h5("World Championships"),
        tableOutput("points_world_champs")
      ),
      column(
        width = 4,
        h5("World Cup"),
        tableOutput("points_world_cup")
      ),
      column(
        width = 4,
        h5("Continental Championships"),
        tableOutput("points_continental")
      )
    )
    
  })
  
  
  # World Championships points
  output$points_world_champs <- renderTable({
    
    points_table |>
      filter(
        competition_type == "World Championships",
        discipline == input$points_discipline
      ) |>
      transmute(
        Placing = placing,
        `Points awarded` = points
      )
    
  }, striped = TRUE, bordered = TRUE, spacing = "xs", digits = 0)
  
  
  # World Cup points
  output$points_world_cup <- renderTable({
    
    points_table |>
      filter(
        competition_type == "World Cup",
        discipline == input$points_discipline
      ) |>
      transmute(
        Placing = placing,
        `Points awarded` = points
      )
    
  }, striped = TRUE, bordered = TRUE, spacing = "xs", digits = 0)
  
  
  # Continental Championships points
  output$points_continental <- renderTable({
    
    points_table |>
      filter(
        competition_type == "Continental Championships",
        discipline == input$points_discipline
      ) |>
      mutate(
        points_awarded = points * 0.75
      ) |>
      transmute(
        Placing = placing,
        `Points awarded` = points_awarded
      )
    
  }, striped = TRUE, bordered = TRUE, spacing = "xs", digits = 0)
  
  
  
  
}
