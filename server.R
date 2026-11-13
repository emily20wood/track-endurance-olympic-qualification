
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
  
}
