
# ==========================================
# OLYMPIC CYCLING QUALIFICATION TRACKER
# USER INTERFACE
# ==========================================

library(shiny)
library(bslib)
library(DT)

# ------------------------------------------
# THEME
# ------------------------------------------

app_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#142B4A",
  secondary = "#6C757D",
  success = "#198754",
  warning = "#D99A24",
  danger = "#B02A37",
  base_font = font_google("Roboto"),
  heading_font = font_google("Roboto")
)

# ------------------------------------------
# CUSTOM CSS
# ------------------------------------------

app_css <- tags$style(HTML("

  body {
    background-color: #F4F6F9;
  }

  .app-header {
    background-color: #FFFFFF;
    padding: 20px 28px;
    margin-bottom: 20px;
    border-bottom: 4px solid #C8202F;
    display: flex;
    align-items: center;
    gap: 25px;
  }

  .app-logo {
    max-height: 75px;
    max-width: 150px;
    object-fit: contain;
  }

  .app-title {
    color: #142B4A;
    font-weight: 700;
    margin-bottom: 3px;
  }

  .app-subtitle {
    color: #6C757D;
    margin-bottom: 0;
  }

  .section-heading {
    color: #142B4A;
    font-weight: 700;
    margin-bottom: 15px;
  }

  .metric-card {
    background: #FFFFFF;
    border-radius: 10px;
    padding: 20px;
    margin-bottom: 15px;
    box-shadow: 0 2px 7px rgba(0,0,0,0.05);
    border-top: 3px solid #142B4A;
  }

  .metric-label {
    color: #6C757D;
    font-size: 14px;
  }

  .metric-number {
    color: #142B4A;
    font-size: 30px;
    font-weight: 700;
  }

  .qualification-note {
    background: #FFFFFF;
    border-left: 4px solid #D99A24;
    padding: 15px;
    border-radius: 5px;
    margin-bottom: 20px;
  }

  .tab-content {
    padding-top: 20px;
  }

  .form-control, .selectize-input {
    border-radius: 6px;
  }

  .dataTables_wrapper {
    background: #FFFFFF;
    padding: 15px;
    border-radius: 8px;
  }
  
    /* Qualification status colours */

  .status-qualified {
    background-color: #DDF2E3 !important;
    color: #216B3A !important;
    font-weight: 600;
  }

  .status-reserved {
    background-color: #FFF0D2 !important;
    color: #946200 !important;
    font-weight: 600;
  }

  .status-not-qualified {
    background-color: #FBE3E3 !important;
    color: #A52A2A !important;
    font-weight: 600;
  }
  
  

/* Scenario matrix gridlines */
table.scenario-matrix {
  border-collapse: collapse !important;
  border: 1px solid #aeb7c2 !important;
}

table.scenario-matrix thead th,
table.scenario-matrix tbody td {
  border: 1px solid #aeb7c2 !important;
}

/* Highlight hovered row */
table.scenario-matrix tbody tr:hover td {
  background-color: #eaf2fa !important;
}

/* Highlight hovered cell */
table.scenario-matrix tbody td:hover {
  background-color: #cfe2f5 !important;
  outline: 2px solid #668db5;
  outline-offset: -2px;
}

/* Country column */
table.scenario-matrix th:first-child,
table.scenario-matrix td:first-child {
  font-weight: 600;
  background-color: #f0f2f4 !important;
}



"))

# ------------------------------------------
# USER INTERFACE
# ------------------------------------------

ui <- fluidPage(

  
  theme = app_theme,
  
  app_css,
  
  # ----------------------------------------
  # HEADER
  # ----------------------------------------
  
  div(
    class = "app-header",
    
    # img(
    #   src = "USA_Cycling_logo.png",
    #   class = "app-logo",
    #   alt = "USA Cycling logo"
    # ),
    tags$img(
      src = "USA_Cycling_logo.png",
      height = "100px",
      style = "display: block; object-fit: contain;"
    ),
    
    div(
      h2(
        "Track Endurance Olympic Qualification Tracker",
        class = "app-title"
      ),
      
      p(
        "Los Angeles 2028 | Qualification monitoring",
        class = "app-subtitle"
      )
    )
  ),
  
  # ----------------------------------------
  # GLOBAL GENDER and COUNTRY SEARCH
  # ----------------------------------------
  
  fluidRow(
    column(
      width = 4,
      
      selectInput(
        inputId = "gender_filter",
        label = "Gender",
        choices = c("Men", "Women"),
        selected = "Men",
        width = "150px"
      ),
      
      selectInput(
        inputId = "country_filter",
        label = "Find a country",
        choices = c("All countries"),
        selected = "All countries"
      )
    ),
    
    column(
      width = 8,
      
      div(
        style = "padding-top: 30px; text-align: right;",
        
        uiOutput("data_status")
      )
    )
  ),
  
  # ----------------------------------------
  # NAVIGATION
  # ----------------------------------------
  
  tabsetPanel(
    
    id = "main_navigation",
    
    type = "tabs",
    
    # ======================================
    # TAB 1: OVERVIEW
    # ======================================
    
    tabPanel(
      title = "USA Overview",
      
      br(),
      
      h3(
        "USA qualification overview",
        class = "section-heading"
      ),
      
      p(
        "Current Olympic qualification positions across all three disciplines."
      ),
      
      uiOutput("overview_metrics"),
      br(),
      uiOutput("overview_table"),
      
      br(),
      
      div(
        class = "qualification-note",
        
        strong("Important: "),
        
        "Rankings are provisional and will update as additional eligible competitions are entered."
      )
    ),
    
    
    tabPanel(
      title = "How to Use",
      
      br(),
      
      h3(
        "How to use the Qualification Tracker",
        class = "section-heading"
      ),
      
      p(
        "This guide explains the Olympic qualification rankings,
    points allocation, qualification pathways and how to use
    the Scenario Tracker."
      ),
      
      div(
        class = "qualification-note",
        
        strong("Official qualification reference"),
        
        p(
          "This tracker is based on the official LA28 Cycling Track
    Qualification System published by the International Olympic
    Committee (IOC). The qualification rules, points allocation
    and qualification pathways presented in this tracker are
    derived from this document."
        ),
        
        tags$a(
          href = "https://stillmed.olympics.com/media/Documents/Olympic-Games/LA28/CTR-LA28-Qualification-System.pdf",
          target = "_blank",
          "View the official LA28 Cycling Track Qualification System (PDF)"
        )
      ),
      
      hr(),
      
      # ----------------------------------------
      # 1. NAVIGATING THE TRACKER
      # ----------------------------------------
      
      h4("1. Navigating the tracker", class = "section-heading"),
      
      p(
        "Use the navigation tabs to explore the different
    qualification disciplines and competition results."
      ),
      
      tags$ul(
        tags$li(
          strong("USA overview: "),
          "A summary of current qualification positions across
      the Team Pursuit, Madison and Omnium for USA."
        ),
        tags$li(
          strong("Team Pursuit: "),
          "View current rankings and qualification places."
        ),
        tags$li(
          strong("Madison: "),
          "View current rankings and qualification places."
        ),
        tags$li(
          strong("Omnium: "),
          "View current rankings and qualification places."
        ),
        tags$li(
          strong("Country Profile: "),
          "Explore the qualification position of an individual country."
        ),
        tags$li(
          strong("Competition Results: "),
          "Review competition results and points awarded."
        ),
        tags$li(
          strong("Scenario Tracker: "),
          "Explore hypothetical results and their potential
      impact on Olympic qualification."
        )
      ),
      
      hr(),
      
      # ----------------------------------------
      # 2. UNDERSTANDING THE RANKINGS
      # ----------------------------------------
      
      h4("2. Understanding the qualification pathway", class = "section-heading"),
      
      p(
        "Countries are ranked according to the qualification
    points accumulated during the qualification period.
    Rankings update as additional competition results
    become available."
      ),
      
      h5("Competitions"),
      
      p(
        "Points are acquired in the following competitions:"
      ),
      
      tags$ul(
        tags$li(
            "The 2027 and 2028 editions of the respective continental championships."
          ),
        tags$li(
            "The best two results in the UCI Track World Cup in each of the seasons 2027 and 2028."
        ),
        tags$li(
            "The 2026 and 2027 UCI Track World Championships (elite categories)."
          )
        ),
      h5("Points scales"),
      
      p(
        "Points scales are applied as detailed in articles 3.3.010 of the UCI regulations, adapted as follows:"
      ),
      
      tags$ul(
        tags$li(
          
            "Continental championships points multipled by 0.75."
          ),
        tags$li(
          
            "UCI Track World Cup points as is."
          ),
        tags$li(
            "UCI Track World Championships points as is."
          )
        ),
      
      h5("Points allocation by competition and discipline"),
      
      p(
        "The tables below shows the points allocation for World Championships, World Cups and Continental Championships (with the *0.75 multiplier.) You can select Team Pursuit, Madison or Omnium to see the differences in points between disciplines."
      ),
      
      selectInput(
        "points_discipline",
        "Select discipline",
        choices = c(
          "Team Pursuit" = "TP",
          "Madison" = "MAD",
          "Omnium" = "OMN"
        ),
        selected = "TP"
      ),
      
      uiOutput("points_result"),
      
      
      hr(),
      
      h5("Quota places"),
      
      p(
        "This is how the rankings work for each discipline:"
      ),
      
      h5("Team Pursuit"),
      
      tags$ul(
        tags$li(
            "The eight highest-ranked NOCs per gender in the UCI Track Olympic Qualification Ranking – Team Pursuit will each have a team of four athletes qualify."
          )
        ),
      
      h5("Madison"),
      
      tags$ul(
        tags$li(
            "The six best-ranked NOCs in the UCI Track Olympic Qualification Ranking – Madison that have not obtained any quota places in the team pursuit will each have a team of two athletes qualify."
          ),
        tags$li(
            "In addition, NOCs that have qualified in the team pursuit have the right to enter one team of two athletes in the madison."
          ),
        tags$li(
            "The total number of teams taking part in the madison could therefore reach a maximum of 14 per gender."
          )
        ),
      
      h5("Omnium"),
      
      tags$ul(
        tags$li(
            "The 13 highest-ranked NOCs in the UCI Track Olympic Qualification Ranking – Omnium – adhering to the minimum allocation per continent as specified under point 1 of the “Special Provisions” – that have not directly obtained any quota places in the madison will each obtain one quota place."
          ),
        tags$li(
            "In addition, the six NOCs qualified in the madison have the right to enter one athlete in the omnium."
          ),
        tags$li(
            "The total number of athletes taking part in the omnium could therefore reach a maximum of 19 per gender."
          )
        ),
      
      
      hr(),
      
      # ----------------------------------------
      # 5. SCENARIO TRACKER
      # ----------------------------------------
      
      h4("3. Using the Scenario Tracker",
         class = "section-heading"),
      
      p(
        "The Scenario Tracker allows you to explore how
    hypothetical competition results could change
    Olympic qualification."
      ),
      
      tags$ol(
        tags$li(
          strong("Choose a gender: "),
          "Select Men or Women."
        ),
        tags$li(
          strong("Choose a scenario mode: "),
          "Select TP only to simulate Team Pursuit results,
      or Full scenario to simulate Team Pursuit, Madison
      and Omnium results."
        ),
        tags$li(
          strong("Enter hypothetical results: "),
          "Edit the competition matrices by entering finishing
      positions for each country."
        ),
        tags$li(
          strong("Add countries: "),
          "Use the country selectors to include additional
      countries in the scenario."
        ),
        tags$li(
          strong("Run the scenario: "),
          "Select Run Scenario to recalculate rankings and
      qualification positions."
        ),
        tags$li(
          strong("Review the results: "),
          "Explore the updated rankings and qualification
      pathways."
        ),
        tags$li(
          strong("Download results: "),
          "Export the scenario qualification results as a CSV file."
        )
      ),
      
      p(
        strong("Completed competitions: "),
        "Competitions with recorded results are locked and
    cannot be edited. You can hide completed competitions
    using the checkbox."
      ),
      
      hr(),
      
      # ----------------------------------------
      # 6. IMPORTANT INFORMATION
      # ----------------------------------------
      
      h4("4. Important information",
         class = "section-heading"),
      
      p(
        "Rankings may be provisional until all expected
    competitions in the qualification period have been
    completed and entered."
      ),
      
      p(
        "Scenario results are hypothetical and are intended
    for analysis and planning."
      ),
      
      br()
    ),
    
    
    # ======================================
    # TAB 2: TEAM PURSUIT
    # ======================================
    
    tabPanel(
      title = "Team Pursuit",
      
      br(),
      
      h3(
        "Team Pursuit rankings",
        class = "section-heading"
      ),
      
      p(
        "The eight highest-ranked countries qualify for the Olympic Team Pursuit."
      ),
      
      uiOutput("tp_summary"),
      
      DTOutput("tp_table")
    ),
    
    # ======================================
    # TAB 3: MADISON
    # ======================================
    
    tabPanel(
      title = "Madison",
      
      br(),
      
      h3(
        "Madison qualification",
        class = "section-heading"
      ),
      
      p(
        "Eight countries qualify automatically through Team Pursuit, with six additional countries qualifying through the Madison ranking."
      ),
      
      uiOutput("madison_summary"),
      
      DTOutput("madison_table")
    ),
    
    # ======================================
    # TAB 4: OMNIUM
    # ======================================
    
    tabPanel(
      title = "Omnium",
      
      br(),
      
      h3(
        "Omnium qualification",
        class = "section-heading"
      ),
      
      p(
        "Six countries qualify automatically through Madison. Thirteen additional places are allocated through the overall Omnium ranking, with priority given to countries needed to ensure continental representation."
      ),
      
      uiOutput("omnium_summary"),
      
      br(),
      
      h4(
        "Continental representation",
        class = "section-heading"
      ),
      
      DTOutput("continent_table"),
      
      br(),
      
      h4(
        "Omnium country rankings",
        class = "section-heading"
      ),
      
      DTOutput("omnium_table")
    ),
    
    #country profile
    
    tabPanel(
      "Country Profile",
      br(),
      h3("Country Qualification Profile"),
      p("Select a country using the country selector above to view its current qualification position."),
      uiOutput("country_profile")
    ),
    
    # ======================================
    # TAB 5: COMPETITION RESULTS
    # ======================================
    
    tabPanel(
      title = "Competition Results",
      
      br(),
      
      h3(
        "Competition results and points",
        class = "section-heading"
      ),
      
      p(
        "Explore competition placings, points awarded and results contributing to the qualification rankings."
      ),
      
      fluidRow(
        
        column(
          width = 4,
          
          selectInput(
            "results_discipline",
            "Discipline",
            choices = c(
              "All disciplines",
              "TP",
              "MAD",
              "OMN"
            )
          )
        ),
        
        column(
          width = 4,
          
          selectInput(
            "results_competition",
            "Competition",
            choices = "All competitions"
          )
        )
        
      ),
      
      DTOutput("results_table")
    ),
    
    
    
    # ======================================
    # TAB 7: SCENARIO TRACKER
    # ======================================
    
    
    tabPanel(
      title = "Scenario Tracker",
      
      br(),
      
      h3(
        "Qualification Scenario Simulator",
        class = "section-heading"
      ),
      
      p(
        "Explore how hypothetical competition results could change Olympic qualification."
      ),
      
      wellPanel(
        
        h4("Scenario settings"),
        
        fluidRow(
          
          column(
            width = 4,
            
            selectInput(
              "scenario_gender",
              "Gender",
              choices = c("Men", "Women"),
              selected = "Men"
            )
          ),
          
          column(
            width = 4,
            
            selectInput(
              "scenario_mode",
              "Scenario mode",
              choices = c(
                "TP only",
                "Full scenario"
              ),
              selected = "TP only"
            )
          ),
          
          column(
            width = 4,
            
            checkboxInput(
              "hide_completed",
              "Hide completed competitions",
              value = FALSE
            )
          )
        )
      ),
      
      br(),
      
      h4(
        "Team Pursuit",
        class = "section-heading"
      ),
      
      p(
        "Edit hypothetical finishing positions. Completed competitions are locked."
      ),
      
      rhandsontable::rHandsontableOutput("scenario_tp_matrix"),
      
      fluidRow(
        
        column(
          width = 8,
          
          selectizeInput(
            "add_tp_country",
            "Add a country to Team Pursuit",
            choices = NULL,
            options = list(
              placeholder = "Search for a country..."
            )
          )
        ),
        
        column(
          width = 4,
          
          br(),
          
          actionButton(
            "add_tp_country_button",
            "Add country",
            class = "btn-secondary"
          )
        )
      ),
      
      conditionalPanel(
        
        condition = "input.scenario_mode == 'Full scenario'",
        
        br(),
        
        h4(
          "Madison",
          class = "section-heading"
        ),
        
        p(
          "Edit hypothetical Madison results."
        ),
        
        rhandsontable::rHandsontableOutput("scenario_madison_matrix"),
        
        fluidRow(
          
          column(
            width = 8,
            
            selectizeInput(
              "add_madison_country",
              "Add a country to Madison",
              choices = NULL,
              options = list(
                placeholder = "Search for a country..."
              )
            )
          ),
          
          column(
            width = 4,
            
            br(),
            
            actionButton(
              "add_madison_country_button",
              "Add country",
              class = "btn-secondary"
            )
          )
        ),
        
        br(),
        
        h4(
          "Omnium",
          class = "section-heading"
        ),
        
        p(
          "Edit hypothetical Omnium results."
        ),
        
        rhandsontable::rHandsontableOutput("scenario_omnium_matrix"),
        
        fluidRow(
          
          column(
            width = 8,
            
            selectizeInput(
              "add_omnium_country",
              "Add a country to Omnium",
              choices = NULL,
              options = list(
                placeholder = "Search for a country..."
              )
            )
          ),
          
          column(
            width = 4,
            
            br(),
            
            actionButton(
              "add_omnium_country_button",
              "Add country",
              class = "btn-secondary"
            )
          )
        )
      ),
      
      br(),
      
      actionButton(
        "run_scenario",
        "Run Scenario",
        class = "btn-primary"
      ),
      
      br(),
      br(),
      
      h4(
        "Scenario Results",
        class = "section-heading"
      ),
      
      uiOutput("scenario_results_summary"),
      
      br(),
      
      downloadButton(
        "download_scenario",
        "Download Scenario Results (CSV)",
        class = "btn-success"
      )
    )
    
    
    
  ),
  
  # ----------------------------------------
  # FOOTER
  # ----------------------------------------
  
  br(),
  
  hr(),
  
  div(
    style = "text-align: center; color: #6C757D; padding: 10px;",
    
    "Olympic Cycling Qualification Tracker | LA 2028",
    
    br(),
    
    "Emily Wood"
  )
)
