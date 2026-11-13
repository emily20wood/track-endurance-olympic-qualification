
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
      title = "Overview",
      
      br(),
      
      h3(
        "Qualification overview",
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
