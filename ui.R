library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(tidyverse)
library(plotly)
library(htmltools)
library(DT)
library(readxl)

# Olympic countries list
olympic_countries <- c(
  "Afghanistan", "Albania", "Algeria", "Andorra", "Angola", "Antigua and Barbuda", 
  "Argentina", "Armenia", "Australia", "Austria", "Azerbaijan", "Bahamas", "Bahrain", 
  "Bangladesh", "Barbados", "Belarus", "Belgium", "Belize", "Benin", "Bhutan",
  "Bolivia", "Bosnia and Herzegovina", "Botswana", "Brazil", "Brunei", "Bulgaria", 
  "Burkina Faso", "Burundi", "Cambodia", "Cameroon", "Canada", "Cape Verde", 
  "Central African Republic", "Chad", "Chile", "China", "Colombia", "Comoros", 
  "Congo (Brazzaville)", "Congo (Kinshasa)", "Costa Rica", "Cote d'Ivoire", "Croatia", 
  "Cuba", "Cyprus", "Czech Republic", "Czechoslovakia", "Denmark", "Djibouti", 
  "Dominica", "Dominican Republic", "East Germany", "Ecuador", "Egypt", "El Salvador", 
  "Equatorial Guinea", "Eritrea", "Estonia", "Ethiopia", "Fiji", "Finland", "France",
  "Gabon", "Gambia", "Georgia", "Germany", "Ghana", "Great Britain", "Greece", 
  "Grenada", "Guam", "Guatemala", "Guinea", "Guinea Bissau", "Guyana", "Haiti", 
  "Honduras", "Hong Kong", "Hungary", "Iceland", "India", "Indonesia", "Iran", "Iraq",
  "Ireland", "Israel", "Italy", "Jamaica", "Japan", "Jordan", "Kazakhstan", "Kenya", 
  "Kiribati", "Kosovo", "Kuwait", "Kyrgyzstan", "Laos", "Latvia", "Lebanon", 
  "Lesotho", "Liberia", "Libya", "Liechtenstein", "Lithuania", "Luxembourg",
  "Macedonia", "Madagascar", "Malawi", "Malaysia", "Maldives", "Mali", "Malta", 
  "Marshall Islands", "Mauritania", "Mauritius", "Mexico", "Micronesia", "Moldova", 
  "Monaco", "Mongolia", "Montenegro", "Morocco", "Mozambique", "Myanmar", "Namibia", 
  "Nauru", "Nepal", "Netherlands", "Netherlands Antilles", "New Zealand", 
  "Newfoundland", "Nicaragua", "Niger", "Nigeria", "North Korea", "Norway", "Oman",
  "Pakistan", "Palau", "Palestine", "Panama", "Papua New Guinea", "Paraguay", "Peru", 
  "Philippines", "Poland", "Portugal", "Puerto Rico", "Qatar", "Refugee Olympic Athletes",
  "Rhodesia", "Romania", "Russia", "Rwanda", "Saint Kitts and Nevis", "Saint Lucia", 
  "Saint Vincent and the Grenadines", "Samoa", "San Marino", "Sao Tome and Principe", 
  "Saudi Arabia", "Senegal", "Serbia", "Serbia and Montenegro", "Seychelles", 
  "Sierra Leone", "Singapore", "Slovakia", "Slovenia", "Solomon Islands", "Somalia",
  "South Africa", "South Korea", "South Sudan", "Soviet Union", "Spain", "Sri Lanka", 
  "Sudan", "Suriname", "Swaziland", "Sweden", "Switzerland", "Syria", "Tajikistan", 
  "Tanzania", "Thailand", "Timor Leste", "Togo", "Tonga", "Trinidad and Tobago", 
  "Tunisia", "Turkey", "Turkmenistan", "Tuvalu", "Uganda", "Ukraine", "Unified Team",
  "United Arab Emirates", "United Arab Republic", "United States", 
  "United States Virgin Islands", "Uruguay", "Uzbekistan", "Vanuatu", "Venezuela", 
  "Vietnam", "West Germany", "West Indies Federation", "Yemen", "Yugoslavia", 
  "Zambia", "Zimbabwe"
)

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = span(
      icon("medal", style = "margin-right: 10px; color: #FFD700;"),
      "Olympic Analytics"
    ),
    titleWidth = 300
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "tabs",
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("medal")),
      menuItem("Athletes", tabName = "athletes", icon = icon("user")),
      menuItem("Countries", tabName = "countries", icon = icon("flag")),
      menuItem("Sports", tabName = "sports", icon = icon("running")),
      menuItem("Data Explorer", tabName = "explorer", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    # Custom CSS
    tags$head(
      tags$style(HTML("
        @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;600;700&display=swap');
        
        body, .content-wrapper, .main-sidebar {
          font-family: 'Inter', sans-serif !important;
        }
        
        .content-wrapper {
          background: linear-gradient(135deg, #f5f7fa 0%, #e8ecf1 100%) !important;
        }
        
        .main-header .logo {
          background: linear-gradient(135deg, #0066cc 0%, #004c99 100%) !important;
          font-weight: 700 !important;
          border-bottom: 3px solid #FFD700;
        }
        
        .main-header .navbar {
          background: linear-gradient(135deg, #0066cc 0%, #004c99 100%) !important;
        }
        
        .main-sidebar {
          background: linear-gradient(180deg, #1a1a2e 0%, #16213e 100%) !important;
        }
        
        .sidebar-menu > li > a {
          color: #e8ecf1 !important;
          border-left: 3px solid transparent;
          transition: all 0.3s ease;
        }
        
        .sidebar-menu > li > a:hover {
          background: rgba(255, 215, 0, 0.1) !important;
          border-left: 3px solid #FFD700;
        }
        
        .sidebar-menu > li.active > a {
          background: linear-gradient(90deg, rgba(255, 215, 0, 0.2) 0%, transparent 100%) !important;
          border-left: 3px solid #FFD700;
        }
        
        .box {
          border-radius: 15px !important;
          box-shadow: 0 8px 25px rgba(0,0,0,0.08) !important;
          border: none !important;
        }
        
        .box-header {
          background: linear-gradient(135deg, #0066cc 0%, #004c99 100%) !important;
          color: white !important;
          border-radius: 15px 15px 0 0 !important;
        }
        
        .metric-card {
          background: linear-gradient(135deg, #ffffff 0%, #f8f9fa 100%);
          padding: 25px;
          border-radius: 15px;
          text-align: center;
          box-shadow: 0 5px 20px rgba(0,0,0,0.06);
          border-top: 4px solid #FFD700;
        }
        
        .metric-card h4 {
          color: #6c757d;
          font-size: 14px;
          font-weight: 600;
          text-transform: uppercase;
        }
        
        .metric-card h2 {
          color: #0066cc;
          font-size: 36px;
          font-weight: 700;
        }
        
        .btn-primary {
          background: linear-gradient(135deg, #0066cc 0%, #004c99 100%) !important;
          border: none !important;
          color: white !important;
        }
        
        .btn-primary:hover {
          background: linear-gradient(135deg, #FFD700 0%, #FFA500 100%) !important;
        }
        
        h2 {
          color: #0066cc;
          font-weight: 700;
          border-bottom: 3px solid #FFD700;
          display: inline-block;
          padding-bottom: 10px;
        }
      "))
    ),
    
    tabItems(
      # ========================================
      # HOME TAB
      # ========================================
      tabItem(tabName = "home",
              fluidRow(
                column(width = 12,
                       tags$div(
                         style = "text-align: center; background: white; padding: 50px; border-radius: 20px; box-shadow: 0 10px 40px rgba(0,0,0,0.1);",
                         
                         icon("medal", style = "font-size: 80px; color: #FFD700; margin-bottom: 20px;"),
                         
                         tags$h1("Olympic Analytics", 
                                 style = "color: #0066cc; font-weight: 800; font-size: 48px; margin-bottom: 15px;"),
                         tags$h3("120 Years of Olympic History (1896-2016)", 
                                 style = "color: #666; font-size: 28px; margin-bottom: 30px;"),
                         
                         tags$p(
                           style = "font-size: 19px; line-height: 1.8; max-width: 850px; margin: 0 auto 30px auto; color: #495057;",
                           "Explore over a century of Olympic Games data. Discover legendary athletes, ",
                           "compare nations, analyze sports trends, and uncover fascinating insights from the world's ",
                           "greatest sporting event."
                         ),
                         
                         tags$div(
                           style = "background: #f8f9fa; padding: 30px; border-radius: 15px; max-width: 650px; margin: 30px auto; border-left: 5px solid #FFD700;",
                           tags$h4("Features:", style = "color: #0066cc; margin-bottom: 20px;"),
                           tags$ul(
                             style = "list-style: none; padding: 0; text-align: left;",
                             tags$li(icon("medal"), " 271,116 athlete-event records from 1896-2016"),
                             tags$li(icon("user"), " 134,732 unique athletes profiled"),
                             tags$li(icon("flag"), " 230 countries and territories"),
                             tags$li(icon("running"), " 66 different Olympic sports"),
                             tags$li(icon("chart-line"), " Interactive visualizations and comparisons"),
                             tags$li(icon("download"), " Export data for your own analysis")
                           )
                         )
                       )
                )
              )
      ),
      
      # ========================================
      # DASHBOARD TAB
      # ========================================
      tabItem(tabName = "dashboard",
              h2(icon("medal"), " Medal Dashboard"),
              
              # Summary Cards
              fluidRow(
                column(3, div(class = "metric-card", 
                              h4("Total Medals"), h2("39,783"))),
                column(3, div(class = "metric-card", 
                              h4("Athletes"), h2("134,732"))),
                column(3, div(class = "metric-card", 
                              h4("Countries"), h2("230"))),
                column(3, div(class = "metric-card", 
                              h4("Events"), h2("15,000+")))
              ),
              
              br(),
              
              # Charts
              fluidRow(
                column(6,
                       box(width = NULL, solidHeader = TRUE,
                           title = "Medal Distribution",
                           plotlyOutput("medal_pie_chart", height = "350px"))
                ),
                column(6,
                       box(width = NULL, solidHeader = TRUE,
                           title = "Medals Over Time",
                           plotlyOutput("medals_timeline", height = "350px"))
                )
              ),
              
              fluidRow(
                column(6,
                       box(width = NULL, solidHeader = TRUE,
                           title = "Top 10 Countries",
                           plotlyOutput("top_countries_bar", height = "400px"))
                ),
                column(6,
                       box(width = NULL, solidHeader = TRUE,
                           title = "Top 10 Athletes",
                           plotlyOutput("top_athletes_bar", height = "400px"))
                )
              )
      ),
      
      # ========================================
      # ATHLETES TAB
      # ========================================
      tabItem(tabName = "athletes",
              h2(icon("user"), " Athlete Profiles"),
              
              fluidRow(
                column(4,
                       box(width = NULL, status = "primary", solidHeader = TRUE,
                           title = "Search Athletes",
                           selectInput("athlete_search", "Select Athlete:",
                                       choices = NULL, selected = NULL),
                           hr(),
                           helpText(icon("info-circle"), " Search for any Olympic athlete to view their complete profile and medal history.")
                       )
                ),
                column(8,
                       box(width = NULL, solidHeader = TRUE,
                           title = "Athlete Profile",
                           uiOutput("athlete_profile_display"))
                )
              ),
              
              fluidRow(
                column(4, div(class = "metric-card", 
                              h4("Total Medals"), h2(textOutput("athlete_total_medals", inline = TRUE)))),
                column(4, div(class = "metric-card", 
                              h4("Gold"), h2(textOutput("athlete_gold", inline = TRUE)))),
                column(4, div(class = "metric-card", 
                              h4("Silver"), h2(textOutput("athlete_silver", inline = TRUE))))
              ),
              
              br(),
              
              fluidRow(
                column(12,
                       box(width = NULL, solidHeader = TRUE,
                           title = "Medal Timeline",
                           plotlyOutput("athlete_timeline", height = "400px"))
                )
              ),
              
              fluidRow(
                column(12,
                       box(width = NULL, solidHeader = TRUE,
                           title = "Complete Record",
                           DT::dataTableOutput("athlete_record_table"))
                )
              )
      ),
      
      # ========================================
      # COUNTRIES TAB
      # ========================================
      tabItem(tabName = "countries",
              h2(icon("flag"), " Country Comparison"),
              
              fluidRow(
                column(4,
                       box(width = NULL, status = "primary", solidHeader = TRUE,
                           title = "Select Countries",
                           selectInput("country1", "Country 1:",
                                       choices = olympic_countries, selected = "United States"),
                           selectInput("country2", "Country 2:",
                                       choices = olympic_countries, selected = "China"),
                           hr(),
                           helpText(icon("info-circle"), " Compare Olympic performance between any two nations.")
                       )
                ),
                column(8,
                       box(width = NULL, solidHeader = TRUE,
                           title = "Medal Comparison",
                           plotlyOutput("country_comparison", height = "400px"))
                )
              ),
              
              fluidRow(
                column(12,
                       box(width = NULL, solidHeader = TRUE,
                           title = "Performance Over Time",
                           plotlyOutput("country_timeline", height = "400px"))
                )
              )
      ),
      
      # ========================================
      # SPORTS TAB
      # ========================================
      tabItem(tabName = "sports",
              h2(icon("running"), " Sport Analytics"),
              
              fluidRow(
                column(4,
                       box(width = NULL, status = "primary", solidHeader = TRUE,
                           title = "Select Sport",
                           selectInput("sport_select", "Choose a Sport:",
                                       choices = NULL, selected = NULL),
                           hr(),
                           helpText(icon("info-circle"), " Analyze performance trends for any Olympic sport.")
                       )
                ),
                column(8,
                       fluidRow(
                         column(6, div(class = "metric-card", 
                                       h4("Total Medals"), h2(textOutput("sport_medals", inline = TRUE)))),
                         column(6, div(class = "metric-card", 
                                       h4("Countries"), h2(textOutput("sport_countries", inline = TRUE))))
                       )
                )
              ),
              
              br(),
              
              fluidRow(
                column(6,
                       box(width = NULL, solidHeader = TRUE,
                           title = "Top Countries",
                           plotlyOutput("sport_top_countries", height = "400px"))
                ),
                column(6,
                       box(width = NULL, solidHeader = TRUE,
                           title = "Top Athletes",
                           plotlyOutput("sport_top_athletes", height = "400px"))
                )
              )
      ),
      
      # ========================================
      # DATA EXPLORER TAB
      # ========================================
      tabItem(tabName = "explorer",
              h2(icon("table"), " Data Explorer"),
              
              fluidRow(
                column(12,
                       box(width = NULL, status = "primary", solidHeader = TRUE,
                           title = "Filter Data",
                           fluidRow(
                             column(3, selectInput("explorer_sport", "Sport:", 
                                                   choices = c("All" = "all"), selected = "all")),
                             column(3, selectInput("explorer_country", "Country:", 
                                                   choices = c("All" = "all"), selected = "all")),
                             column(3, selectInput("explorer_year", "Year:", 
                                                   choices = c("All" = "all"), selected = "all")),
                             column(3, selectInput("explorer_medal", "Medal:", 
                                                   choices = c("All" = "all", "Gold", "Silver", "Bronze"), 
                                                   selected = "all"))
                           ),
                           downloadButton("download_data", "Download Filtered Data", 
                                          class = "btn-primary", icon = icon("download"))
                       )
                )
              ),
              
              fluidRow(
                column(12,
                       box(width = NULL, solidHeader = TRUE,
                           title = "Olympic Dataset",
                           DT::dataTableOutput("explorer_table"))
                )
              )
      )
    )
  )
)