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
  skin = "purple",
  
  dashboardHeader(
    title = span(
      icon("medal", style = "margin-right: 10px; color: #FFD700;"),
      "From Togas to Tracksuits"
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
    # Custom CSS - Olympic themed (Gold, Blue, Green)
    tags$head(
      tags$style(HTML("
        @import url('https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;600;700&display=swap');
        
        /* Global Styles */
        body, .content-wrapper, .main-sidebar {
          font-family: 'Poppins', sans-serif !important;
        }
        
        .content-wrapper {
          background: linear-gradient(135deg, #f0f4f8 0%, #e6eef5 100%) !important;
        }
        
        /* Header - Olympic Blue and Gold */
        .main-header .logo {
          background: linear-gradient(135deg, #0085C7 0%, #005A8C 100%) !important;
          font-weight: 700 !important;
          border-bottom: 4px solid #FFD700;
          box-shadow: 0 2px 8px rgba(0,0,0,0.15);
        }
        
        .main-header .navbar {
          background: linear-gradient(135deg, #0085C7 0%, #005A8C 100%) !important;
        }
        
        /* Sidebar - Deep Purple/Navy */
        .main-sidebar {
          background: linear-gradient(180deg, #2C3E50 0%, #34495E 100%) !important;
        }
        
        .sidebar-menu > li > a {
          color: #ECF0F1 !important;
          border-left: 4px solid transparent;
          transition: all 0.3s ease;
          padding: 16px 20px !important;
          font-weight: 500;
        }
        
        .sidebar-menu > li > a:hover {
          background: rgba(255, 215, 0, 0.15) !important;
          border-left: 4px solid #FFD700;
          transform: translateX(3px);
        }
        
        .sidebar-menu > li.active > a {
          background: linear-gradient(90deg, rgba(255, 215, 0, 0.25) 0%, transparent 100%) !important;
          border-left: 4px solid #FFD700;
          font-weight: 600;
          color: #FFD700 !important;
        }
        
        /* Box Styling - Clean and Modern */
        .box {
          border-radius: 12px !important;
          box-shadow: 0 4px 20px rgba(0,0,0,0.06) !important;
          border: 1px solid #E8ECF0 !important;
          transition: all 0.3s ease;
          background: white !important;
        }
        
        .box:hover {
          box-shadow: 0 8px 30px rgba(0,0,0,0.1) !important;
          transform: translateY(-2px);
        }
        
        .box-header {
          background: linear-gradient(135deg, #0085C7 0%, #005A8C 100%) !important;
          color: white !important;
          border-radius: 12px 12px 0 0 !important;
          padding: 18px 20px !important;
        }
        
        .box-header .box-title {
          font-weight: 600 !important;
          font-size: 17px !important;
          letter-spacing: 0.3px;
        }
        
        /* Metric Cards - Olympic Ring Colors */
        .metric-card {
          background: white;
          padding: 30px 20px;
          border-radius: 12px;
          text-align: center;
          box-shadow: 0 3px 15px rgba(0,0,0,0.08);
          border-top: 5px solid #FFD700;
          transition: all 0.3s ease;
          position: relative;
          overflow: hidden;
        }
        
        .metric-card::after {
          content: '';
          position: absolute;
          top: 0;
          right: 0;
          width: 60px;
          height: 60px;
          background: radial-gradient(circle, rgba(255,215,0,0.1) 0%, transparent 70%);
        }
        
        .metric-card:hover {
          transform: translateY(-5px) scale(1.02);
          box-shadow: 0 8px 25px rgba(0,133,199,0.15);
          border-top-color: #0085C7;
        }
        
        .metric-card h4 {
          color: #7F8C8D;
          font-size: 13px;
          font-weight: 600;
          text-transform: uppercase;
          letter-spacing: 1.2px;
          margin-bottom: 12px;
        }
        
        .metric-card h2 {
          color: #0085C7;
          font-size: 38px;
          font-weight: 700;
          margin: 0;
          background: linear-gradient(135deg, #0085C7, #005A8C);
          -webkit-background-clip: text;
          -webkit-text-fill-color: transparent;
        }
        
        /* Buttons - Olympic Gold */
        .btn-primary {
          background: linear-gradient(135deg, #FFD700 0%, #FFA500 100%) !important;
          border: none !important;
          color: #2C3E50 !important;
          font-weight: 600 !important;
          letter-spacing: 0.5px;
          transition: all 0.3s ease !important;
          box-shadow: 0 4px 15px rgba(255, 215, 0, 0.3) !important;
        }
        
        .btn-primary:hover {
          background: linear-gradient(135deg, #0085C7 0%, #005A8C 100%) !important;
          color: white !important;
          box-shadow: 0 6px 25px rgba(0, 133, 199, 0.4) !important;
          transform: translateY(-2px);
        }
        
        /* Headers - Olympic Colors */
        h2 {
          color: #0085C7;
          font-weight: 700;
          border-bottom: 4px solid #FFD700;
          display: inline-block;
          padding-bottom: 12px;
          margin-bottom: 25px;
        }
        
        h3, h4 {
          color: #2C3E50;
          font-weight: 600;
        }
        
        /* Home Page - Olympic Rings Theme */
        .olympic-home {
          background: white;
          border-radius: 20px;
          padding: 60px 40px;
          box-shadow: 0 10px 40px rgba(0,0,0,0.08);
          position: relative;
          overflow: hidden;
        }
        
        .olympic-home::before {
          content: '';
          position: absolute;
          top: 0;
          left: 0;
          right: 0;
          height: 6px;
          background: linear-gradient(90deg, 
            #0085C7 0%, #0085C7 20%,
            #000000 20%, #000000 40%,
            #EE334E 40%, #EE334E 60%,
            #FFD700 60%, #FFD700 80%,
            #00A651 80%, #00A651 100%);
        }
        
        .olympic-title {
          color: #0085C7;
          font-weight: 800;
          font-size: 52px;
          margin-bottom: 15px;
          text-shadow: 2px 2px 4px rgba(0,0,0,0.05);
        }
        
        .olympic-subtitle {
          color: #7F8C8D;
          font-size: 24px;
          font-weight: 500;
          margin-bottom: 35px;
        }
        
        .feature-box {
          background: linear-gradient(135deg, #F8F9FA 0%, #ECF0F1 100%);
          padding: 35px;
          border-radius: 15px;
          border-left: 6px solid #FFD700;
          max-width: 700px;
          margin: 35px auto;
        }
        
        .feature-box h4 {
          color: #0085C7;
          font-size: 22px;
          margin-bottom: 20px;
          font-weight: 700;
        }
        
        .feature-box ul {
          list-style: none;
          padding: 0;
        }
        
        .feature-box li {
          padding: 12px 0;
          font-size: 16px;
          color: #2C3E50;
          font-weight: 500;
          border-bottom: 1px solid #E0E0E0;
        }
        
        .feature-box li:last-child {
          border-bottom: none;
        }
        
        .feature-box li i {
          color: #FFD700;
          margin-right: 12px;
          font-size: 18px;
        }
        
        /* Tables */
        table thead {
          background: linear-gradient(135deg, #0085C7 0%, #005A8C 100%);
          color: white;
        }
        
        table thead th {
          padding: 15px;
          font-weight: 600;
          text-transform: uppercase;
          font-size: 12px;
          letter-spacing: 1px;
        }
        
        table tbody tr:hover {
          background: rgba(0, 133, 199, 0.05);
        }
        
        /* Form Controls */
        .form-control, .selectize-input {
          border-radius: 8px !important;
          border: 2px solid #E0E6ED !important;
          transition: all 0.3s ease !important;
        }
        
        .form-control:focus, .selectize-input.focus {
          border-color: #0085C7 !important;
          box-shadow: 0 0 0 0.2rem rgba(0, 133, 199, 0.15) !important;
        }
        
        /* Scrollbar - Olympic Colors */
        ::-webkit-scrollbar {
          width: 10px;
        }
        
        ::-webkit-scrollbar-track {
          background: #ECF0F1;
        }
        
        ::-webkit-scrollbar-thumb {
          background: linear-gradient(180deg, #0085C7, #FFD700);
          border-radius: 5px;
        }
        
        ::-webkit-scrollbar-thumb:hover {
          background: linear-gradient(180deg, #005A8C, #FFA500);
        }
        
        /* Animation */
        @keyframes fadeInUp {
          from {
            opacity: 0;
            transform: translateY(30px);
          }
          to {
            opacity: 1;
            transform: translateY(0);
          }
        }
        
        .box, .metric-card {
          animation: fadeInUp 0.6s ease;
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
                         class = "olympic-home",
                         style = "text-align: center;",
                         
                         icon("medal", style = "font-size: 90px; color: #FFD700; margin-bottom: 25px; filter: drop-shadow(0 4px 8px rgba(0,0,0,0.1));"),
                         
                         tags$h1("Olympic Analytics", class = "olympic-title"),
                         tags$h3("120 Years of Excellence (1896-2016)", class = "olympic-subtitle"),
                         
                         tags$p(
                           style = "font-size: 18px; line-height: 1.8; max-width: 800px; margin: 0 auto 35px auto; color: #5D6D7E;",
                           "Journey through over a century of Olympic Games data. Discover legendary athletes, ",
                           "compare nations, analyze sports trends, and uncover fascinating insights from the world's ",
                           "most celebrated sporting competition."
                         ),
                         
                         tags$div(
                           class = "feature-box",
                           tags$h4(icon("chart-line"), " What's Inside"),
                           tags$ul(
                             tags$li(icon("medal"), " 271,116 athlete-event records spanning 120 years"),
                             tags$li(icon("user"), " 134,732 unique athletes from around the globe"),
                             tags$li(icon("flag"), " 230 countries and territories represented"),
                             tags$li(icon("running"), " 66 different Olympic sports analyzed"),
                             tags$li(icon("trophy"), " Interactive charts and country comparisons"),
                             tags$li(icon("download"), " Export capabilities for deeper analysis")
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
                              h4("Sports"), h2("66")))
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
                column(3, div(class = "metric-card", 
                              h4("Total Medals"), h2(textOutput("athlete_total_medals", inline = TRUE)))),
                column(3, div(class = "metric-card", 
                              h4("Gold"), h2(textOutput("athlete_gold", inline = TRUE)))),
                column(3, div(class = "metric-card", 
                              h4("Silver"), h2(textOutput("athlete_silver", inline = TRUE)))),
                column(3, div(class = "metric-card", 
                              h4("Bronze"), h2(textOutput("athlete_bronze", inline = TRUE))))
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
                         column(4, div(class = "metric-card", 
                                       h4("Total Medals"), h2(textOutput("sport_medals", inline = TRUE)))),
                         column(4, div(class = "metric-card", 
                                       h4("Athletes"), h2(textOutput("sport_athletes", inline = TRUE)))),
                         column(4, div(class = "metric-card", 
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