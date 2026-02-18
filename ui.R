# ============================================================================
# Olympic Athletes Interactive Shiny Dashboard
# Run this app with: shiny::runApp("path/to/this/file")
# ============================================================================

library(shiny)
library(ggplot2)
library(dplyr)
library(maps)
library(viridis)
library(scales)

# ============================================================================
# LOAD DATA - IMPORTANT: Must be in same directory as this script!
# ============================================================================

# Get the directory where this script is located
script_dir <- getwd()

# Try to load the data
data_file <- "athlete_events_Olympic_Dataset.csv"

if (!file.exists(data_file)) {
  stop("ERROR: Cannot find 'athlete_events_Olympic_Dataset.csv' in the current directory.\n",
       "Current directory: ", script_dir, "\n",
       "Please make sure the CSV file is in the same folder as this R script.")
}

# Load the data
cat("Loading Olympic data...\n")
df <- read.csv(data_file, stringsAsFactors = FALSE)
cat("✓ Data loaded successfully:", nrow(df), "records\n\n")

# Check if data loaded correctly
if (!is.data.frame(df)) {
  stop("ERROR: Data did not load as a data frame. Check your CSV file.")
}

# Prepare data
df$Medal <- factor(df$Medal, levels = c("Gold", "Silver", "Bronze"))
df$Season <- factor(df$Season)

# NOC to country mapping for maps
noc_to_country <- data.frame(
  NOC = c("USA", "RUS", "GER", "GBR", "FRA", "ITA", "SWE", "CHN", "AUS", "HUN",
          "JPN", "FIN", "NED", "CAN", "NOR", "POL", "BRA", "ESP", "KOR", "CUB",
          "ROU", "BUL", "SUI", "DEN", "AUT", "BEL", "CZE", "TUR", "GRE", "ARG",
          "MEX", "IND", "NZL", "RSA", "UKR", "JAM", "KEN", "ETH", "IRL", "EGY",
          "POR", "SRB", "SVK", "CHI", "ISR", "CRO", "COL", "URS", "GDR", "FRG"),
  region = c("USA", "Russia", "Germany", "UK", "France", "Italy", "Sweden", "China", 
             "Australia", "Hungary", "Japan", "Finland", "Netherlands", "Canada", 
             "Norway", "Poland", "Brazil", "Spain", "South Korea", "Cuba", "Romania", 
             "Bulgaria", "Switzerland", "Denmark", "Austria", "Belgium", "Czech Republic", 
             "Turkey", "Greece", "Argentina", "Mexico", "India", "New Zealand", 
             "South Africa", "Ukraine", "Jamaica", "Kenya", "Ethiopia", "Ireland", 
             "Egypt", "Portugal", "Serbia", "Slovakia", "Chile", "Israel", "Croatia", 
             "Colombia", "Russia", "Germany", "Germany"),
  stringsAsFactors = FALSE
)

# ============================================================================
# USER INTERFACE
# ============================================================================
ui <- fluidPage(
  
  # Custom CSS
  tags$head(
    tags$style(HTML("
      .title-panel {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        padding: 30px;
        margin-bottom: 30px;
        border-radius: 10px;
      }
      .well {
        background-color: #f8f9fa;
        border: 1px solid #dee2e6;
      }
    "))
  ),
  
  # Title
  div(class = "title-panel",
      h1("🏅 Olympic Athletes Dashboard", style = "margin: 0; font-weight: bold;"),
      h4("Interactive exploration of 120 years of Olympic history (1896-2016)", 
         style = "margin-top: 10px; opacity: 0.9;")
  ),
  
  # Tabs
  tabsetPanel(
    
    # TAB 1: OVERVIEW
    tabPanel("📊 Overview",
             br(),
             fluidRow(
               column(3,
                      wellPanel(
                        h4("📅 Year Range"),
                        sliderInput("year_range", NULL,
                                    min = 1896, max = 2016,
                                    value = c(1896, 2016), step = 4, sep = ""),
                        hr(),
                        h4("🏃 Season"),
                        checkboxGroupInput("season_filter", NULL,
                                           choices = c("Summer", "Winter"),
                                           selected = c("Summer", "Winter")),
                        hr(),
                        h4("📈 Statistics"),
                        verbatimTextOutput("stats_summary")
                      )
               ),
               column(9,
                      plotOutput("participation_plot", height = "400px"),
                      br(),
                      plotOutput("gender_plot", height = "400px")
               )
             )
    ),
    
    # TAB 2: MEDALS
    tabPanel("🥇 Medals",
             br(),
             fluidRow(
               column(3,
                      wellPanel(
                        h4("🎯 Top Countries"),
                        sliderInput("top_n_countries", "Number of countries:",
                                    min = 5, max = 30, value = 15, step = 5),
                        hr(),
                        h4("📅 Year Range"),
                        sliderInput("medal_year_range", NULL,
                                    min = 1896, max = 2016,
                                    value = c(1896, 2016), step = 4, sep = ""),
                        hr(),
                        h4("🏅 Medal Type"),
                        checkboxGroupInput("medal_filter", NULL,
                                           choices = c("Gold", "Silver", "Bronze"),
                                           selected = c("Gold", "Silver", "Bronze"))
                      )
               ),
               column(9,
                      plotOutput("medal_distribution_plot", height = "600px"),
                      br(),
                      h4("📋 Top Countries Table"),
                      tableOutput("medal_table")
               )
             )
    ),
    
    # TAB 3: WORLD MAPS
    tabPanel("🗺️ World Maps",
             br(),
             fluidRow(
               column(3,
                      wellPanel(
                        h4("🗺️ Map Type"),
                        radioButtons("map_type", NULL,
                                     choices = c("Total Medals" = "total",
                                                 "Medal Efficiency" = "efficiency",
                                                 "Gold Medals Only" = "gold"),
                                     selected = "total"),
                        hr(),
                        h4("📅 Year Range"),
                        sliderInput("map_year_range", NULL,
                                    min = 1896, max = 2016,
                                    value = c(1896, 2016), step = 4, sep = ""),
                        hr(),
                        h4("🎨 Color Scheme"),
                        selectInput("map_colors", NULL,
                                    choices = c("Inferno" = "inferno",
                                                "Viridis" = "viridis",
                                                "Plasma" = "plasma",
                                                "Magma" = "magma"),
                                    selected = "inferno"),
                        hr(),
                        checkboxInput("show_log_scale", "Use log scale", value = TRUE)
                      )
               ),
               column(9,
                      plotOutput("world_map_plot", height = "600px"),
                      br(),
                      h4("🏆 Top 10 Countries in Selected Range"),
                      tableOutput("map_countries_table")
               )
             )
    ),
    
    # TAB 4: ATHLETES
    tabPanel("👤 Athletes",
             br(),
             fluidRow(
               column(3,
                      wellPanel(
                        h4("🎯 Top Athletes"),
                        sliderInput("top_n_athletes", "Number of athletes:",
                                    min = 5, max = 20, value = 10, step = 5),
                        hr(),
                        h4("⚧️ Gender"),
                        radioButtons("athlete_gender", NULL,
                                     choices = c("All" = "all", "Male" = "M", "Female" = "F"),
                                     selected = "all"),
                        hr(),
                        h4("🏃 Sport Filter"),
                        selectInput("sport_filter", NULL,
                                    choices = c("All Sports" = "all"),
                                    selected = "all")
                      )
               ),
               column(9,
                      plotOutput("top_athletes_plot", height = "500px"),
                      br(),
                      plotOutput("age_distribution_plot", height = "400px")
               )
             )
    ),
    
    # TAB 5: AGE & SUCCESS
    tabPanel("📈 Age & Success",
             br(),
             fluidRow(
               column(3,
                      wellPanel(
                        h4("🎯 Age Analysis"),
                        sliderInput("age_range", "Age range:",
                                    min = 10, max = 70, value = c(15, 45), step = 1),
                        hr(),
                        h4("📊 Group Size"),
                        sliderInput("age_group_size", "Age group width (years):",
                                    min = 2, max = 10, value = 5, step = 1),
                        hr(),
                        h4("🏃 Season"),
                        radioButtons("age_season", NULL,
                                     choices = c("All" = "all", "Summer" = "Summer", "Winter" = "Winter"),
                                     selected = "all")
                      )
               ),
               column(9,
                      plotOutput("medal_success_by_age_plot", height = "400px"),
                      br(),
                      plotOutput("age_histogram_plot", height = "400px")
               )
             )
    )
  ),
  
  # Footer
  hr(),
  div(style = "text-align: center; padding: 20px; color: #6c757d;",
      p("📊 Data: 120 years of Olympic history | Built with Shiny in R"),
      p("Dataset contains", format(nrow(df), big.mark = ","), "records from 1896-2016")
  )
)

# ============================================================================
# SERVER LOGIC
# ============================================================================
server <- function(input, output, session) {
  
  # Update sport filter choices
  observe({
    sports_list <- c("All Sports" = "all", sort(unique(df$Sport)))
    updateSelectInput(session, "sport_filter", choices = sports_list)
  })
  
  # Filtered data for overview
  filtered_data <- reactive({
    req(input$year_range, input$season_filter)
    df %>%
      filter(Year >= input$year_range[1], 
             Year <= input$year_range[2],
             Season %in% input$season_filter)
  })
  
  # Filtered data for medals
  medal_filtered_data <- reactive({
    req(input$medal_year_range, input$medal_filter)
    df %>%
      filter(Year >= input$medal_year_range[1],
             Year <= input$medal_year_range[2],
             !is.na(Medal),
             Medal %in% input$medal_filter)
  })
  
  # Filtered data for maps
  map_filtered_data <- reactive({
    req(input$map_year_range)
    df %>%
      filter(Year >= input$map_year_range[1],
             Year <= input$map_year_range[2])
  })
  
  # OVERVIEW TAB
  output$stats_summary <- renderText({
    data <- filtered_data()
    paste0(
      "Athletes: ", format(n_distinct(data$ID), big.mark = ","), "\n",
      "Countries: ", n_distinct(data$NOC), "\n",
      "Sports: ", n_distinct(data$Sport), "\n",
      "Events: ", format(n_distinct(data$Event), big.mark = ","), "\n",
      "Medals: ", format(sum(!is.na(data$Medal)), big.mark = ",")
    )
  })
  
  output$participation_plot <- renderPlot({
    participation <- filtered_data() %>%
      group_by(Year, Season) %>%
      summarise(Athletes = n_distinct(ID), .groups = "drop")
    
    ggplot(participation, aes(x = Year, y = Athletes, color = Season)) +
      geom_line(size = 1.5) +
      geom_point(size = 3) +
      scale_color_manual(values = c("Summer" = "#E63946", "Winter" = "#457B9D")) +
      scale_y_continuous(labels = comma) +
      labs(title = "Olympic Participation Over Time",
           x = "Year", y = "Number of Athletes") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold", size = 16),
            legend.position = "top")
  })
  
  output$gender_plot <- renderPlot({
    gender_data <- filtered_data() %>%
      group_by(Year, Sex) %>%
      summarise(Athletes = n_distinct(ID), .groups = "drop") %>%
      group_by(Year) %>%
      mutate(Percentage = Athletes / sum(Athletes) * 100)
    
    ggplot(gender_data, aes(x = Year, y = Percentage, fill = Sex)) +
      geom_area(alpha = 0.8) +
      scale_fill_manual(values = c("M" = "#3A86FF", "F" = "#FF006E"),
                        labels = c("M" = "Male", "F" = "Female")) +
      labs(title = "Gender Representation Over Time",
           x = "Year", y = "Percentage of Athletes", fill = "Gender") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold", size = 16),
            legend.position = "top")
  })
  
  # MEDALS TAB
  output$medal_distribution_plot <- renderPlot({
    medal_data <- medal_filtered_data() %>%
      group_by(NOC, Medal) %>%
      summarise(Count = n(), .groups = "drop") %>%
      group_by(NOC) %>%
      mutate(Total = sum(Count)) %>%
      ungroup() %>%
      arrange(desc(Total)) %>%
      filter(NOC %in% unique(NOC)[1:input$top_n_countries])
    
    ggplot(medal_data, aes(x = reorder(NOC, Total), y = Count, fill = Medal)) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = c("Gold" = "#FFD700", "Silver" = "#C0C0C0", "Bronze" = "#CD7F32")) +
      scale_y_continuous(labels = comma) +
      labs(title = paste("Top", input$top_n_countries, "Countries by Medal Count"),
           x = "Country (NOC)", y = "Number of Medals", fill = "Medal Type") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold", size = 16),
            legend.position = "top")
  })
  
  output$medal_table <- renderTable({
    medal_filtered_data() %>%
      group_by(NOC) %>%
      summarise(
        Gold = sum(Medal == "Gold"),
        Silver = sum(Medal == "Silver"),
        Bronze = sum(Medal == "Bronze"),
        Total = n(),
        .groups = "drop"
      ) %>%
      arrange(desc(Total)) %>%
      head(10)
  })
  
  # WORLD MAPS TAB
  output$world_map_plot <- renderPlot({
    data <- map_filtered_data()
    
    # Calculate metrics
    if (input$map_type == "total") {
      country_data <- data %>%
        filter(!is.na(Medal)) %>%
        group_by(NOC) %>%
        summarise(Value = n(), .groups = "drop")
      legend_label <- "Total Medals"
      
    } else if (input$map_type == "efficiency") {
      country_data <- data %>%
        filter(!is.na(Medal)) %>%
        group_by(NOC) %>%
        summarise(
          TotalMedals = n(),
          Athletes = n_distinct(ID),
          .groups = "drop"
        ) %>%
        filter(Athletes >= 50) %>%
        mutate(Value = TotalMedals / Athletes)
      legend_label <- "Medals per Athlete"
      
    } else {
      country_data <- data %>%
        filter(Medal == "Gold") %>%
        group_by(NOC) %>%
        summarise(Value = n(), .groups = "drop")
      legend_label <- "Gold Medals"
    }
    
    # Merge with country names
    map_data <- country_data %>%
      left_join(noc_to_country, by = "NOC") %>%
      filter(!is.na(region))
    
    world_map <- map_data("world")
    world_with_data <- world_map %>%
      left_join(map_data, by = "region")
    
    # Create plot
    p <- ggplot() +
      geom_polygon(data = world_with_data,
                   aes(x = long, y = lat, group = group, fill = Value),
                   color = "white", size = 0.1) +
      theme_void(base_size = 14) +
      theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            legend.position = "right") +
      coord_fixed(1.3)
    
    # Apply colors
    if (input$map_type == "efficiency") {
      p <- p + scale_fill_gradient2(
        low = "#2166AC", mid = "#F7F7F7", high = "#B2182B",
        midpoint = 0.3, na.value = "gray90", name = legend_label
      ) +
        labs(title = "Olympic Medal Efficiency by Country")
      
    } else {
      if (input$show_log_scale) {
        p <- p + scale_fill_viridis(
          option = input$map_colors, na.value = "gray90",
          trans = "log10", labels = comma, name = legend_label
        )
      } else {
        p <- p + scale_fill_viridis(
          option = input$map_colors, na.value = "gray90",
          labels = comma, name = legend_label
        )
      }
      
      title_text <- ifelse(input$map_type == "total",
                           "Olympic Medal Distribution by Country",
                           "Olympic Gold Medal Distribution by Country")
      p <- p + labs(title = title_text)
    }
    
    p
  })
  
  output$map_countries_table <- renderTable({
    data <- map_filtered_data()
    
    if (input$map_type == "gold") {
      data %>%
        filter(Medal == "Gold") %>%
        count(NOC, name = "Gold_Medals") %>%
        arrange(desc(Gold_Medals)) %>%
        head(10)
    } else {
      data %>%
        filter(!is.na(Medal)) %>%
        count(NOC, name = "Total_Medals") %>%
        arrange(desc(Total_Medals)) %>%
        head(10)
    }
  })
  
  # ATHLETES TAB
  output$top_athletes_plot <- renderPlot({
    data <- df %>% filter(!is.na(Medal))
    
    if (input$athlete_gender != "all") {
      data <- data %>% filter(Sex == input$athlete_gender)
    }
    
    if (input$sport_filter != "all") {
      data <- data %>% filter(Sport == input$sport_filter)
    }
    
    top_athletes <- data %>%
      group_by(Name) %>%
      summarise(
        Gold = sum(Medal == "Gold"),
        Silver = sum(Medal == "Silver"),
        Bronze = sum(Medal == "Bronze"),
        Total = n(),
        .groups = "drop"
      ) %>%
      arrange(desc(Total), desc(Gold)) %>%
      head(input$top_n_athletes)
    
    top_athletes_long <- top_athletes %>%
      tidyr::pivot_longer(cols = c(Gold, Silver, Bronze), 
                          names_to = "Medal", values_to = "Count") %>%
      mutate(Medal = factor(Medal, levels = c("Gold", "Silver", "Bronze")))
    
    ggplot(top_athletes_long, aes(x = reorder(Name, Total), y = Count, fill = Medal)) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = c("Gold" = "#FFD700", "Silver" = "#C0C0C0", "Bronze" = "#CD7F32")) +
      labs(title = paste("Top", input$top_n_athletes, "Most Decorated Athletes"),
           x = "Athlete", y = "Number of Medals") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold", size = 16),
            legend.position = "top")
  })
  
  output$age_distribution_plot <- renderPlot({
    data <- df %>% filter(!is.na(Age), Age >= 10, Age <= 70)
    
    if (input$athlete_gender != "all") {
      data <- data %>% filter(Sex == input$athlete_gender)
    }
    
    ggplot(data, aes(x = Age, fill = Season)) +
      geom_histogram(binwidth = 1, alpha = 0.7, position = "identity") +
      scale_fill_manual(values = c("Summer" = "#E63946", "Winter" = "#457B9D")) +
      scale_y_continuous(labels = comma) +
      labs(title = "Age Distribution of Olympic Athletes",
           x = "Age (years)", y = "Number of Athletes") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold", size = 16),
            legend.position = "top")
  })
  
  # AGE & SUCCESS TAB
  output$medal_success_by_age_plot <- renderPlot({
    data <- df %>%
      filter(!is.na(Age), Age >= input$age_range[1], Age <= input$age_range[2])
    
    if (input$age_season != "all") {
      data <- data %>% filter(Season == input$age_season)
    }
    
    breaks_seq <- seq(input$age_range[1], input$age_range[2], by = input$age_group_size)
    
    age_medals <- data %>%
      mutate(AgeGroup = cut(Age, breaks = breaks_seq, include.lowest = TRUE)) %>%
      group_by(AgeGroup) %>%
      summarise(
        TotalAthletes = n(),
        MedalsWon = sum(!is.na(Medal)),
        SuccessRate = MedalsWon / TotalAthletes * 100,
        .groups = "drop"
      )
    
    ggplot(age_medals, aes(x = AgeGroup, y = SuccessRate)) +
      geom_col(fill = "#06FFA5", alpha = 0.8) +
      geom_text(aes(label = sprintf("%.1f%%", SuccessRate)), vjust = -0.5, fontface = "bold") +
      labs(title = "Medal Success Rate by Age Group",
           x = "Age Group", y = "Success Rate (%)") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold", size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$age_histogram_plot <- renderPlot({
    data <- df %>%
      filter(!is.na(Age), Age >= input$age_range[1], Age <= input$age_range[2])
    
    if (input$age_season != "all") {
      data <- data %>% filter(Season == input$age_season)
    }
    
    data <- data %>%
      mutate(MedalStatus = ifelse(is.na(Medal), "No Medal", "Medal Winner"))
    
    ggplot(data, aes(x = Age, fill = MedalStatus)) +
      geom_histogram(binwidth = 1, alpha = 0.7, position = "identity") +
      scale_fill_manual(values = c("No Medal" = "#95a5a6", "Medal Winner" = "#f39c12")) +
      scale_y_continuous(labels = comma) +
      labs(title = "Age Distribution: Medal Winners vs. All Athletes",
           x = "Age (years)", y = "Number of Athletes", fill = "Status") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold", size = 16),
            legend.position = "top")
  })
}

# ============================================================================
# RUN THE APP
# ============================================================================
shinyApp(ui = ui, server = server)
