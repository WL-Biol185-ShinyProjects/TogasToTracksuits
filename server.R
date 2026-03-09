library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyverse)
library(plotly)
library(DT)
library(readxl)

# ============================================================
# LOAD & CLEAN DATA
# ============================================================
olympic_data <- read_excel("athlete_events-Olympic_Dataset.xlsx") %>%
  mutate(
    Medal = ifelse(Medal == "NA" | is.na(Medal), NA, Medal),
    Age    = suppressWarnings(as.numeric(ifelse(Age    == "NA", NA, Age))),
    Height = suppressWarnings(as.numeric(ifelse(Height == "NA", NA, Height))),
    Weight = suppressWarnings(as.numeric(ifelse(Weight == "NA", NA, Weight))),
    Year   = as.integer(Year)
  )

medal_data <- olympic_data %>% filter(!is.na(Medal))

# Colour palette (consistent across charts)
GOLD   <- "#FFD700"
SILVER <- "#C0C0C0"
BRONZE <- "#CD7F32"
BLUE   <- "#0085C7"
NAVY   <- "#005A8C"

medal_colors <- c(Gold = GOLD, Silver = SILVER, Bronze = BRONZE)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {
  
  # ── Populate dynamic dropdowns ──────────────────────────
  observe({
    athletes <- sort(unique(olympic_data$Name))
    updateSelectInput(session, "athlete_search",
                      choices  = athletes,
                      selected = athletes[1])
  })
  
  observe({
    sports <- sort(unique(olympic_data$Sport))
    updateSelectInput(session, "sport_select",
                      choices  = sports,
                      selected = sports[1])
    updateSelectInput(session, "explorer_sport",
                      choices  = c("All" = "all", sports),
                      selected = "all")
  })
  
  observe({
    countries <- sort(unique(olympic_data$Team))
    updateSelectInput(session, "explorer_country",
                      choices  = c("All" = "all", countries),
                      selected = "all")
  })
  
  observe({
    years <- sort(unique(olympic_data$Year))
    updateSelectInput(session, "explorer_year",
                      choices  = c("All" = "all", years),
                      selected = "all")
  })
  
  # ============================================================
  # DASHBOARD TAB
  # ============================================================
  
  # Medal distribution pie
  output$medal_pie_chart <- renderPlotly({
    counts <- medal_data %>%
      count(Medal) %>%
      mutate(Medal = factor(Medal, levels = c("Gold","Silver","Bronze")))
    
<<<<<<< HEAD
    # ========== LOAD AND PROCESS DATA ==========
    olympic_data <- reactive({
      df <- read_excel("athlete_events-Olympic Dataset.xlsx")
      
      # Add calculated fields
      df <- df %>%
        mutate(
          medal_won = !is.na(Medal),
          medal_type = ifelse(is.na(Medal), "None", Medal),
          age_group = case_when(
            is.na(Age) ~ "Unknown",
            Age < 20 ~ "Under 20",
            Age < 30 ~ "20-29",
            Age < 40 ~ "30-39",
            TRUE ~ "40+"
          ),
          decade = floor(Year / 10) * 10,
          # Create era categories
          era = case_when(
            Year < 1920 ~ "Early Era (1896-1919)",
            Year < 1950 ~ "Pre-War Era (1920-1949)",
            Year < 1980 ~ "Cold War Era (1950-1979)",
            Year < 2000 ~ "Modern Era (1980-1999)",
            TRUE ~ "Contemporary Era (2000+)"
          )
        )
      
      return(df)
    })
=======
    plot_ly(counts, labels = ~Medal, values = ~n,
            type   = "pie",
            marker = list(colors = c(GOLD, SILVER, BRONZE),
                          line   = list(color = "white", width = 2)),
            textinfo      = "label+percent",
            hovertemplate = "%{label}: %{value:,}<extra></extra>") %>%
      layout(showlegend = TRUE,
             paper_bgcolor = "rgba(0,0,0,0)",
             plot_bgcolor  = "rgba(0,0,0,0)",
             font = list(family = "Poppins"))
  })
  
  # Medals over time line chart
  output$medals_timeline <- renderPlotly({
    timeline <- medal_data %>%
      count(Year, Medal) %>%
      mutate(Medal = factor(Medal, levels = c("Gold","Silver","Bronze")))
>>>>>>> 8bcc5605cf36db4a090bceae0ed751a1110b2cfa
    
    plot_ly(timeline, x = ~Year, y = ~n, color = ~Medal,
            colors = medal_colors,
            type = "scatter", mode = "lines+markers",
            line    = list(width = 3),
            marker  = list(size = 6),
            hovertemplate = "%{x}: %{y:,} medals<extra>%{fullData.name}</extra>") %>%
      layout(xaxis = list(title = "Year"),
             yaxis = list(title = "Medals Awarded"),
             legend = list(orientation = "h", y = -0.2),
             paper_bgcolor = "rgba(0,0,0,0)",
             plot_bgcolor  = "rgba(0,0,0,0)",
             font = list(family = "Poppins"))
  })
  
  # Top 10 countries bar
  output$top_countries_bar <- renderPlotly({
    top <- medal_data %>%
      count(Team, name = "Medals") %>%
      slice_max(Medals, n = 10) %>%
      arrange(Medals)
    
    plot_ly(top, x = ~Medals, y = ~reorder(Team, Medals),
            type = "bar", orientation = "h",
            marker = list(color = BLUE,
                          line  = list(color = NAVY, width = 1)),
            hovertemplate = "%{y}: %{x:,} medals<extra></extra>") %>%
      layout(xaxis = list(title = "Total Medals"),
             yaxis = list(title = ""),
             paper_bgcolor = "rgba(0,0,0,0)",
             plot_bgcolor  = "rgba(0,0,0,0)",
             font = list(family = "Poppins"))
  })
  
  # Top 10 athletes bar
  output$top_athletes_bar <- renderPlotly({
    top <- medal_data %>%
      count(Name, name = "Medals") %>%
      slice_max(Medals, n = 10) %>%
      arrange(Medals)
    
    plot_ly(top, x = ~Medals, y = ~reorder(Name, Medals),
            type = "bar", orientation = "h",
            marker = list(color = GOLD,
                          line  = list(color = "#B8860B", width = 1)),
            hovertemplate = "%{y}: %{x:,} medals<extra></extra>") %>%
      layout(xaxis = list(title = "Total Medals"),
             yaxis = list(title = ""),
             paper_bgcolor = "rgba(0,0,0,0)",
             plot_bgcolor  = "rgba(0,0,0,0)",
             font = list(family = "Poppins"))
  })
  
  # ============================================================
  # ATHLETES TAB
  # ============================================================
  
  athlete_data <- reactive({
    req(input$athlete_search)
    olympic_data %>% filter(Name == input$athlete_search)
  })
  
  athlete_medals <- reactive({
    athlete_data() %>% filter(!is.na(Medal))
  })
  
  # Profile display
  output$athlete_profile_display <- renderUI({
    d <- athlete_data()
    if (nrow(d) == 0) return(p("No data found."))
    
    age    <- ifelse(all(is.na(d$Age)),    "N/A", paste0(round(mean(d$Age,    na.rm=TRUE)), " yrs"))
    height <- ifelse(all(is.na(d$Height)), "N/A", paste0(round(mean(d$Height, na.rm=TRUE)), " cm"))
    weight <- ifelse(all(is.na(d$Weight)), "N/A", paste0(round(mean(d$Weight, na.rm=TRUE)), " kg"))
    sex    <- ifelse(d$Sex[1] == "M", "Male", "Female")
    sports <- paste(unique(d$Sport), collapse = ", ")
    teams  <- paste(unique(d$Team),  collapse = ", ")
    years  <- paste(range(d$Year), collapse = " – ")
    
    tags$div(
      style = "padding: 10px;",
      tags$h3(style = "color: #0085C7; margin-bottom: 5px;", input$athlete_search),
      tags$p(style = "color: #7F8C8D; font-size: 15px; margin-bottom: 20px;",
             icon("flag"), " ", teams),
      fluidRow(
        column(4, tags$div(style="text-align:center; padding:10px; background:#f8f9fa; border-radius:8px;",
                           tags$small(style="color:#7F8C8D; text-transform:uppercase; font-weight:600;","Sex"),
                           tags$br(), tags$strong(style="font-size:18px;", sex))),
        column(4, tags$div(style="text-align:center; padding:10px; background:#f8f9fa; border-radius:8px;",
                           tags$small(style="color:#7F8C8D; text-transform:uppercase; font-weight:600;","Age"),
                           tags$br(), tags$strong(style="font-size:18px;", age))),
        column(4, tags$div(style="text-align:center; padding:10px; background:#f8f9fa; border-radius:8px;",
                           tags$small(style="color:#7F8C8D; text-transform:uppercase; font-weight:600;","Years Active"),
                           tags$br(), tags$strong(style="font-size:18px;", years)))
      ),
      tags$br(),
      fluidRow(
        column(6, tags$div(style="text-align:center; padding:10px; background:#f8f9fa; border-radius:8px;",
                           tags$small(style="color:#7F8C8D; text-transform:uppercase; font-weight:600;","Height"),
                           tags$br(), tags$strong(style="font-size:18px;", height))),
        column(6, tags$div(style="text-align:center; padding:10px; background:#f8f9fa; border-radius:8px;",
                           tags$small(style="color:#7F8C8D; text-transform:uppercase; font-weight:600;","Weight"),
                           tags$br(), tags$strong(style="font-size:18px;", weight)))
      ),
      tags$br(),
      tags$p(tags$strong("Sports: "), sports)
    )
  })
  
  # Medal summary cards
  output$athlete_total_medals <- renderText({
    nrow(athlete_medals())
  })
  output$athlete_gold <- renderText({
    sum(athlete_medals()$Medal == "Gold",   na.rm = TRUE)
  })
  output$athlete_silver <- renderText({
    sum(athlete_medals()$Medal == "Silver", na.rm = TRUE)
  })
  output$athlete_bronze <- renderText({
    sum(athlete_medals()$Medal == "Bronze", na.rm = TRUE)
  })
  
  # Athlete medal timeline
  output$athlete_timeline <- renderPlotly({
    m <- athlete_medals()
    if (nrow(m) == 0) {
      return(plot_ly() %>%
               layout(title = "No medals recorded for this athlete",
                      paper_bgcolor = "rgba(0,0,0,0)"))
    }
    
    timeline <- m %>%
      count(Year, Medal) %>%
      mutate(Medal = factor(Medal, levels = c("Gold","Silver","Bronze")))
    
    plot_ly(timeline, x = ~Year, y = ~n, color = ~Medal,
            colors  = medal_colors,
            type    = "bar",
            hovertemplate = "%{x}: %{y} medal(s)<extra>%{fullData.name}</extra>") %>%
      layout(barmode = "stack",
             xaxis   = list(title = "Year", dtick = 4),
             yaxis   = list(title = "Medals"),
             paper_bgcolor = "rgba(0,0,0,0)",
             plot_bgcolor  = "rgba(0,0,0,0)",
             font = list(family = "Poppins"))
  })
  
  # Athlete complete record table
  output$athlete_record_table <- DT::renderDataTable({
    d <- athlete_data() %>%
      select(Year, Games, Sport, Event, Medal, Team) %>%
      arrange(Year)
    
    DT::datatable(d,
                  options = list(pageLength = 15, scrollX = TRUE,
                                 dom = "Bfrtip"),
                  rownames = FALSE) %>%
      formatStyle("Medal",
                  backgroundColor = styleEqual(
                    c("Gold","Silver","Bronze"),
                    c(GOLD,  SILVER,  BRONZE)))
  })
  
  # ============================================================
  # COUNTRIES TAB
  # ============================================================
  
  country1_data <- reactive({
    req(input$country1)
    medal_data %>% filter(Team == input$country1)
  })
  
  country2_data <- reactive({
    req(input$country2)
    medal_data %>% filter(Team == input$country2)
  })
  
  # Side-by-side medal bar comparison
  output$country_comparison <- renderPlotly({
    summarise_country <- function(d, name) {
      d %>% count(Medal) %>% mutate(Country = name)
    }
    
    comp <- bind_rows(
      summarise_country(country1_data(), input$country1),
      summarise_country(country2_data(), input$country2)
    ) %>%
      mutate(Medal = factor(Medal, levels = c("Gold","Silver","Bronze")))
    
    plot_ly(comp, x = ~Medal, y = ~n, color = ~Country,
            type = "bar",
            colors = c(BLUE, "#EE334E"),
            hovertemplate = "%{x}: %{y:,} medals<extra>%{fullData.name}</extra>") %>%
      layout(barmode = "group",
             xaxis   = list(title = "Medal Type"),
             yaxis   = list(title = "Count"),
             legend  = list(orientation = "h", y = -0.2),
             paper_bgcolor = "rgba(0,0,0,0)",
             plot_bgcolor  = "rgba(0,0,0,0)",
             font = list(family = "Poppins"))
  })
  
  # Countries performance over time
  output$country_timeline <- renderPlotly({
    c1 <- country1_data() %>% count(Year, name = "Medals") %>% mutate(Country = input$country1)
    c2 <- country2_data() %>% count(Year, name = "Medals") %>% mutate(Country = input$country2)
    
    timeline <- bind_rows(c1, c2)
    
    plot_ly(timeline, x = ~Year, y = ~Medals, color = ~Country,
            type   = "scatter", mode = "lines+markers",
            colors = c(BLUE, "#EE334E"),
            line   = list(width = 3),
            marker = list(size = 7),
            hovertemplate = "%{x}: %{y:,} medals<extra>%{fullData.name}</extra>") %>%
      layout(xaxis = list(title = "Year"),
             yaxis = list(title = "Total Medals"),
             legend = list(orientation = "h", y = -0.2),
             paper_bgcolor = "rgba(0,0,0,0)",
             plot_bgcolor  = "rgba(0,0,0,0)",
             font = list(family = "Poppins"))
  })
  
  # ============================================================
  # SPORTS TAB
  # ============================================================
  
  sport_data <- reactive({
    req(input$sport_select)
    medal_data %>% filter(Sport == input$sport_select)
  })
  
  sport_all <- reactive({
    req(input$sport_select)
    olympic_data %>% filter(Sport == input$sport_select)
  })
  
  output$sport_medals <- renderText({
    format(nrow(sport_data()), big.mark = ",")
  })
  output$sport_athletes <- renderText({
    format(n_distinct(sport_all()$Name), big.mark = ",")
  })
  output$sport_countries <- renderText({
    format(n_distinct(sport_all()$Team), big.mark = ",")
  })
  
  # Top countries for sport
  output$sport_top_countries <- renderPlotly({
    top <- sport_data() %>%
      count(Team, name = "Medals") %>%
      slice_max(Medals, n = 10) %>%
      arrange(Medals)
    
    plot_ly(top, x = ~Medals, y = ~reorder(Team, Medals),
            type = "bar", orientation = "h",
            marker = list(color = BLUE,
                          line  = list(color = NAVY, width = 1)),
            hovertemplate = "%{y}: %{x:,} medals<extra></extra>") %>%
      layout(xaxis = list(title = "Medals"),
             yaxis = list(title = ""),
             paper_bgcolor = "rgba(0,0,0,0)",
             plot_bgcolor  = "rgba(0,0,0,0)",
             font = list(family = "Poppins"))
  })
  
  # Top athletes for sport
  output$sport_top_athletes <- renderPlotly({
    top <- sport_data() %>%
      count(Name, name = "Medals") %>%
      slice_max(Medals, n = 10) %>%
      arrange(Medals)
    
    plot_ly(top, x = ~Medals, y = ~reorder(Name, Medals),
            type = "bar", orientation = "h",
            marker = list(color = GOLD,
                          line  = list(color = "#B8860B", width = 1)),
            hovertemplate = "%{y}: %{x:,} medals<extra></extra>") %>%
      layout(xaxis = list(title = "Medals"),
             yaxis = list(title = ""),
             paper_bgcolor = "rgba(0,0,0,0)",
             plot_bgcolor  = "rgba(0,0,0,0)",
             font = list(family = "Poppins"))
  })
  
  # ============================================================
  # DATA EXPLORER TAB
  # ============================================================
  
  explorer_filtered <- reactive({
    d <- olympic_data %>%
      select(Year, Name, Sex, Age, Team, NOC, Games, Season, Sport, Event, Medal)
    
    if (input$explorer_sport   != "all") d <- d %>% filter(Sport == input$explorer_sport)
    if (input$explorer_country != "all") d <- d %>% filter(Team  == input$explorer_country)
    if (input$explorer_year    != "all") d <- d %>% filter(Year  == as.integer(input$explorer_year))
    if (input$explorer_medal   != "all") d <- d %>% filter(Medal == input$explorer_medal)
    
    d
  })
  
  output$explorer_table <- DT::renderDataTable({
    DT::datatable(explorer_filtered(),
                  options = list(
                    pageLength = 20,
                    scrollX    = TRUE,
                    dom        = "Bfrtip",
                    buttons    = c("copy","csv","excel")
                  ),
                  filter   = "top",
                  rownames = FALSE) %>%
      formatStyle("Medal",
                  backgroundColor = styleEqual(
                    c("Gold","Silver","Bronze"),
                    c(GOLD,  SILVER,  BRONZE)))
  })
  
  # Download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("olympic_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(explorer_filtered(), file, row.names = FALSE)
    }
  )
}