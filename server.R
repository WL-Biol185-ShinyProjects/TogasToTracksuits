Olympic_Dataset = read.csv("athlete_events-Olympic Dataset.csv")

library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(DT)
library(leaflet)


function(input, output, session) {
  
  # ===========================================================================
  # DATA LOADING
  # ===========================================================================
  
  olympic_data <- read.csv("athlete_events-Olympic Dataset.csv", stringsAsFactors = FALSE)
  
  olympic_data <- olympic_data %>%
    mutate(
      Medal = ifelse(is.na(Medal), "None", Medal),
      Has_Medal = (Medal != "None"),
      Age    = suppressWarnings(as.numeric(Age)),
      Height = suppressWarnings(as.numeric(Height)),
      Weight = suppressWarnings(as.numeric(Weight)),
      Year   = as.integer(Year)
    )
  
  medal_data <- olympic_data %>% filter(Has_Medal == TRUE)
  

  
  # ===========================================================================
  # DASHBOARD TAB
  # ===========================================================================
  
  output$age_distribution <- renderPlotly({
    df <- medal_data %>%
      filter(Medal %in% c("Gold", "Silver", "Bronze"), !is.na(Age)) %>%
      mutate(AgeGroup = cut(Age, breaks = seq(10, 80, by = 5),
                            labels = paste(seq(10, 75, by = 5), seq(14, 79, by = 5), sep = "-"))) %>%
      count(AgeGroup, Medal) %>%
      filter(!is.na(AgeGroup))
    
    colors <- c("Gold" = "#FFD700", "Silver" = "#A8A9AD", "Bronze" = "#CD7F32")
    
    plot_ly(df, x = ~AgeGroup, y = ~n, color = ~Medal,
            colors = colors,
            type = "bar",
            hoverinfo = "x+y+name") %>%
      layout(
        barmode = "group",
        xaxis = list(title = "Age Group", tickangle = -45),
        yaxis = list(title = "Number of Athletes"),
        paper_bgcolor = "white",
        plot_bgcolor  = "white",
        legend = list(title = list(text = "Medal"))
      )
  })
  
  output$gender_participation <- renderPlotly({
    df <- olympic_data %>%
      count(Year, Sex) %>%
      mutate(Sex = ifelse(Sex == "M", "Male", "Female"))
    
    plot_ly(df, x = ~Year, y = ~n, color = ~Sex,
            colors = c("Male" = "#0085C7", "Female" = "#EE334E"),
            type = "scatter", mode = "lines+markers",
            hoverinfo = "x+y+name") %>%
      layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = "Number of Athletes"),
        paper_bgcolor = "white",
        plot_bgcolor  = "white",
        legend = list(title = list(text = "Gender"))
      )
  })
  
  output$top_countries_bar <- renderPlotly({
    top10_noc <- medal_data %>%
      filter(Medal %in% c("Gold", "Silver", "Bronze")) %>%
      count(NOC) %>%
      arrange(desc(n)) %>%
      head(10) %>%
      pull(NOC)
    
    top_countries <- medal_data %>%
      filter(Medal %in% c("Gold", "Silver", "Bronze"), NOC %in% top10_noc) %>%
      count(NOC, Medal) %>%
      mutate(NOC = factor(NOC, levels = rev(top10_noc)))
    
    colors <- c("Gold" = "#FFD700", "Silver" = "#A8A9AD", "Bronze" = "#CD7F32")
    
    plot_ly(top_countries, x = ~n, y = ~NOC, color = ~Medal,
            colors = colors,
            type = "bar", orientation = "h",
            hoverinfo = "x+y+name") %>%
      layout(
        barmode = "stack",
        xaxis = list(title = "Number of Medals"),
        yaxis = list(title = ""),
        paper_bgcolor = "white",
        plot_bgcolor  = "white",
        legend = list(title = list(text = "Medal"))
      )
  })
  
  output$top_athletes_bar <- renderPlotly({
    top10_names <- medal_data %>%
      filter(Medal %in% c("Gold", "Silver", "Bronze")) %>%
      count(Name) %>%
      arrange(desc(n)) %>%
      head(10) %>%
      pull(Name)
    
    top10_athletes <- medal_data %>%
      filter(Medal %in% c("Gold", "Silver", "Bronze"), Name %in% top10_names) %>%
      count(Name, Medal) %>%
      mutate(Name = factor(Name, levels = rev(top10_names)))
    
    colors <- c("Gold" = "#FFD700", "Silver" = "#A8A9AD", "Bronze" = "#CD7F32")
    
    plot_ly(top10_athletes, x = ~n, y = ~Name, color = ~Medal,
            colors = colors,
            type = "bar", orientation = "h",
            hoverinfo = "x+y+name") %>%
      layout(
        barmode = "stack",
        xaxis = list(title = "Number of Medals"),
        yaxis = list(title = ""),
        paper_bgcolor = "white",
        plot_bgcolor  = "white",
        legend = list(title = list(text = "Medal"))
      )
  })
  
  # ===========================================================================
  # ATHLETES TAB
  # ===========================================================================
  
  observe({
    athletes <- sort(unique(medal_data$Name))
    updateSelectInput(session, "athlete_search",
                      choices = athletes,
                      selected = athletes[1])
  })
  
  athlete_df <- reactive({
    req(input$athlete_search)
    olympic_data %>% filter(Name == input$athlete_search)
  })
  
  athlete_medals <- reactive({
    athlete_df() %>% filter(Has_Medal == TRUE)
  })
  
  output$athlete_profile_display <- renderUI({
    df <- athlete_df()
    if (nrow(df) == 0) return(p("No data found for this athlete."))
    
    info <- df %>% arrange(Year) %>% slice(1)
    
    tagList(
      fluidRow(
        column(6,
               tags$table(
                 style = "width: 100%; font-size: 15px;",
                 tags$tr(tags$td(strong("Name:")),   tags$td(info$Name)),
                 tags$tr(tags$td(strong("Sex:")),    tags$td(info$Sex)),
                 tags$tr(tags$td(strong("Age:")),    tags$td(ifelse(is.na(info$Age),    "N/A", info$Age))),
                 tags$tr(tags$td(strong("Height:")), tags$td(ifelse(is.na(info$Height), "N/A", paste0(info$Height, " cm")))),
                 tags$tr(tags$td(strong("Weight:")), tags$td(ifelse(is.na(info$Weight), "N/A", paste0(info$Weight, " kg"))))
               )
        ),
        column(6,
               tags$table(
                 style = "width: 100%; font-size: 15px;",
                 tags$tr(tags$td(strong("Country:")),  tags$td(info$Team)),
                 tags$tr(tags$td(strong("NOC:")),      tags$td(info$NOC)),
                 tags$tr(tags$td(strong("Sport(s):")), tags$td(paste(unique(df$Sport), collapse = ", "))),
                 tags$tr(tags$td(strong("Games:")),    tags$td(paste(sort(unique(df$Year)), collapse = ", "))),
                 tags$tr(tags$td(strong("Events:")),   tags$td(length(unique(df$Event))))
               )
        )
      )
    )
  })
  
  output$athlete_total_medals <- renderText({ nrow(athlete_medals()) })
  output$athlete_gold         <- renderText({ sum(athlete_medals()$Medal == "Gold")   })
  output$athlete_silver       <- renderText({ sum(athlete_medals()$Medal == "Silver") })
  output$athlete_bronze       <- renderText({ sum(athlete_medals()$Medal == "Bronze") })
  
  output$athlete_timeline <- renderPlotly({
    df <- athlete_medals()
    
    if (nrow(df) == 0) {
      return(plot_ly() %>% layout(title = "No medals found for this athlete"))
    }
    
    timeline <- df %>%
      count(Year, Medal) %>%
      mutate(Medal = factor(Medal, levels = c("Gold", "Silver", "Bronze")))
    
    colors <- c("Gold" = "#FFD700", "Silver" = "#A8A9AD", "Bronze" = "#CD7F32")
    
    plot_ly(timeline, x = ~Year, y = ~n, color = ~Medal,
            colors = colors,
            type = "bar",
            hoverinfo = "x+y+name") %>%
      layout(
        barmode = "stack",
        xaxis = list(title = "Year", dtick = 4),
        yaxis = list(title = "Medals Won"),
        paper_bgcolor = "white",
        plot_bgcolor  = "white"
      )
  })
  
  output$athlete_record_table <- DT::renderDataTable({
    df <- athlete_df() %>%
      select(Year, Season, City, Sport, Event, Team, NOC, Medal) %>%
      arrange(desc(Year))
    
    DT::datatable(df,
                  options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE)
  })
  
  # ===========================================================================
  # COUNTRIES TAB
  # ===========================================================================
  
  output$country_comparison <- renderPlotly({
    req(input$country1, input$country2)
    
    df <- medal_data %>%
      filter(Team %in% c(input$country1, input$country2),
             Medal %in% c("Gold", "Silver", "Bronze")) %>%
      count(Team, Medal)
    
    if (nrow(df) == 0) {
      return(plot_ly() %>% layout(title = "No medal data found for selected countries"))
    }
    
    colors <- c("Gold" = "#FFD700", "Silver" = "#A8A9AD", "Bronze" = "#CD7F32")
    
    plot_ly(df, x = ~Team, y = ~n, color = ~Medal,
            colors = colors,
            type = "bar",
            hoverinfo = "x+y+name") %>%
      layout(
        barmode = "group",
        xaxis = list(title = ""),
        yaxis = list(title = "Number of Medals"),
        paper_bgcolor = "white",
        plot_bgcolor  = "white",
        legend = list(title = list(text = "Medal"))
      )
  })
  
  output$country_timeline <- renderPlotly({
    req(input$country1, input$country2)
    
    df <- medal_data %>%
      filter(Team %in% c(input$country1, input$country2),
             Medal %in% c("Gold", "Silver", "Bronze")) %>%
      count(Year, Team)
    
    if (nrow(df) == 0) {
      return(plot_ly() %>% layout(title = "No medal data found for selected countries"))
    }
    
    plot_ly(df, x = ~Year, y = ~n, color = ~Team,
            type = "scatter", mode = "lines+markers",
            hoverinfo = "x+y+name") %>%
      layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = "Medals Won"),
        paper_bgcolor = "white",
        plot_bgcolor  = "white",
        legend = list(title = list(text = "Country"))
      )
  })
  
  # ===========================================================================
  # SPORTS TAB
  # ===========================================================================
  
  observe({
    sports <- sort(unique(olympic_data$Sport))
    updateSelectInput(session, "sport_select",
                      choices = sports,
                      selected = sports[1])
  })
  
  sport_df <- reactive({
    req(input$sport_select)
    olympic_data %>% filter(Sport == input$sport_select)
  })
  
  sport_medals_df <- reactive({
    sport_df() %>% filter(Has_Medal == TRUE)
  })
  
  output$sport_medals    <- renderText({ formatC(nrow(sport_medals_df()),         format = "d", big.mark = ",") })
  output$sport_athletes  <- renderText({ formatC(length(unique(sport_df()$Name)), format = "d", big.mark = ",") })
  output$sport_countries <- renderText({ formatC(length(unique(sport_df()$NOC)),  format = "d", big.mark = ",") })
  
  output$sport_top_countries <- renderPlotly({
    df <- sport_medals_df()
    
    if (nrow(df) == 0) {
      return(plot_ly() %>% layout(title = "No medal data for this sport"))
    }
    
    top10_noc <- df %>%
      filter(Medal %in% c("Gold", "Silver", "Bronze")) %>%
      count(NOC) %>%
      arrange(desc(n)) %>%
      head(10) %>%
      pull(NOC)
    
    plot_data <- df %>%
      filter(Medal %in% c("Gold", "Silver", "Bronze"), NOC %in% top10_noc) %>%
      count(NOC, Medal) %>%
      mutate(NOC = factor(NOC, levels = rev(top10_noc)))
    
    colors <- c("Gold" = "#FFD700", "Silver" = "#A8A9AD", "Bronze" = "#CD7F32")
    
    plot_ly(plot_data, x = ~n, y = ~NOC, color = ~Medal,
            colors = colors,
            type = "bar", orientation = "h",
            hoverinfo = "x+y+name") %>%
      layout(
        barmode = "stack",
        xaxis = list(title = "Medals"),
        yaxis = list(title = ""),
        paper_bgcolor = "white",
        plot_bgcolor  = "white"
      )
  })
  
  output$sport_top_athletes <- renderPlotly({
    df <- sport_medals_df()
    
    if (nrow(df) == 0) {
      return(plot_ly() %>% layout(title = "No medal data for this sport"))
    }
    
    top10_names <- df %>%
      filter(Medal %in% c("Gold", "Silver", "Bronze")) %>%
      count(Name) %>%
      arrange(desc(n)) %>%
      head(10) %>%
      pull(Name)
    
    plot_data <- df %>%
      filter(Medal %in% c("Gold", "Silver", "Bronze"), Name %in% top10_names) %>%
      count(Name, Medal) %>%
      mutate(Name = factor(Name, levels = rev(top10_names)))
    
    colors <- c("Gold" = "#FFD700", "Silver" = "#A8A9AD", "Bronze" = "#CD7F32")
    
    plot_ly(plot_data, x = ~n, y = ~Name, color = ~Medal,
            colors = colors,
            type = "bar", orientation = "h",
            hoverinfo = "x+y+name") %>%
      layout(
        barmode = "stack",
        xaxis = list(title = "Medals"),
        yaxis = list(title = ""),
        paper_bgcolor = "white",
        plot_bgcolor  = "white"
      )
  })
}