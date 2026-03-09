library(shiny)
  
function(input, output, session) {
  
  # ========== LOAD AND PROCESS DATA ==========
  olympic_data <- reactive({
    df <- read.csv("athlete_events-Olympic Dataset.csv", stringsAsFactors = FALSE)
    
    df <- df %>%
      mutate(
        Medal = ifelse(is.na(Medal), "None", Medal),
        Has_Medal = (Medal != "None"),
        Age = suppressWarnings(as.numeric(Age)),
        Height = suppressWarnings(as.numeric(Height)),
        Weight = suppressWarnings(as.numeric(Weight)),
        Year = as.integer(Year)
      )
    
    return(df)
  })
  
  # Only medal-winning rows
  medal_data <- reactive({
    olympic_data() %>% filter(Has_Medal == TRUE)
  })
  
  
  # ========== DASHBOARD TAB ==========
  
  # Medal Pie Chart
  output$medal_pie_chart <- renderPlotly({
    df <- medal_data()
    
    medal_counts <- df %>%
      count(Medal) %>%
      filter(Medal %in% c("Gold", "Silver", "Bronze"))
    
    colors <- c("Gold" = "#FFD700", "Silver" = "#C0C0C0", "Bronze" = "#CD7F32")
    
    plot_ly(
      medal_counts,
      labels = ~Medal,
      values = ~n,
      type = "pie",
      marker = list(colors = unname(colors[medal_counts$Medal])),
      textinfo = "label+percent",
      hoverinfo = "label+value+percent"
    ) %>%
      layout(
        showlegend = TRUE,
        paper_bgcolor = "white",
        plot_bgcolor = "white"
      )
  })
  
  # Medals Over Time
  output$medals_timeline <- renderPlotly({
    df <- medal_data()
    
    timeline <- df %>%
      filter(Medal %in% c("Gold", "Silver", "Bronze")) %>%
      count(Year, Medal)
    
    colors <- c("Gold" = "#FFD700", "Silver" = "#A8A9AD", "Bronze" = "#CD7F32")
    
    plot_ly(timeline, x = ~Year, y = ~n, color = ~Medal,
            colors = colors,
            type = "scatter", mode = "lines+markers",
            hoverinfo = "x+y+name") %>%
      layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = "Number of Medals"),
        paper_bgcolor = "white",
        plot_bgcolor = "white",
        legend = list(title = list(text = "Medal"))
      )
  })
  
  # Top 10 Countries Bar Chart
  output$top_countries_bar <- renderPlotly({
    df <- medal_data()
    
    top_countries <- df %>%
      filter(Medal %in% c("Gold", "Silver", "Bronze")) %>%
      count(NOC, Medal) %>%
      group_by(NOC) %>%
      mutate(Total = sum(n)) %>%
      ungroup() %>%
      filter(NOC %in% (df %>% count(NOC) %>% arrange(desc(n)) %>% head(10) %>% pull(NOC)))
    
    top10_noc <- df %>%
      filter(Medal %in% c("Gold", "Silver", "Bronze")) %>%
      count(NOC) %>%
      arrange(desc(n)) %>%
      head(10) %>%
      pull(NOC)
    
    top_countries <- df %>%
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
        plot_bgcolor = "white",
        legend = list(title = list(text = "Medal"))
      )
  })
  
  # Top 10 Athletes Bar Chart
  output$top_athletes_bar <- renderPlotly({
    df <- medal_data()
    
    top10_athletes <- df %>%
      filter(Medal %in% c("Gold", "Silver", "Bronze")) %>%
      count(Name, Medal) %>%
      group_by(Name) %>%
      mutate(Total = sum(n)) %>%
      ungroup()
    
    top10_names <- top10_athletes %>%
      distinct(Name, Total) %>%
      arrange(desc(Total)) %>%
      head(10) %>%
      pull(Name)
    
    top10_athletes <- top10_athletes %>%
      filter(Name %in% top10_names) %>%
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
        plot_bgcolor = "white",
        legend = list(title = list(text = "Medal"))
      )
  })
  
  
  # ========== ATHLETES TAB ==========
  
  # Populate athlete dropdown
  observe({
    df <- medal_data()
    athletes <- sort(unique(df$Name))
    updateSelectInput(session, "athlete_search",
                      choices = athletes,
                      selected = athletes[1])
  })
  
  # Selected athlete's data
  athlete_df <- reactive({
    req(input$athlete_search)
    olympic_data() %>% filter(Name == input$athlete_search)
  })
  
  athlete_medals <- reactive({
    athlete_df() %>% filter(Has_Medal == TRUE)
  })
  
  # Athlete profile card
  output$athlete_profile_display <- renderUI({
    df <- athlete_df()
    if (nrow(df) == 0) return(p("No data found for this athlete."))
    
    # Use first row for bio info
    info <- df %>% arrange(Year) %>% slice(1)
    
    tagList(
      fluidRow(
        column(6,
               tags$table(
                 style = "width: 100%; font-size: 15px;",
                 tags$tr(tags$td(strong("Name:")), tags$td(info$Name)),
                 tags$tr(tags$td(strong("Sex:")),  tags$td(info$Sex)),
                 tags$tr(tags$td(strong("Age:")),  tags$td(ifelse(is.na(info$Age), "N/A", info$Age))),
                 tags$tr(tags$td(strong("Height:")), tags$td(ifelse(is.na(info$Height), "N/A", paste0(info$Height, " cm")))),
                 tags$tr(tags$td(strong("Weight:")), tags$td(ifelse(is.na(info$Weight), "N/A", paste0(info$Weight, " kg"))))
               )
        ),
        column(6,
               tags$table(
                 style = "width: 100%; font-size: 15px;",
                 tags$tr(tags$td(strong("Country:")),   tags$td(info$Team)),
                 tags$tr(tags$td(strong("NOC:")),       tags$td(info$NOC)),
                 tags$tr(tags$td(strong("Sport(s):")),  tags$td(paste(unique(df$Sport), collapse = ", "))),
                 tags$tr(tags$td(strong("Games:")),     tags$td(paste(sort(unique(df$Year)), collapse = ", "))),
                 tags$tr(tags$td(strong("Events:")),    tags$td(length(unique(df$Event))))
               )
        )
      )
    )
  })
  
  # Athlete medal counts
  output$athlete_total_medals <- renderText({
    nrow(athlete_medals())
  })
  
  output$athlete_gold <- renderText({
    sum(athlete_medals()$Medal == "Gold")
  })
  
  output$athlete_silver <- renderText({
    sum(athlete_medals()$Medal == "Silver")
  })
  
  output$athlete_bronze <- renderText({
    sum(athlete_medals()$Medal == "Bronze")
  })
  
  # Athlete medal timeline
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
        plot_bgcolor = "white"
      )
  })
  
  # Athlete full record table
  output$athlete_record_table <- DT::renderDataTable({
    df <- athlete_df() %>%
      select(Year, Season, City, Sport, Event, Team, NOC, Medal) %>%
      arrange(desc(Year))
    
    DT::datatable(df,
                  options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE)
  })
  
  
  # ========== COUNTRIES TAB ==========
  
  country_medals <- reactive({
    df <- medal_data()
    
    df %>%
      filter(Medal %in% c("Gold", "Silver", "Bronze")) %>%
      group_by(Team, Medal) %>%
      summarise(Count = n(), .groups = "drop")
  })
  
  # Country comparison bar chart
  output$country_comparison <- renderPlotly({
    req(input$country1, input$country2)
    
    df <- medal_data() %>%
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
        plot_bgcolor = "white",
        legend = list(title = list(text = "Medal"))
      )
  })
  
  # Country performance over time
  output$country_timeline <- renderPlotly({
    req(input$country1, input$country2)
    
    df <- medal_data() %>%
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
        plot_bgcolor = "white",
        legend = list(title = list(text = "Country"))
      )
  })
  
  
  # ========== SPORTS TAB ==========
  
  # Populate sport dropdown
  observe({
    df <- olympic_data()
    sports <- sort(unique(df$Sport))
    updateSelectInput(session, "sport_select",
                      choices = sports,
                      selected = sports[1])
  })
  
  sport_df <- reactive({
    req(input$sport_select)
    olympic_data() %>% filter(Sport == input$sport_select)
  })
  
  sport_medals_df <- reactive({
    sport_df() %>% filter(Has_Medal == TRUE)
  })
  
  # Sport summary cards
  output$sport_medals <- renderText({
    formatC(nrow(sport_medals_df()), format = "d", big.mark = ",")
  })
  
  output$sport_athletes <- renderText({
    formatC(length(unique(sport_df()$Name)), format = "d", big.mark = ",")
  })
  
  output$sport_countries <- renderText({
    formatC(length(unique(sport_df()$NOC)), format = "d", big.mark = ",")
  })
  
  # Sport top countries
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
        title = paste("Top Countries in", input$sport_select),
        xaxis = list(title = "Medals"),
        yaxis = list(title = ""),
        paper_bgcolor = "white",
        plot_bgcolor = "white"
      )
  })
  
  # Sport top athletes
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
        title = paste("Top Athletes in", input$sport_select),
        xaxis = list(title = "Medals"),
        yaxis = list(title = ""),
        paper_bgcolor = "white",
        plot_bgcolor = "white"
      )
  })
  
  
  # ========== DATA EXPLORER TAB ==========
  
  # Populate explorer filters
  observe({
    df <- olympic_data()
    
    sports  <- sort(unique(df$Sport))
    countries <- sort(unique(df$Team))
    years   <- sort(unique(df$Year))
    
    updateSelectInput(session, "explorer_sport",
                      choices = c("All" = "all", setNames(sports, sports)),
                      selected = "all")
    
    updateSelectInput(session, "explorer_country",
                      choices = c("All" = "all", setNames(countries, countries)),
                      selected = "all")
    
    updateSelectInput(session, "explorer_year",
                      choices = c("All" = "all", setNames(years, years)),
                      selected = "all")
  })
  
  # Filtered explorer data
  explorer_filtered <- reactive({
    df <- olympic_data()
    
    if (input$explorer_sport != "all") {
      df <- df %>% filter(Sport == input$explorer_sport)
    }
    
    if (input$explorer_country != "all") {
      df <- df %>% filter(Team == input$explorer_country)
    }
    
    if (input$explorer_year != "all") {
      df <- df %>% filter(Year == as.integer(input$explorer_year))
    }
    
    if (input$explorer_medal != "all") {
      df <- df %>% filter(Medal == input$explorer_medal)
    }
    
    df %>%
      select(Name, Sex, Age, Team, NOC, Year, Season, City, Sport, Event, Medal) %>%
      arrange(desc(Year), Name)
  })
  
  # Explorer table
  output$explorer_table <- DT::renderDataTable({
    DT::datatable(
      explorer_filtered(),
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        dom = "frtip"
      ),
      rownames = FALSE
    )
  })
  
  # Download filtered data
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("olympic_data_filtered_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(explorer_filtered(), file, row.names = FALSE)
    }
  )
}