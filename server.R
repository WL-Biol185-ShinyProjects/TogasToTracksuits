library(shiny)
  
function(input, output, session) {
    
    # ========== LOAD AND PROCESS DATA ==========
    olympic_data <- reactive({
      df <- read_excel("athlete_events-Olympic_Dataset.xlsx")
      
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
    
    # ========== DASHBOARD TAB ==========
    
    # Medal Distribution Pie Chart
    output$medal_pie_chart <- renderPlotly({
      df <- olympic_data()
      
      medal_counts <- df %>%
        filter(!is.na(Medal)) %>%
        group_by(Medal) %>%
        summarise(count = n(), .groups = "drop")
      
      plot_ly(medal_counts, 
              labels = ~Medal, 
              values = ~count,
              type = 'pie',
              marker = list(colors = c('#CD7F32', '#FFD700', '#C0C0C0')),
              textinfo = 'label+percent',
              hoverinfo = 'text',
              text = ~paste0(Medal, ': ', formatC(count, format="d", big.mark=","))) %>%
        layout(title = "",
               showlegend = TRUE,
               paper_bgcolor = 'rgba(0,0,0,0)',
               plot_bgcolor = 'rgba(0,0,0,0)')
    })
    
    # Medals Over Time
    output$medals_timeline <- renderPlotly({
      df <- olympic_data()
      
      timeline <- df %>%
        filter(!is.na(Medal)) %>%
        group_by(Year) %>%
        summarise(total_medals = n(), .groups = "drop")
      
      plot_ly(timeline, 
              x = ~Year, 
              y = ~total_medals,
              type = 'scatter',
              mode = 'lines+markers',
              line = list(color = '#0085C7', width = 3),
              marker = list(color = '#FFD700', size = 6),
              hovertemplate = paste0(
                '<b>Year:</b> %{x}<br>',
                '<b>Medals:</b> %{y:,}<br>',
                '<extra></extra>'
              )) %>%
        layout(
          xaxis = list(title = "Year"),
          yaxis = list(title = "Total Medals Awarded"),
          hovermode = 'closest',
          paper_bgcolor = 'rgba(0,0,0,0)',
          plot_bgcolor = 'rgba(0,0,0,0)'
        )
    })
    
    # Top 10 Countries
    output$top_countries_bar <- renderPlotly({
      df <- olympic_data()
      
      top_countries <- df %>%
        filter(!is.na(Medal)) %>%
        group_by(Team) %>%
        summarise(total_medals = n(), .groups = "drop") %>%
        arrange(desc(total_medals)) %>%
        head(10)
      
      plot_ly(top_countries,
              x = ~reorder(Team, total_medals),
              y = ~total_medals,
              type = 'bar',
              marker = list(color = '#0085C7',
                            line = list(color = '#FFD700', width = 2)),
              text = ~formatC(total_medals, format="d", big.mark=","),
              textposition = 'outside',
              hovertemplate = paste0(
                '<b>%{x}</b><br>',
                'Medals: %{y:,}<br>',
                '<extra></extra>'
              )) %>%
        layout(
          xaxis = list(title = "Country", tickangle = -45),
          yaxis = list(title = "Total Medals"),
          margin = list(b = 100),
          paper_bgcolor = 'rgba(0,0,0,0)',
          plot_bgcolor = 'rgba(0,0,0,0)'
        )
    })
    
    # Top 10 Athletes
    output$top_athletes_bar <- renderPlotly({
      df <- olympic_data()
      
      top_athletes <- df %>%
        filter(!is.na(Medal)) %>%
        group_by(Name) %>%
        summarise(total_medals = n(), .groups = "drop") %>%
        arrange(desc(total_medals)) %>%
        head(10)
      
      plot_ly(top_athletes,
              x = ~reorder(Name, total_medals),
              y = ~total_medals,
              type = 'bar',
              marker = list(color = '#FFD700',
                            line = list(color = '#0085C7', width = 2)),
              text = ~total_medals,
              textposition = 'outside',
              hovertemplate = paste0(
                '<b>%{x}</b><br>',
                'Medals: %{y}<br>',
                '<extra></extra>'
              )) %>%
        layout(
          xaxis = list(title = "Athlete", tickangle = -45),
          yaxis = list(title = "Total Medals"),
          margin = list(b = 150),
          paper_bgcolor = 'rgba(0,0,0,0)',
          plot_bgcolor = 'rgba(0,0,0,0)'
        )
    })
    
    # ========== ATHLETES TAB ==========
    
    # Populate athlete dropdown
    observe({
      df <- olympic_data()
      
      athletes <- df %>%
        filter(!is.na(Medal)) %>%
        distinct(Name) %>%
        arrange(Name) %>%
        pull(Name)
      
      updateSelectInput(session, "athlete_search",
                        choices = athletes,
                        selected = "Michael Fred Phelps, II")
    })
    
    # Athlete Profile Display
    output$athlete_profile_display <- renderUI({
      req(input$athlete_search)
      df <- olympic_data()
      
      athlete_data <- df %>%
        filter(Name == input$athlete_search) %>%
        slice(1)
      
      if(nrow(athlete_data) == 0) {
        return(p("No data available for this athlete."))
      }
      
      tagList(
        fluidRow(
          column(6,
                 p(strong("Name:"), athlete_data$Name),
                 p(strong("Sex:"), athlete_data$Sex),
                 p(strong("Age (at first Olympics):"), ifelse(is.na(athlete_data$Age), "Unknown", athlete_data$Age))
          ),
          column(6,
                 p(strong("Height:"), ifelse(is.na(athlete_data$Height), "Unknown", paste0(athlete_data$Height, " cm"))),
                 p(strong("Weight:"), ifelse(is.na(athlete_data$Weight), "Unknown", paste0(athlete_data$Weight, " kg"))),
                 p(strong("Team:"), athlete_data$Team)
          )
        )
      )
    })
    
    # Athlete Medal Counts
    output$athlete_total_medals <- renderText({
      req(input$athlete_search)
      df <- olympic_data()
      
      total <- df %>%
        filter(Name == input$athlete_search, !is.na(Medal)) %>%
        nrow()
      
      formatC(total, format="d", big.mark=",")
    })
    
    output$athlete_gold <- renderText({
      req(input$athlete_search)
      df <- olympic_data()
      
      gold <- df %>%
        filter(Name == input$athlete_search, Medal == "Gold") %>%
        nrow()
      
      formatC(gold, format="d", big.mark=",")
    })
    
    output$athlete_silver <- renderText({
      req(input$athlete_search)
      df <- olympic_data()
      
      silver <- df %>%
        filter(Name == input$athlete_search, Medal == "Silver") %>%
        nrow()
      
      formatC(silver, format="d", big.mark=",")
    })
    
    output$athlete_bronze <- renderText({
      req(input$athlete_search)
      df <- olympic_data()
      
      bronze <- df %>%
        filter(Name == input$athlete_search, Medal == "Bronze") %>%
        nrow()
      
      formatC(bronze, format="d", big.mark=",")
    })
    
    # Athlete Timeline
    output$athlete_timeline <- renderPlotly({
      req(input$athlete_search)
      df <- olympic_data()
      
      medals <- df %>%
        filter(Name == input$athlete_search, !is.na(Medal)) %>%
        group_by(Year, Medal) %>%
        summarise(count = n(), .groups = "drop")
      
      if(nrow(medals) == 0) {
        return(plot_ly() %>% layout(title = "No medal data available"))
      }
      
      plot_ly(medals, 
              x = ~Year, 
              y = ~count, 
              color = ~Medal,
              colors = c("Gold" = "#FFD700", "Silver" = "#C0C0C0", "Bronze" = "#CD7F32"),
              type = 'bar',
              text = ~count,
              textposition = 'outside') %>%
        layout(
          barmode = 'stack',
          xaxis = list(title = "Year"),
          yaxis = list(title = "Medals Won"),
          hovermode = 'closest',
          paper_bgcolor = 'rgba(0,0,0,0)',
          plot_bgcolor = 'rgba(0,0,0,0)'
        )
    })
    
    # Athlete Record Table
    output$athlete_record_table <- DT::renderDataTable({
      req(input$athlete_search)
      df <- olympic_data()
      
      records <- df %>%
        filter(Name == input$athlete_search) %>%
        select(Year, Season, City, Sport, Event, Medal) %>%
        arrange(desc(Year))
      
      DT::datatable(
        records,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          searching = TRUE,
          ordering = TRUE
        ),
        rownames = FALSE
      )
    })
    
    # ========== COUNTRIES TAB ==========
    
    # Country Comparison Bar Chart
    output$country_comparison <- renderPlotly({
      req(input$country1, input$country2)
      df <- olympic_data()
      
      country1_data <- df %>%
        filter(Team == input$country1, !is.na(Medal)) %>%
        group_by(Medal) %>%
        summarise(count = n(), .groups = "drop") %>%
        mutate(Country = input$country1)
      
      country2_data <- df %>%
        filter(Team == input$country2, !is.na(Medal)) %>%
        group_by(Medal) %>%
        summarise(count = n(), .groups = "drop") %>%
        mutate(Country = input$country2)
      
      comparison <- bind_rows(country1_data, country2_data)
      
      if(nrow(comparison) == 0) {
        return(plot_ly() %>% layout(title = "No data for selected countries"))
      }
      
      plot_ly(comparison, 
              x = ~Medal, 
              y = ~count, 
              color = ~Country,
              type = 'bar',
              colors = c('#0085C7', '#FFD700'),
              text = ~count,
              textposition = 'outside') %>%
        layout(
          barmode = 'group',
          xaxis = list(title = "Medal Type"),
          yaxis = list(title = "Count"),
          hovermode = 'closest',
          paper_bgcolor = 'rgba(0,0,0,0)',
          plot_bgcolor = 'rgba(0,0,0,0)'
        )
    })
    
    # Country Timeline
    output$country_timeline <- renderPlotly({
      req(input$country1, input$country2)
      df <- olympic_data()
      
      country1_timeline <- df %>%
        filter(Team == input$country1, !is.na(Medal)) %>%
        group_by(Year) %>%
        summarise(medals = n(), .groups = "drop") %>%
        mutate(Country = input$country1)
      
      country2_timeline <- df %>%
        filter(Team == input$country2, !is.na(Medal)) %>%
        group_by(Year) %>%
        summarise(medals = n(), .groups = "drop") %>%
        mutate(Country = input$country2)
      
      timeline <- bind_rows(country1_timeline, country2_timeline)
      
      if(nrow(timeline) == 0) {
        return(plot_ly() %>% layout(title = "No timeline data available"))
      }
      
      plot_ly(timeline, 
              x = ~Year, 
              y = ~medals, 
              color = ~Country,
              colors = c('#0085C7', '#FFD700'),
              type = 'scatter',
              mode = 'lines+markers',
              line = list(width = 3),
              marker = list(size = 6)) %>%
        layout(
          xaxis = list(title = "Year"),
          yaxis = list(title = "Medals Won"),
          hovermode = 'closest',
          paper_bgcolor = 'rgba(0,0,0,0)',
          plot_bgcolor = 'rgba(0,0,0,0)'
        )
    })
    
    # ========== SPORTS TAB ==========
    
    # Populate sport dropdown
    observe({
      df <- olympic_data()
      
      sports <- df %>%
        distinct(Sport) %>%
        arrange(Sport) %>%
        pull(Sport)
      
      updateSelectInput(session, "sport_select",
                        choices = sports,
                        selected = "Swimming")
    })
    
    # Sport Metrics
    output$sport_medals <- renderText({
      req(input$sport_select)
      df <- olympic_data()
      
      total <- df %>%
        filter(Sport == input$sport_select, !is.na(Medal)) %>%
        nrow()
      
      formatC(total, format="d", big.mark=",")
    })
    
    output$sport_athletes <- renderText({
      req(input$sport_select)
      df <- olympic_data()
      
      athletes <- df %>%
        filter(Sport == input$sport_select) %>%
        distinct(Name) %>%
        nrow()
      
      formatC(athletes, format="d", big.mark=",")
    })
    
    output$sport_countries <- renderText({
      req(input$sport_select)
      df <- olympic_data()
      
      countries <- df %>%
        filter(Sport == input$sport_select) %>%
        distinct(Team) %>%
        nrow()
      
      formatC(countries, format="d", big.mark=",")
    })
    
    # Sport Top Countries
    output$sport_top_countries <- renderPlotly({
      req(input$sport_select)
      df <- olympic_data()
      
      top_countries <- df %>%
        filter(Sport == input$sport_select, !is.na(Medal)) %>%
        group_by(Team) %>%
        summarise(medals = n(), .groups = "drop") %>%
        arrange(desc(medals)) %>%
        head(10)
      
      if(nrow(top_countries) == 0) {
        return(plot_ly() %>% layout(title = "No data available"))
      }
      
      plot_ly(top_countries,
              x = ~reorder(Team, medals),
              y = ~medals,
              type = 'bar',
              marker = list(color = '#0085C7'),
              text = ~medals,
              textposition = 'outside') %>%
        layout(
          xaxis = list(title = "Country", tickangle = -45),
          yaxis = list(title = "Medals"),
          margin = list(b = 100),
          paper_bgcolor = 'rgba(0,0,0,0)',
          plot_bgcolor = 'rgba(0,0,0,0)'
        )
    })
    
    # Sport Top Athletes
    output$sport_top_athletes <- renderPlotly({
      req(input$sport_select)
      df <- olympic_data()
      
      top_athletes <- df %>%
        filter(Sport == input$sport_select, !is.na(Medal)) %>%
        group_by(Name) %>%
        summarise(medals = n(), .groups = "drop") %>%
        arrange(desc(medals)) %>%
        head(10)
      
      if(nrow(top_athletes) == 0) {
        return(plot_ly() %>% layout(title = "No data available"))
      }
      
      plot_ly(top_athletes,
              x = ~reorder(Name, medals),
              y = ~medals,
              type = 'bar',
              marker = list(color = '#FFD700'),
              text = ~medals,
              textposition = 'outside') %>%
        layout(
          xaxis = list(title = "Athlete", tickangle = -45),
          yaxis = list(title = "Medals"),
          margin = list(b = 150),
          paper_bgcolor = 'rgba(0,0,0,0)',
          plot_bgcolor = 'rgba(0,0,0,0)'
        )
    })
    
    # ========== DATA EXPLORER TAB ==========
    
    # Populate filter dropdowns
    observe({
      df <- olympic_data()
      
      sports <- c("All", sort(unique(df$Sport)))
      countries <- c("All", sort(unique(df$Team)))
      years <- c("All", sort(unique(df$Year), decreasing = TRUE))
      
      updateSelectInput(session, "explorer_sport", choices = sports)
      updateSelectInput(session, "explorer_country", choices = countries)
      updateSelectInput(session, "explorer_year", choices = years)
    })
    
    # Filtered data for explorer
    explorer_filtered_data <- reactive({
      df <- olympic_data()
      
      if(input$explorer_sport != "All") {
        df <- df %>% filter(Sport == input$explorer_sport)
      }
      
      if(input$explorer_country != "All") {
        df <- df %>% filter(Team == input$explorer_country)
      }
      
      if(input$explorer_year != "All") {
        df <- df %>% filter(Year == as.numeric(input$explorer_year))
      }
      
      if(input$explorer_medal != "all") {
        df <- df %>% filter(Medal == input$explorer_medal)
      }
      
      return(df)
    })
    
    # Explorer Table
    output$explorer_table <- DT::renderDataTable({
      data <- explorer_filtered_data() %>%
        select(Name, Sex, Age, Height, Weight, Team, NOC, Year, Season, City, Sport, Event, Medal)
      
      DT::datatable(
        data,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          searching = TRUE,
          ordering = TRUE,
          order = list(list(7, 'desc'))  # Sort by Year descending
        ),
        rownames = FALSE,
        filter = 'top'
      )
    })
    
    # Download Handler
    output$download_data <- downloadHandler(
      filename = function() {
        paste0("olympic_data_", Sys.Date(), ".csv")
      },
      content = function(file) {
        data <- explorer_filtered_data() %>%
          select(Name, Sex, Age, Height, Weight, Team, NOC, Year, Season, City, Sport, Event, Medal)
        write.csv(data, file, row.names = FALSE)
      }
    )
  }