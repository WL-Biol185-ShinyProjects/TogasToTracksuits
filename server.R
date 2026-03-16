Olympic_Dataset <- read.csv("athlete_events-Olympic Dataset.csv")

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
      Medal     = ifelse(is.na(Medal), "None", Medal),
      Has_Medal = (Medal != "None"),
      Age       = suppressWarnings(as.numeric(Age)),
      Height    = suppressWarnings(as.numeric(Height)),
      Weight    = suppressWarnings(as.numeric(Weight)),
      Year      = as.integer(Year)
    )
  
  medal_data <- olympic_data %>% filter(Has_Medal == TRUE)
  
  # ===========================================================================
  # STARTUP MODAL - FAVORITE COUNTRY SELECTION
  # ===========================================================================
  
  countries <- sort(unique(olympic_data$Team))
  
  showModal(modalDialog(
    title = tags$div(
      style = "text-align: center;",
      icon("medal", style = "font-size: 48px; color: #FFD700; margin-bottom: 15px;"),
      tags$h2("Welcome to Olympic Analytics!",
              style = "color: #0085C7; margin-top: 10px;")
    ),
    tags$div(
      style = "text-align: center; padding: 20px;",
      tags$h4("Select Your Favorite Country", style = "margin-bottom: 20px;"),
      tags$p("Your dashboard will jump straight to your country's Olympic history!",
             style = "color: #7F8C8D; margin-bottom: 20px;"),
      selectInput("modal_country", NULL,
                  choices = countries,
                  selected = "United States",
                  width = "100%")
    ),
    footer = tagList(
      actionButton("skip_country", "Skip", class = "btn-default"),
      actionButton("confirm_country", "Let's Go!", class = "btn-primary",
                   icon = icon("check"))
    ),
    size = "m",
    easyClose = FALSE
  ))
  
  observeEvent(input$confirm_country, {
    updateSelectInput(session, "fav_country", selected = input$modal_country)
    removeModal()
    updateTabItems(session, "tabs", "my_country")
  })
  
  observeEvent(input$skip_country, {
    removeModal()
  })
  
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
    top10_team <- medal_data %>%
      filter(Medal %in% c("Gold", "Silver", "Bronze")) %>%
      count(Team) %>%
      arrange(desc(n)) %>%
      head(10) %>%
      pull(Team)
    
    top_countries <- medal_data %>%
      filter(Medal %in% c("Gold", "Silver", "Bronze"), Team %in% top10_team) %>%
      count(Team, Medal) %>%
      mutate(Team = factor(Team, levels = rev(top10_team)))
    
    colors <- c("Gold" = "#FFD700", "Silver" = "#A8A9AD", "Bronze" = "#CD7F32")
    
    plot_ly(top_countries, x = ~n, y = ~Team, color = ~Medal,
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
  output$sport_countries <- renderText({ formatC(length(unique(sport_df()$Team)), format = "d", big.mark = ",") })
  
  output$sport_top_countries <- renderPlotly({
    df <- sport_medals_df()
    
    if (nrow(df) == 0) {
      return(plot_ly() %>% layout(title = "No medal data for this sport"))
    }
    
    top10_team <- df %>%
      filter(Medal %in% c("Gold", "Silver", "Bronze")) %>%
      count(Team) %>%
      arrange(desc(n)) %>%
      head(10) %>%
      pull(Team)
    
    plot_data <- df %>%
      filter(Medal %in% c("Gold", "Silver", "Bronze"), Team %in% top10_team) %>%
      count(Team, Medal) %>%
      mutate(Team = factor(Team, levels = rev(top10_team)))
    
    colors <- c("Gold" = "#FFD700", "Silver" = "#A8A9AD", "Bronze" = "#CD7F32")
    
    plot_ly(plot_data, x = ~n, y = ~Team, color = ~Medal,
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
  
  # ===========================================================================
  # MY COUNTRY TAB
  # ===========================================================================
  
  observe({
    countries <- sort(unique(olympic_data$Team))
    updateSelectInput(session, "fav_country",
                      choices = countries,
                      selected = "United States")
  })
  
  fav_country_df <- reactive({
    req(input$fav_country)
    medal_data %>% filter(Team == input$fav_country)
  })
  
  output$fav_total_medals <- renderText({
    formatC(nrow(fav_country_df()), format = "d", big.mark = ",")
  })
  
  output$fav_gold   <- renderText({ sum(fav_country_df()$Medal == "Gold")   })
  output$fav_silver <- renderText({ sum(fav_country_df()$Medal == "Silver") })
  output$fav_bronze <- renderText({ sum(fav_country_df()$Medal == "Bronze") })
  
  output$fav_country_timeline <- renderPlotly({
    df <- fav_country_df()
    
    if (nrow(df) == 0) {
      return(plot_ly() %>% layout(title = "No medal data for this country"))
    }
    
    timeline <- df %>%
      filter(Medal %in% c("Gold", "Silver", "Bronze")) %>%
      count(Year, Medal) %>%
      mutate(Medal = factor(Medal, levels = c("Gold", "Silver", "Bronze")))
    
    colors <- c("Gold" = "#FFD700", "Silver" = "#A8A9AD", "Bronze" = "#CD7F32")
    
    plot_ly(timeline, x = ~Year, y = ~n, color = ~Medal,
            colors = colors,
            type = "bar",
            hoverinfo = "x+y+name") %>%
      layout(
        barmode = "stack",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Medals Won"),
        paper_bgcolor = "white",
        plot_bgcolor  = "white"
      )
  })
  
  output$fav_country_pie <- renderPlotly({
    df <- fav_country_df()
    
    if (nrow(df) == 0) {
      return(plot_ly() %>% layout(title = "No medal data for this country"))
    }
    
    medal_counts <- df %>%
      count(Medal) %>%
      filter(Medal %in% c("Gold", "Silver", "Bronze"))
    
    colors <- c("Gold" = "#FFD700", "Silver" = "#C0C0C0", "Bronze" = "#CD7F32")
    
    plot_ly(medal_counts,
            labels = ~Medal,
            values = ~n,
            type = "pie",
            marker = list(colors = unname(colors[medal_counts$Medal])),
            textinfo = "label+percent",
            hoverinfo = "label+value+percent") %>%
      layout(
        showlegend = TRUE,
        paper_bgcolor = "white",
        plot_bgcolor  = "white"
      )
  })
  
  output$fav_country_sports <- renderPlotly({
    df <- fav_country_df()
    
    if (nrow(df) == 0) {
      return(plot_ly() %>% layout(title = "No medal data for this country"))
    }
    
    top_sports <- df %>%
      filter(Medal %in% c("Gold", "Silver", "Bronze")) %>%
      count(Sport, Medal) %>%
      group_by(Sport) %>%
      mutate(Total = sum(n)) %>%
      ungroup()
    
    top10_sports <- top_sports %>%
      distinct(Sport, Total) %>%
      arrange(desc(Total)) %>%
      head(10) %>%
      pull(Sport)
    
    plot_data <- top_sports %>%
      filter(Sport %in% top10_sports) %>%
      mutate(Sport = factor(Sport, levels = rev(top10_sports)))
    
    colors <- c("Gold" = "#FFD700", "Silver" = "#A8A9AD", "Bronze" = "#CD7F32")
    
    plot_ly(plot_data, x = ~n, y = ~Sport, color = ~Medal,
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
  
  output$fav_country_athletes <- renderPlotly({
    df <- fav_country_df()
    
    if (nrow(df) == 0) {
      return(plot_ly() %>% layout(title = "No medal data for this country"))
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
  
  # ===========================================================================
  # DOMINANCE INDEX TAB
  # ===========================================================================
  
  dominance_df <- reactive({
    df <- medal_data %>%
      filter(Medal %in% c("Gold", "Silver", "Bronze"))
    
    if (input$dominance_season != "both") {
      df <- df %>% filter(Season == input$dominance_season)
    }
    
    if (input$dominance_medal == "Gold") {
      df <- df %>% filter(Medal == "Gold")
    }
    
    df <- df %>%
      mutate(Decade = paste0(floor(Year / 10) * 10, "s"))
    
    return(df)
  })
  
  decade_summary <- reactive({
    dominance_df() %>%
      count(Decade, Team) %>%
      group_by(Decade) %>%
      mutate(Total_That_Decade = sum(n)) %>%
      ungroup()
  })
  
  top5_countries <- reactive({
    dominance_df() %>%
      count(Team) %>%
      arrange(desc(n)) %>%
      head(5) %>%
      pull(Team)
  })
  
  top10_countries <- reactive({
    dominance_df() %>%
      count(Team) %>%
      arrange(desc(n)) %>%
      head(10) %>%
      pull(Team)
  })
  
  decade_champions <- reactive({
    decade_summary() %>%
      group_by(Decade) %>%
      slice_max(n, n = 1) %>%
      ungroup() %>%
      arrange(Decade)
  })
  
  output$dom_top_country <- renderText({
    dominance_df() %>%
      count(Team) %>%
      arrange(desc(n)) %>%
      slice(1) %>%
      pull(Team)
  })
  
  output$dom_peak_decade <- renderText({
    decade_champions() %>%
      arrange(desc(n)) %>%
      slice(1) %>%
      pull(Decade)
  })
  
  output$dom_peak_medals <- renderText({
    decade_champions() %>%
      arrange(desc(n)) %>%
      slice(1) %>%
      pull(n) %>%
      formatC(format = "d", big.mark = ",")
  })
  
  output$dominance_heatmap <- renderPlotly({
    df <- decade_summary() %>%
      filter(Team %in% top10_countries())
    
    if (nrow(df) == 0) {
      return(plot_ly() %>% layout(title = "No data available"))
    }
    
    plot_ly(df,
            x = ~Decade,
            y = ~reorder(Team, n),
            z = ~n,
            type = "heatmap",
            colorscale = list(
              c(0,    "#F7F9FC"),
              c(0.2,  "#C8E6F5"),
              c(0.4,  "#0085C7"),
              c(0.7,  "#FFD700"),
              c(1,    "#FF4500")
            ),
            text = ~paste0("<b>", Team, "</b><br>",
                           "Decade: ", Decade, "<br>",
                           "Medals: ", n),
            hoverinfo = "text",
            showscale = TRUE,
            colorbar = list(
              title = "Medals",
              titlefont = list(size = 13),
              tickfont  = list(size = 11)
            )) %>%
      layout(
        xaxis = list(
          title      = "",
          tickangle  = -45,
          tickfont   = list(size = 12, color = "#2C3E50"),
          showgrid   = FALSE
        ),
        yaxis = list(
          title    = "",
          tickfont = list(size = 12, color = "#2C3E50"),
          showgrid = FALSE
        ),
        paper_bgcolor = "white",
        plot_bgcolor  = "white",
        margin = list(l = 140, r = 40, t = 20, b = 80)
      )
  })
  
  output$decade_champion_bar <- renderPlotly({
    df <- decade_champions()
    
    if (nrow(df) == 0) {
      return(plot_ly() %>% layout(title = "No data available"))
    }
    
    unique_teams <- unique(df$Team)
    ring_colors  <- c("#0085C7", "#EE334E", "#FFD700", "#00A651", "#000000",
                      "#F39C12", "#8E44AD", "#16A085", "#E74C3C", "#2C3E50")
    team_colors  <- setNames(ring_colors[seq_along(unique_teams)], unique_teams)
    
    plot_ly(df,
            x = ~Decade,
            y = ~n,
            color = ~Team,
            colors = team_colors,
            type = "bar",
            text = ~paste0("<b>", Team, "</b><br>",
                           Decade, "<br>",
                           "Medals: <b>", n, "</b>"),
            hoverinfo = "text",
            marker = list(
              line = list(color = "white", width = 1.5)
            )) %>%
      layout(
        barmode = "stack",
        xaxis = list(
          title     = "",
          tickangle = -45,
          tickfont  = list(size = 12, color = "#2C3E50"),
          showgrid  = FALSE
        ),
        yaxis = list(
          title      = "Medals Won",
          tickfont   = list(size = 12, color = "#2C3E50"),
          gridcolor  = "#ECF0F1",
          gridwidth  = 1
        ),
        paper_bgcolor = "white",
        plot_bgcolor  = "white",
        legend = list(
          title       = list(text = "<b>Country</b>"),
          bgcolor     = "rgba(255,255,255,0.9)",
          bordercolor = "#E0E6ED",
          borderwidth = 1
        ),
        margin = list(l = 60, r = 40, t = 20, b = 80)
      )
  })
  
  output$dominance_line <- renderPlotly({
    df <- decade_summary() %>%
      filter(Team %in% top5_countries())
    
    if (nrow(df) == 0) {
      return(plot_ly() %>% layout(title = "No data available"))
    }
    
    unique_teams <- unique(df$Team)
    ring_colors  <- c("#0085C7", "#EE334E", "#FFD700", "#00A651", "#2C3E50")
    team_colors  <- setNames(ring_colors[seq_along(unique_teams)], unique_teams)
    
    plot_ly(df,
            x = ~Decade,
            y = ~n,
            color = ~Team,
            colors = team_colors,
            type = "scatter",
            mode = "lines+markers",
            text = ~paste0("<b>", Team, "</b><br>",
                           Decade, "<br>",
                           "Medals: <b>", n, "</b>"),
            hoverinfo = "text",
            line   = list(width = 3),
            marker = list(
              size = 10,
              line = list(color = "white", width = 2)
            )) %>%
      layout(
        xaxis = list(
          title     = "",
          tickangle = -45,
          tickfont  = list(size = 12, color = "#2C3E50"),
          showgrid  = FALSE
        ),
        yaxis = list(
          title     = "Medals Won",
          tickfont  = list(size = 12, color = "#2C3E50"),
          gridcolor = "#ECF0F1",
          gridwidth = 1,
          zeroline  = FALSE
        ),
        paper_bgcolor = "white",
        plot_bgcolor  = "white",
        legend = list(
          title       = list(text = "<b>Country</b>"),
          bgcolor     = "rgba(255,255,255,0.9)",
          bordercolor = "#E0E6ED",
          borderwidth = 1
        ),
        margin    = list(l = 60, r = 40, t = 20, b = 80),
        hovermode = "x unified"
      )
  })
  
}