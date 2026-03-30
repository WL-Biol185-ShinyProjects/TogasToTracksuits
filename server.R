Olympic_Dataset <- read.csv("athlete_events-Olympic Dataset.csv")

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
                  choices = olympic_countries,
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
             Medal %in% c("Gold", "Silver", "Bronze"))
    
    if (input$country_gender != "both") {
      df <- df %>% filter(Sex == input$country_gender)
    }
    
    df <- df %>% count(Team, Medal)
    
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
             Medal %in% c("Gold", "Silver", "Bronze"))
    
    if (input$country_gender != "both") {
      df <- df %>% filter(Sex == input$country_gender)
    }
    
    df <- df %>% count(Year, Team)
    
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
    df <- olympic_data %>% filter(Sport == input$sport_select)
    if (input$sport_gender != "both") {
      df <- df %>% filter(Sex == input$sport_gender)
    }
    return(df)
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
    updateSelectInput(session, "fav_country",
                      choices = olympic_countries,
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
    
    all_decades  <- sort(unique(df$Decade))
    all_teams    <- top10_countries()
    full_grid    <- expand.grid(Decade = all_decades, Team = all_teams,
                                stringsAsFactors = FALSE)
    df <- full_grid %>%
      left_join(df %>% select(Decade, Team, n), by = c("Decade", "Team")) %>%
      mutate(n = ifelse(is.na(n), 0, n),
             Team = factor(Team, levels = rev(all_teams)))
    
    plot_ly(df,
            x = ~Decade,
            y = ~Team,
            z = ~n,
            type = "heatmap",
            colorscale = list(
              c(0,    "#FFFFFF"),
              c(0.15, "#D4EAF7"),
              c(0.35, "#74B9E0"),
              c(0.6,  "#0085C7"),
              c(0.8,  "#FFD700"),
              c(1,    "#FF8C00")
            ),
            text = ~paste0("<b>", Team, "</b><br>",
                           "Decade: ", Decade, "<br>",
                           "Medals: ", n),
            hoverinfo = "text",
            showscale = TRUE,
            colorbar = list(
              title      = "Medals",
              titlefont  = list(size = 13, color = "#2C3E50"),
              tickfont   = list(size = 11, color = "#2C3E50"),
              len        = 0.8
            ),
            zmin = 0) %>%
      layout(
        xaxis = list(
          title     = "",
          tickangle = -45,
          tickfont  = list(size = 12, color = "#2C3E50"),
          showgrid  = FALSE
        ),
        yaxis = list(
          title    = "",
          tickfont = list(size = 12, color = "#2C3E50"),
          showgrid = FALSE
        ),
        paper_bgcolor = "white",
        plot_bgcolor  = "white",
        margin = list(l = 160, r = 80, t = 20, b = 80)
      )
  })
  
  output$decade_champion_table <- DT::renderDataTable({
    df <- decade_champions()
    
    if (nrow(df) == 0) {
      return(DT::datatable(data.frame(Message = "No data available")))
    }
    
    medal_breakdown <- dominance_df() %>%
      inner_join(decade_champions() %>% select(Decade, Team),
                 by = c("Decade", "Team")) %>%
      group_by(Decade, Team, Medal) %>%
      summarise(count = n(), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = Medal, values_from = count,
                         values_fill = 0)
    
    df <- df %>%
      left_join(medal_breakdown, by = c("Decade", "Team"))
    
    flag_codes <- c(
      "United States"  = "us", "Soviet Union"   = "ru", "Germany"        = "de",
      "Great Britain"  = "gb", "France"         = "fr", "Australia"      = "au",
      "Italy"          = "it", "China"          = "cn", "Sweden"         = "se",
      "Hungary"        = "hu", "East Germany"   = "de", "Russia"         = "ru",
      "Finland"        = "fi", "Japan"          = "jp", "Norway"         = "no",
      "Canada"         = "ca", "Netherlands"    = "nl", "South Korea"    = "kr",
      "Cuba"           = "cu", "Romania"        = "ro", "Poland"         = "pl",
      "Denmark"        = "dk", "Switzerland"    = "ch", "Austria"        = "at",
      "Belgium"        = "be", "Unified Team"   = "ru", "Czechoslovakia" = "cz",
      "West Germany"   = "de", "Bulgaria"       = "bg", "Yugoslavia"     = "rs",
      "Kenya"          = "ke", "Brazil"         = "br", "Spain"          = "es",
      "New Zealand"    = "nz", "Jamaica"        = "jm", "Greece"         = "gr"
    )
    
    df <- df %>%
      mutate(
        iso    = tolower(flag_codes[Team]),
        iso    = ifelse(is.na(iso), "un", iso),
        Flag   = paste0(
          '<img src="https://flagcdn.com/32x24/', iso, '.png" ',
          'width="32" height="24" ',
          'style="border-radius:3px; box-shadow: 0 1px 4px rgba(0,0,0,0.2);" ',
          'onerror="this.style.display=\'none\'">'
        ),
        Gold   = ifelse(is.na(Gold),   0L, as.integer(Gold)),
        Silver = ifelse(is.na(Silver), 0L, as.integer(Silver)),
        Bronze = ifelse(is.na(Bronze), 0L, as.integer(Bronze)),
        Total  = Gold + Silver + Bronze,
        Medals = paste0(
          '<span style="color:#DAA520; font-weight:700; font-size:15px;">&#9679; ',
          Gold, '</span>&nbsp;&nbsp;',
          '<span style="color:#909090; font-weight:700; font-size:15px;">&#9679; ',
          Silver, '</span>&nbsp;&nbsp;',
          '<span style="color:#8B4513; font-weight:700; font-size:15px;">&#9679; ',
          Bronze, '</span>&nbsp;&nbsp;',
          '<span style="background:#0085C7; color:white; font-weight:700; ',
          'font-size:12px; padding:2px 8px; border-radius:10px;">',
          Total, ' total</span>'
        )
      ) %>%
      select(Flag, Decade, Team, Medals) %>%
      rename(Country = Team)
    
    DT::datatable(
      df,
      escape   = FALSE,
      rownames = FALSE,
      options  = list(
        pageLength = 15,
        dom        = 't',
        ordering   = FALSE,
        scrollX    = FALSE,
        columnDefs = list(
          list(className = 'dt-center', targets = c(0, 1, 3)),
          list(width = '55px',  targets = 0),
          list(width = '90px',  targets = 1),
          list(width = '180px', targets = 2),
          list(width = '300px', targets = 3)
        )
      ),
      class = 'cell-border stripe'
    ) %>%
      DT::formatStyle(
        'Decade',
        fontWeight = 'bold',
        color      = '#0085C7',
        fontSize   = '15px'
      ) %>%
      DT::formatStyle(
        'Country',
        fontWeight = '600',
        color      = '#2C3E50',
        fontSize   = '14px'
      ) %>%
      DT::formatStyle(
        columns    = 0:3,
        lineHeight = '44px'
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
  
  # ===========================================================================
  # MOST IMPROVED TAB
  # ===========================================================================
  
  improved_df <- reactive({
    df <- medal_data %>%
      filter(Medal %in% c("Gold", "Silver", "Bronze"))
    
    if (input$improved_medal == "Gold") {
      df <- df %>% filter(Medal == "Gold")
    }
    
    nineties <- df %>%
      filter(Year >= 1990, Year <= 1999) %>%
      count(Team) %>%
      rename(Medals_1990s = n)
    
    twenty_tens <- df %>%
      filter(Year >= 2010, Year <= 2016) %>%
      count(Team) %>%
      rename(Medals_2010s = n)
    
    improved <- nineties %>%
      inner_join(twenty_tens, by = "Team") %>%
      mutate(Improvement = Medals_2010s - Medals_1990s) %>%
      arrange(desc(Improvement))
    
    return(improved)
  })
  
  output$top_improved_country <- renderText({
    improved_df() %>% slice(1) %>% pull(Team)
  })
  
  output$top_improved_then <- renderText({
    improved_df() %>% slice(1) %>% pull(Medals_1990s)
  })
  
  output$top_improved_now <- renderText({
    improved_df() %>% slice(1) %>% pull(Medals_2010s)
  })
  
  output$most_improved_bar <- renderPlotly({
    df <- improved_df() %>% head(15)
    
    df <- df %>%
      mutate(Team = factor(Team, levels = rev(Team)))
    
    plot_ly(df, x = ~Improvement, y = ~Team,
            type = "bar", orientation = "h",
            marker = list(color = "#0085C7",
                          line = list(color = "#005A8C", width = 1)),
            text = ~paste0("+", Improvement, " medals"),
            textposition = "outside",
            hoverinfo = "y+text") %>%
      layout(
        xaxis = list(title = "Medal Increase (1990s → 2010s)"),
        yaxis = list(title = ""),
        paper_bgcolor = "white",
        plot_bgcolor  = "white"
      )
  })
  
  output$most_improved_table <- DT::renderDataTable({
    df <- improved_df() %>%
      head(20) %>%
      mutate(Change = paste0(ifelse(Improvement > 0, "+", ""), Improvement)) %>%
      select(Team, Medals_1990s, Medals_2010s, Change) %>%
      rename(Country = Team, `1990s Medals` = Medals_1990s,
             `2010s Medals` = Medals_2010s)
    
    DT::datatable(df,
                  options = list(pageLength = 20, dom = 't', scrollX = TRUE),
                  rownames = FALSE)
  })
  
  # ===========================================================================
  # MEDAL MAP TAB  — NEW
  # ===========================================================================
  
  # NOC -> country name + ISO2 flag code + lat/lon for all 230 NOCs in dataset
  noc_lookup <- data.frame(
    NOC = c(
      "AFG","AHO","ALB","ALG","AND","ANG","ANT","ANZ","ARG","ARM",
      "ARU","ASA","AUS","AUT","AZE","BAH","BAN","BAR","BDI","BEL",
      "BEN","BER","BHU","BIH","BIZ","BLR","BOH","BOL","BOT","BRA",
      "BRN","BRU","BUL","BUR","CAF","CAM","CAN","CAY","CGO","CHA",
      "CHI","CHN","CIV","CMR","COD","COK","COL","COM","CPV","CRC",
      "CRO","CRT","CUB","CYP","CZE","DEN","DJI","DMA","DOM","ECU",
      "EGY","ERI","ESA","ESP","EST","ETH","EUN","FIJ","FIN","FRA",
      "FRG","FSM","GAB","GAM","GBR","GBS","GDR","GEO","GEQ","GER",
      "GHA","GRE","GRN","GUA","GUI","GUM","GUY","HAI","HKG","HON",
      "HUN","INA","IND","IOA","IRI","IRL","IRQ","ISL","ISR","ISV",
      "ITA","IVB","JAM","JOR","JPN","KAZ","KEN","KGZ","KIR","KOR",
      "KOS","KSA","KUW","LAO","LAT","LBA","LBR","LCA","LES","LIB",
      "LIE","LTU","LUX","MAD","MAL","MAR","MAS","MAW","MDA","MDV",
      "MEX","MGL","MHL","MKD","MLI","MLT","MNE","MON","MOZ","MRI",
      "MTN","MYA","NAM","NBO","NCA","NED","NEP","NFL","NGR","NIG",
      "NOR","NRU","NZL","OMA","PAK","PAN","PAR","PER","PHI","PLE",
      "PLW","PNG","POL","POR","PRK","PUR","QAT","RHO","ROT","ROU",
      "RSA","RUS","RWA","SAA","SAM","SCG","SEN","SEY","SGP","SKN",
      "SLE","SLO","SMR","SOL","SOM","SRB","SRI","SSD","STP","SUD",
      "SUI","SUR","SVK","SWE","SWZ","SYR","TAN","TCH","TGA","THA",
      "TJK","TKM","TLS","TOG","TPE","TTO","TUN","TUR","TUV","UAE",
      "UAR","UGA","UKR","UNK","URS","URU","USA","UZB","VAN","VEN",
      "VIE","VIN","VNM","WIF","YAR","YEM","YMD","YUG","ZAM","ZIM"
    ),
    Country = c(
      "Afghanistan","Netherlands Antilles","Albania","Algeria","Andorra","Angola",
      "Antigua and Barbuda","Australasia","Argentina","Armenia","Aruba","American Samoa",
      "Australia","Austria","Azerbaijan","Bahamas","Bangladesh","Barbados","Burundi",
      "Belgium","Benin","Bermuda","Bhutan","Bosnia & Herzegovina","Belize","Belarus",
      "Bohemia","Bolivia","Botswana","Brazil","Bahrain","Brunei","Bulgaria",
      "Burkina Faso","Central African Rep.","Cambodia","Canada","Cayman Islands",
      "Congo","Chad","Chile","China","Cote d'Ivoire","Cameroon","DR Congo",
      "Cook Islands","Colombia","Comoros","Cape Verde","Costa Rica","Croatia","Crete",
      "Cuba","Cyprus","Czech Republic","Denmark","Djibouti","Dominica",
      "Dominican Republic","Ecuador","Egypt","Eritrea","El Salvador","Spain","Estonia",
      "Ethiopia","Unified Team","Fiji","Finland","France","West Germany","Micronesia",
      "Gabon","Gambia","Great Britain","Guinea-Bissau","East Germany","Georgia",
      "Equatorial Guinea","Germany","Ghana","Greece","Grenada","Guatemala","Guinea",
      "Guam","Guyana","Haiti","Hong Kong","Honduras","Hungary","Indonesia","India",
      "Indep. Olympic Athletes","Iran","Ireland","Iraq","Iceland","Israel",
      "US Virgin Islands","Italy","British Virgin Islands","Jamaica","Jordan","Japan",
      "Kazakhstan","Kenya","Kyrgyzstan","Kiribati","South Korea","Kosovo",
      "Saudi Arabia","Kuwait","Laos","Latvia","Libya","Liberia","Saint Lucia",
      "Lesotho","Lebanon","Liechtenstein","Lithuania","Luxembourg","Madagascar",
      "Malaysia (historical)","Morocco","Malaysia","Malawi","Moldova","Maldives",
      "Mexico","Mongolia","Marshall Islands","North Macedonia","Mali","Malta",
      "Montenegro","Monaco","Mozambique","Mauritius","Mauritania","Myanmar","Namibia",
      "Kenya (historical)","Nicaragua","Netherlands","Nepal","Newfoundland","Nigeria",
      "Niger","Norway","Nauru","New Zealand","Oman","Pakistan","Panama","Paraguay",
      "Peru","Philippines","Palestine","Palau","Papua New Guinea","Poland","Portugal",
      "North Korea","Puerto Rico","Qatar","Rhodesia","Refugee Athletes","Romania",
      "South Africa","Russia","Rwanda","Saar","Samoa","Serbia & Montenegro","Senegal",
      "Seychelles","Singapore","St Kitts & Nevis","Sierra Leone","Slovenia",
      "San Marino","Solomon Islands","Somalia","Serbia","Sri Lanka","South Sudan",
      "Sao Tome & Principe","Sudan","Switzerland","Suriname","Slovakia","Sweden",
      "Eswatini","Syria","Tanzania","Czechoslovakia","Tonga","Thailand","Tajikistan",
      "Turkmenistan","Timor-Leste","Togo","Chinese Taipei","Trinidad & Tobago",
      "Tunisia","Turkey","Tuvalu","UAE","United Arab Republic","Uganda","Ukraine",
      "Unknown","Soviet Union","Uruguay","United States","Uzbekistan","Vanuatu",
      "Venezuela","Vietnam","St Vincent & Grenadines","Vietnam (historical)",
      "West Indies Fed.","North Yemen","Yemen","South Yemen","Yugoslavia","Zambia","Zimbabwe"
    ),
    iso2 = c(
      "af","an","al","dz","ad","ao","ag","au","ar","am","aw","as","au","at","az",
      "bs","bd","bb","bi","be","bj","bm","bt","ba","bz","by","cz","bo","bw","br",
      "bh","bn","bg","bf","cf","kh","ca","ky","cg","td","cl","cn","ci","cm","cd",
      "ck","co","km","cv","cr","hr","gr","cu","cy","cz","dk","dj","dm","do","ec",
      "eg","er","sv","es","ee","et","ru","fj","fi","fr","de","fm","ga","gm","gb",
      "gw","de","ge","gq","de","gh","gr","gd","gt","gn","gu","gy","ht","hk","hn",
      "hu","id","in","un","ir","ie","iq","is","il","vi","it","vg","jm","jo","jp",
      "kz","ke","kg","ki","kr","xk","sa","kw","la","lv","ly","lr","lc","ls","lb",
      "li","lt","lu","mg","my","ma","my","mw","md","mv","mx","mn","mh","mk","ml",
      "mt","me","mc","mz","mu","mr","mm","na","ke","ni","nl","np","ca","ng","ne",
      "no","nr","nz","om","pk","pa","py","pe","ph","ps","pw","pg","pl","pt","kp",
      "pr","qa","zw","un","ro","za","ru","rw","de","ws","rs","sn","sc","sg","kn",
      "sl","si","sm","sb","so","rs","lk","ss","st","sd","ch","sr","sk","se","sz",
      "sy","tz","cz","to","th","tj","tm","tl","tg","tw","tt","tn","tr","tv","ae",
      "eg","ug","ua","un","ru","uy","us","uz","vu","ve","vn","vc","vn","jm","ye",
      "ye","ye","rs","zm","zw"
    ),
    lat = c(
      33.93,12.23,41.15,28.03,42.55,-11.20,17.07,-25.27,-38.42,40.07,
      12.50,-14.27,-25.27,47.52,40.14,25.03,23.68,13.19,-3.38,50.50,
      9.31,32.31,27.51,43.92,17.19,53.71,50.08,-16.29,-22.33,-14.24,
      26.02,4.53,42.73,12.36,6.61,12.57,56.13,19.31,-0.23,15.45,
      -35.68,35.86,7.54,3.85,-4.04,-21.24,4.57,-11.88,16.00,9.75,
      45.10,35.24,21.52,35.13,49.82,56.26,11.83,15.41,18.74,-1.83,
      26.82,15.18,13.79,40.46,58.60,9.15,55.00,-18.12,61.92,46.23,
      51.17,6.92,-0.80,13.44,55.38,11.80,52.13,42.32,1.65,51.17,
      7.95,39.07,12.11,15.78,9.95,13.44,4.86,18.97,22.40,15.20,
      47.16,-0.79,20.59,0.00,32.43,53.41,33.22,64.96,31.05,17.73,
      41.87,18.42,18.11,31.24,36.20,48.02,-0.02,41.20,-1.03,35.91,
      42.60,24.21,29.31,19.86,56.88,26.34,6.43,13.90,-29.62,33.85,
      47.17,55.17,49.82,-18.77,3.15,31.79,2.50,-13.25,47.41,3.20,
      23.63,46.86,7.13,41.61,17.57,31.79,0.00,-21.17,3.92,-20.35,
      20.87,16.87,-22.96,0.00,12.86,52.13,28.39,47.00,10.45,17.61,
      60.47,-0.52,-40.90,21.51,30.37,8.54,-23.44,-9.19,12.88,31.95,
      7.52,-6.31,51.92,39.40,40.34,18.22,25.35,-19.02,0.00,45.94,
      -30.56,61.52,-1.94,0.00,-13.76,44.02,14.50,-4.68,1.35,17.36,
      8.46,46.15,43.94,-9.43,5.15,44.02,7.87,6.88,0.34,15.55,
      46.82,3.92,48.67,60.13,-26.52,34.80,-6.37,49.82,-21.18,15.87,
      38.86,40.00,-8.87,8.62,23.70,10.69,33.89,38.96,-7.11,23.42,
      26.82,1.37,48.38,0.00,61.52,-32.52,37.09,41.38,-15.38,6.42,
      14.06,12.98,14.06,17.19,15.55,15.55,15.55,44.02,-13.13,-20.00
    ),
    lon = c(
      67.71,68.97,20.17,1.66,1.52,17.87,-61.79,133.78,-63.62,45.04,
      -69.97,-170.13,133.78,14.55,47.58,-77.40,90.36,-59.54,29.92,4.47,
      2.31,-64.77,90.43,17.67,-88.49,27.95,14.47,-64.67,24.68,-51.93,
      50.55,114.73,25.49,-1.56,20.94,104.99,-106.35,-81.25,15.83,18.73,
      -71.54,104.20,-5.55,11.50,21.76,159.78,-74.07,43.87,-24.01,-83.75,
      15.20,23.73,-79.52,33.43,15.47,9.50,42.59,-61.37,-69.99,-77.40,
      30.80,39.78,-88.90,-3.75,25.01,40.49,50.00,179.41,25.75,2.21,
      9.01,158.22,11.67,-15.31,-3.44,-15.18,13.40,43.36,10.27,10.45,
      -1.02,21.82,-61.68,-90.23,-11.40,144.79,-58.93,-72.34,114.11,-86.24,
      19.50,113.92,78.96,0.00,53.69,-8.24,43.68,-18.49,34.85,-64.90,
      12.57,-64.64,-77.30,35.94,138.25,66.92,37.91,74.77,173.02,127.77,
      20.90,45.08,47.48,102.50,24.60,17.23,-9.43,-60.98,28.23,35.50,
      9.55,23.88,6.13,46.87,101.97,-7.09,109.80,34.30,28.37,35.86,
      -102.55,103.85,171.18,21.75,-4.00,-7.09,14.37,57.55,-15.18,57.55,
      -5.83,96.68,17.08,0.00,-85.21,5.29,84.12,-62.75,8.08,-1.01,
      8.47,166.93,172.50,57.55,69.35,-80.78,-58.44,-75.02,121.77,35.23,
      134.58,143.96,19.15,-8.22,127.51,-66.49,51.18,29.87,30.00,24.97,
      22.94,100.48,104.22,0.00,-15.31,21.01,14.37,55.49,103.82,-62.78,
      -11.78,14.99,12.46,160.16,46.20,21.01,80.77,31.30,6.73,32.29,
      8.23,-56.03,19.70,18.64,31.47,38.99,34.89,15.47,-26.32,30.22,
      71.27,59.56,125.73,1.22,120.97,-61.22,9.54,35.24,177.64,53.85,
      30.80,32.29,31.17,0.00,90.00,-55.76,-95.71,64.59,166.92,-66.59,
      108.28,-61.20,108.28,-61.79,44.47,44.47,44.47,21.01,27.85,30.00
    ),
    stringsAsFactors = FALSE
  )
  
  # Populate year dropdown
  observe({
    years <- sort(unique(olympic_data$Year))
    updateSelectInput(session, "map_year",
                      choices = c("All Years" = "all", setNames(years, years)),
                      selected = "all")
  })
  
  # Reactive filtered data for map
  map_data <- reactive({
    df <- medal_data %>%
      filter(Medal %in% c("Gold", "Silver", "Bronze"))
    
    if (!is.null(input$map_year) && input$map_year != "all") {
      df <- df %>% filter(Year == as.integer(input$map_year))
    }
    
    if (!is.null(input$map_medal) && input$map_medal != "all") {
      df <- df %>% filter(Medal == input$map_medal)
    }
    
    if (!is.null(input$map_season) && input$map_season != "both") {
      df <- df %>% filter(Season == input$map_season)
    }
    
    df %>%
      group_by(NOC) %>%
      summarise(
        Gold   = sum(Medal == "Gold"),
        Silver = sum(Medal == "Silver"),
        Bronze = sum(Medal == "Bronze"),
        Total  = n(),
        .groups = "drop"
      ) %>%
      inner_join(noc_lookup, by = "NOC") %>%
      filter(!is.na(lat), !is.na(lon))
  })
  
  # Render leaflet map
  output$medal_map <- renderLeaflet({
    df <- map_data()
    
    if (nrow(df) == 0) {
      return(leaflet() %>% addTiles() %>%
               addPopups(0, 20, "No medal data for the selected filters."))
    }
    
    df <- df %>% mutate(radius = pmax(sqrt(Total) * 2.5, 5))
    
    df <- df %>%
      mutate(popup_html = paste0(
        "<div style='font-family:sans-serif; min-width:190px;'>",
        "<div style='display:flex; align-items:center; margin-bottom:8px;'>",
        "<img src='https://flagcdn.com/32x24/", tolower(iso2), ".png' ",
        "style='margin-right:10px; border-radius:3px; ",
        "box-shadow:0 1px 4px rgba(0,0,0,0.3);' ",
        "onerror=\"this.style.display='none'\">",
        "<strong style='font-size:15px; color:#0085C7;'>", Country, "</strong>",
        "</div>",
        "<table style='width:100%; border-collapse:collapse;'>",
        "<tr><td style='padding:3px 6px;'>",
        "<span style='color:#DAA520; font-weight:700;'>&#9679;</span> Gold</td>",
        "<td style='text-align:right; font-weight:600;'>", Gold, "</td></tr>",
        "<tr><td style='padding:3px 6px;'>",
        "<span style='color:#909090; font-weight:700;'>&#9679;</span> Silver</td>",
        "<td style='text-align:right; font-weight:600;'>", Silver, "</td></tr>",
        "<tr><td style='padding:3px 6px;'>",
        "<span style='color:#8B4513; font-weight:700;'>&#9679;</span> Bronze</td>",
        "<td style='text-align:right; font-weight:600;'>", Bronze, "</td></tr>",
        "<tr style='border-top:1px solid #eee;'>",
        "<td style='padding:5px 6px; font-weight:700;'>Total</td>",
        "<td style='text-align:right; font-weight:700; color:#0085C7;'>",
        Total, "</td></tr>",
        "</table></div>"
      ))
    
    leaflet(df) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 10, lat = 20, zoom = 2) %>%
      addCircleMarkers(
        lng          = ~lon,
        lat          = ~lat,
        radius       = ~radius,
        color        = "#005A8C",
        fillColor    = "#FFD700",
        fillOpacity  = 0.75,
        weight       = 1.5,
        popup        = ~popup_html,
        label        = ~paste0(Country, ": ", Total, " medals"),
        labelOptions = labelOptions(
          style = list(
            "font-family" = "sans-serif",
            "font-weight" = "bold",
            "padding"     = "4px 8px"
          )
        )
      )
  })
  
  # Medal table below the map
  output$map_medal_table <- DT::renderDataTable({
    df <- map_data() %>%
      arrange(desc(Total)) %>%
      mutate(
        Flag = paste0(
          '<img src="https://flagcdn.com/24x18/', tolower(iso2),
          '.png" style="border-radius:2px; margin-right:6px;" ',
          'onerror="this.style.display=\'none\'">',
          Country
        )
      ) %>%
      select(Flag, NOC, Gold, Silver, Bronze, Total)
    
    DT::datatable(
      df,
      escape   = FALSE,
      rownames = FALSE,
      options  = list(
        pageLength = 15,
        scrollX    = TRUE,
        order      = list(list(5, "desc")),
        columnDefs = list(
          list(className = "dt-center", targets = c(1, 2, 3, 4, 5))
        )
      ),
      colnames = c("Country", "NOC", "🥇 Gold", "🥈 Silver", "🥉 Bronze", "Total")
    ) %>%
      DT::formatStyle("Gold",   color = "#DAA520", fontWeight = "bold") %>%
      DT::formatStyle("Silver", color = "#909090", fontWeight = "bold") %>%
      DT::formatStyle("Bronze", color = "#8B4513", fontWeight = "bold") %>%
      DT::formatStyle("Total",  color = "#0085C7", fontWeight = "bold")
  })
  
  # ===========================================================================
  # DOWNLOAD HANDLER - MY COUNTRY DATA
  # ===========================================================================
  
  output$download_country <- downloadHandler(
    filename = function() {
      paste0(input$fav_country, "_Olympic_Medals_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data_to_download <- fav_country_df() %>%
        select(Year, Season, City, Sport, Event, Name, Sex, Age, Height, Weight, Medal, NOC) %>%
        arrange(desc(Year), Medal)
      write.csv(data_to_download, file, row.names = FALSE)
    }
  )
}