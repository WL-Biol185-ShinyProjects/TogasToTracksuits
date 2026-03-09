
#loading data
Olympic_Dataset = read.csv("athlete_events-Olympic Dataset.csv")

function(input, output, session) {

    # ========== SHARED DATA ==========
    # Olympic countries list (already defined in UI, but can reference here if needed)
    
    # ========== LOAD AND PROCESS DATA ==========
    olympic_data <- reactive({
      df <- read_excel("athlete_events-Olympic_Dataset.xlsx")
      
      # Add any data processing/mutations here
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
          )
        )
      
      return(df)
      })
    
    # ========== HOME TAB ==========
    # Add welcome content, stats overview, etc.
    
    # ========== DASHBOARD TAB ==========
    # Summary statistics value boxes
    # Medal distribution charts
    # Top countries/athletes visualizations
    
    # ========== ATHLETES TAB ==========
    # Athlete search dropdown
    # Individual athlete profile and stats
    # Medal timeline for selected athlete
    # Career record table
    
    # ========== COUNTRIES TAB ==========
    # Country selection dropdowns (2 countries to compare)
    # Medal comparison charts
    # Performance over time
    # Top sports by country
    
    # ========== SPORTS TAB ==========
    # Sport selection dropdown
    # Sport-specific statistics
    # Top countries in selected sport
    # Top athletes in selected sport
    # Participation trends
    
    # ========== DATA EXPLORER TAB ==========
    # Filterable data table
    # Filter inputs (sport, country, year, medal)
    # Download handler for filtered data
    

}
