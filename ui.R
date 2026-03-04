library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(tidyverse)
library(plotly)
library(htmltools)
library(DT)
library(randomForest)
# Define UI for application that draws a histogram
fluidPage(
  
  # Application title
  titlePanel("120 Years of Olympic Data"),
  
  # NFL Teams list (shared across UI)
  nfl_teams <- c(
    "Arizona Cardinals", "Atlanta Falcons", "Baltimore Ravens", "Buffalo Bills",
    "Carolina Panthers", "Chicago Bears", "Cincinnati Bengals", "Cleveland Browns",
    "Dallas Cowboys", "Denver Broncos", "Detroit Lions", "Green Bay Packers",
    "Houston Texans", "Indianapolis Colts", "Jacksonville Jaguars", "Kansas City Chiefs",
    "Las Vegas Raiders", "Los Angeles Chargers", "Los Angeles Rams", "Miami Dolphins",
    "Minnesota Vikings", "New England Patriots", "New Orleans Saints", "New York Giants",
    "New York Jets", "Philadelphia Eagles", "Pittsburgh Steelers", "San Francisco 49ers",
    "Seattle Seahawks", "Tampa Bay Buccaneers", "Tennessee Titans", "Washington Commanders"
  )
  
  ui <- dashboardPage(
    skin = "blue",
    dashboardHeader(
      title = span(
        icon("football-ball", style = "margin-right: 10px;"),
        "NFL Analytics Pro"
      ),
      titleWidth = 280
    ),
    
    dashboardSidebar(
      width = 280,
      sidebarMenu(
        id = "tabs",
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("Stadium Map", tabName = "analytics", icon = icon("map-marker-alt")),
        menuItem("Betting Analysis", tabName = "betting_analysis", icon = icon("chart-bar")),
        menuItem("ROI Calculator", tabName = "roi_calculator", icon = icon("calculator")),
        menuItem("Game Predictor", tabName = "game_predictor", icon = icon("brain")),
        menuItem("Cluster Analysis", tabName = "cluster_analysis", icon = icon("project-diagram")),
        menuItem("My Betting Library", tabName = "betting_library", icon = icon("book"))
      )
    ),
    