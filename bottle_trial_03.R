library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(leaflet)
library(readxl)
library(dplyr)
library(janitor)  # For automatic column name cleaning

# Load Data with Clean Column Names
file_path <- "data/cleaned_bottle.xlsx"
calcofi_data <- read_excel(file_path) %>% 
  clean_names()  # Convert column names to snake_case

# Load Cast Data for Latitude & Longitude with Clean Column Names
cast_file_path <- "data/cast_cleaned.xlsx"
cast_data <- read_excel(cast_file_path) %>% 
  clean_names() %>% 
  select(sta_id, lat_dec, lon_dec) %>% 
  drop_na()

# Merge Hydrographic Data with Cast Data
selected_columns <- c("sta_id", "depthm", "t_deg_c", "salnty", "o2ml_l")
calcofi_filtered <- calcofi_data %>% 
  select(all_of(selected_columns)) %>% 
  left_join(cast_data, by = "sta_id") %>% 
  head(500)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "California Coastal Water Quality Explorer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map Explorer", tabName = "map", icon = icon("map")),
      menuItem("Time Series Graphs", tabName = "timeseries", icon = icon("line-chart")),
      menuItem("Data Table & Stats", tabName = "data", icon = icon("table")),
      menuItem("Advanced Analysis", tabName = "analysis", icon = icon("cogs"))
    )
  ),
  dashboardBody(
    tabItems(
      # Interactive Map Tab
      tabItem(tabName = "map",
              fluidRow(
                box(width = 12, leafletOutput("map"))
              )
      ),
      # Time Series Graphs Tab
      tabItem(tabName = "timeseries",
              fluidRow(
                box(width = 12, plotOutput("time_series_plot"))
              )
      ),
      # Data Table & Summary Stats
      tabItem(tabName = "data",
              fluidRow(
                box(width = 12, DTOutput("data_table")),
                box(width = 6, verbatimTextOutput("summary_stats"))
              )
      ),
      # Advanced Analysis
      tabItem(tabName = "analysis",
              fluidRow(
                box(width = 12, h4("Coming Soon: PCA & Multiple Linear Regression"))
              )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Render Interactive Map with Data Points
  output$map <- renderLeaflet({
    leaflet(calcofi_filtered) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~lon_dec, lat = ~lat_dec,
        popup = ~paste("Station ID:", sta_id, "<br>",
                       "Depth:", depthm, "m<br>",
                       "Temp:", t_deg_c, "°C<br>",
                       "salnty:", salnty, "<br>",
                       "Oxygen:", o2ml_l, "ml/L"),
        radius = 4, color = "blue", fillOpacity = 0.7
      ) %>%
      setView(lng = -120, lat = 34, zoom = 6)
  })
  
  # Render Time Series Plot
  output$time_series_plot <- renderPlot({
    ggplot(calcofi_filtered, aes(x = depthm, y = t_deg_c)) +
      geom_line(color = "blue") +
      labs(title = "Temperature vs. Depth", x = "Depth (m)", y = "Temperature (°C)")
  })
  
  # Render Data Table
  output$data_table <- renderDT({
    datatable(calcofi_filtered)
  })
  
  # Render Summary Statistics
  output$summary_stats <- renderPrint({
    summary(calcofi_filtered)
  })
}

# Run the Application
shinyApp(ui, server)
