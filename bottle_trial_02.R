library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(leaflet)
library(readxl)
library(dplyr)

# Load Data (Assuming Book2.xlsx is in the working directory)
file_path <- "data/Book2.xlsx"
calcofi_data <- read_excel(file_path)

# Select the first 500 rows of important variables
selected_columns <- c("Depthm", "T_degC", "Salnty", "O2ml_L", "STheta")
calcofi_filtered <- calcofi_data %>% 
  select(all_of(selected_columns)) %>% 
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
  # Render Interactive Map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -120, lat = 34, zoom = 6)
  })
  
  # Render Time Series Plot
  output$time_series_plot <- renderPlot({
    ggplot(calcofi_filtered, aes(x = Depthm, y = T_degC)) +
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

