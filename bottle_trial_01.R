library(shiny)
library(bslib)
library(leaflet)
library(tidyverse)
library(readxl)

# Load dataset
bottle_data <- read_excel("data/bottle.xlsx") %>%
  select(Depthm, T_degC, Salnty, STheta) %>%
  drop_na()

### UI Section ###
ui <- navbarPage(
  theme = bs_theme(
    bootswatch = "flatly", 
    primary = "#b3cde3", 
    secondary = "black", 
    font_scale = 1.2
  ),
  title = "California Coastal Water Quality Explorer",
  
  tabPanel("Home",
           h1("Welcome to the California Coastal Water Quality Explorer"),
           p("Explore water quality trends off the California coast using CalCOFI and EPA/USGS datasets."),
           img(src = "images/coast.jpg", height = "300px"),
           h2("About the Data"),
           p("This app provides interactive visualizations and analyses of oceanographic changes and pollution levels over time.")
  ),
  
  tabPanel("Interactive Map",
           sidebarLayout(
             sidebarPanel(
               sliderInput("year", "Select Year Range:", min = 1950, max = 2025, value = c(2000, 2025)),
               selectInput("parameter", "Water Quality Parameter:",
                           choices = c("Temperature" = "T_degC", "Salinity" = "Salnty")),
               actionButton("update_map", "Update Map")
             ),
             mainPanel(
               leafletOutput("water_quality_map")
             )
           )
  ),
  
  tabPanel("Time Series Analysis",
           sidebarLayout(
             sidebarPanel(
               selectInput("time_var", "Select Variable:", choices = c("Temperature" = "T_degC", "Salinity" = "Salnty")),
               sliderInput("time_range", "Year Range:", min = 1950, max = 2025, value = c(2000, 2025))
             ),
             mainPanel(plotOutput("time_series_plot"))
           )
  ),
  
  tabPanel("Data Table & Summary",
           sidebarLayout(
             sidebarPanel(
               selectInput("summary_param", "Select Parameter:",
                           choices = c("Temperature" = "T_degC", "Salinity" = "Salnty")),
               actionButton("summary_btn", "Show Summary")
             ),
             mainPanel(
               h3("Summary Table"),
               tableOutput("summary_table"),
               h3("Summary Statistics"),
               verbatimTextOutput("summary_stats")
             )
           )
  )
)

### Server Section ###
server <- function(input, output, session) {
  
  output$water_quality_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -119.7, lat = 34.4, zoom = 6)
  })
  
  output$time_series_plot <- renderPlot({
    ggplot(bottle_data, aes(x = Depthm, y = get(input$time_var))) +
      geom_line(color = "blue") +
      labs(title = paste("Time Series of", input$time_var), x = "Depth (m)", y = input$time_var) +
      theme_minimal()
  })
  
  output$summary_table <- renderTable({
    head(bottle_data)
  })
  
  output$summary_stats <- renderPrint({
    summary(bottle_data[[input$summary_param]])
  })
}

### Combine into Shiny App ###
shinyApp(ui = ui, server = server)
