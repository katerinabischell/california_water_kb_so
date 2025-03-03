library(shiny)
library(bslib)
library(leaflet)
library(tidyverse)

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
                           choices = c("Temperature", "Salinity", "Oxygen", "Phosphate", "Chlorophyll")),
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
               selectInput("time_var", "Select Variable:", choices = c("Oxygen", "Temperature", "Salinity")),
               sliderInput("time_range", "Year Range:", min = 1950, max = 2025, value = c(2000, 2025))
             ),
             mainPanel(plotOutput("time_series_plot"))
           )
  ),
  
  tabPanel("Data Table & Summary",
           sidebarLayout(
             sidebarPanel(
               selectInput("summary_param", "Select Parameter:",
                           choices = c("Oxygen", "Temperature", "Salinity", "Phosphate", "Chlorophyll")),
               actionButton("summary_btn", "Show Summary")
             ),
             mainPanel(
               h3("Summary Table"),
               tableOutput("summary_table"),
               h3("Summary Statistics"),
               verbatimTextOutput("summary_stats")
             )
           )
  ),
  
  tabPanel("Advanced Analysis",
           sidebarLayout(
             sidebarPanel(
               selectInput("analysis_type", "Choose Analysis:",
                           choices = c("Multiple Linear Regression", "Principal Component Analysis (PCA)", "Anomaly Detection")),
               actionButton("run_analysis", "Run Analysis")
             ),
             mainPanel(
               h3("Analysis Output"),
               plotOutput("analysis_plot"),
               verbatimTextOutput("analysis_summary")
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
    plot(1:10, 1:10, type = "l", col = "blue",
         main = paste("Time Series of", input$time_var),
         xlab = "Year", ylab = input$time_var)
  })
  
  output$summary_table <- renderTable({
    head(mtcars)
  })
  
  output$summary_stats <- renderPrint({
    summary(mtcars)
  })
  
  output$analysis_plot <- renderPlot({
    plot(rnorm(100), rnorm(100), col = "green",
         main = input$analysis_type,
         xlab = "X-axis", ylab = "Y-axis")
  })
  
  output$analysis_summary <- renderPrint({
    print(paste(input$analysis_type, "summary placeholder"))
  })
}

### Combine into Shiny App ###
shinyApp(ui = ui, server = server)