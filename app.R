library(shiny)
library(bslib)
library(leaflet)
library(tidyverse)
library(lubridate)   # for date parsing if needed

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
               sliderInput("year", "Select Year Range:", 
                           min = 1950, max = 2025, value = c(2000, 2025)),
               
               # Instead of the fixed 5 parameters, we will fill from server 
               # with actual unique parameters from your CSV.
               selectInput("parameter", "Select Parameter:", choices = NULL),
               
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
               # We'll keep these as placeholders
               selectInput("time_var", "Select Variable:", 
                           choices = c("Oxygen", "Temperature", "Salinity")),
               sliderInput("time_range", "Year Range:", 
                           min = 1950, max = 2025, value = c(2000, 2025))
             ),
             mainPanel(plotOutput("time_series_plot"))
           )
  ),
  
  tabPanel("Data Table & Summary",
           sidebarLayout(
             sidebarPanel(
               # Placeholders for demonstration
               selectInput("summary_param", "Select Parameter:",
                           choices = c("Oxygen", "Temperature", "Salinity", 
                                       "Phosphate", "Chlorophyll")),
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
                           choices = c("Multiple Linear Regression", 
                                       "Principal Component Analysis (PCA)",
                                       "Anomaly Detection")),
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
  
  # 1. Read in your biological data CSV
  biological_data <- read.csv("data/biological_data.csv", stringsAsFactors = FALSE)
  
  
  # 2. Clean/parse relevant columns
  #    - Convert ActivityStartDate to a proper date if it’s in YYYY-MM-DD
  #      or at least extract the year. 
  #    - Convert lat/lon columns to numeric.
  #    - You may also want to rename "CharacteristicName" to "Parameter" 
  #      or "ResultMeasureValue" to "Value" for clarity.
  #    - Check the actual column names in your CSV snippet to make sure 
  #      they match (ActivityLocation/LatitudeMeasure, CharacteristicName, etc.).
  
  bio_data_clean <- biological_data %>%
    mutate(
      Year = as.numeric(substr(ActivityStartDate, 1, 4)),
      lat  = as.numeric(ActivityLocation.LatitudeMeasure),
      lon  = as.numeric(ActivityLocation.LongitudeMeasure),
      Parameter = CharacteristicName,              # e.g. "Enterococcus"
      Value = as.numeric(ResultMeasureValue)       # e.g. 10 cfu/100mL
    ) %>%
    # Filter out rows that have no lat/lon or no parameter:
    filter(!is.na(lat), !is.na(lon), !is.na(Parameter))
  
  # 3. Populate the Parameter dropdown *dynamically* from the CSV
  observe({
    updateSelectInput(session, "parameter", 
                      choices = sort(unique(bio_data_clean$Parameter)))
  })
  
  # 4. Reactive subset based on user’s year slider + parameter selection
  filtered_data <- reactive({
    req(bio_data_clean)
    d <- filter(bio_data_clean,
                Year >= input$year[1], 
                Year <= input$year[2],
                Parameter == input$parameter)
    d
  })
  
  # 5. Base map rendering
  output$water_quality_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -119.7, lat = 34.4, zoom = 6)
  })
  
  # 6. Update map markers when user clicks "Update Map"
  observeEvent(input$update_map, {
    data_to_plot <- filtered_data()
    
    # We'll make a simple color palette based on 'Value' 
    # (e.g., cfu/100mL if your data is bacteria). 
    # If Value is not numeric or you have a lot of zeros, 
    # you might need to adjust logic.
    pal <- colorNumeric(
      palette = "viridis",
      domain = data_to_plot$Value,
      na.color = "transparent"
    )
    
    leafletProxy("water_quality_map") %>%
      clearMarkers() %>%
      addCircleMarkers(
        data = data_to_plot,
        lat = ~lat,
        lng = ~lon,
        color = ~pal(Value),
        fillColor = ~pal(Value),
        fillOpacity = 0.7,
        radius = 6,
        popup = ~paste(
          "<strong>Year:</strong>", Year, "<br/>",
          "<strong>Parameter:</strong>", Parameter, "<br/>",
          "<strong>Value:</strong>", Value
        )
      )
  })
  
  # ---- Time Series Plot (placeholder) ----
  output$time_series_plot <- renderPlot({
    plot(1:10, 1:10, type = "l", col = "blue",
         main = paste("Time Series of", input$time_var),
         xlab = "Year", ylab = input$time_var)
  })
  
  # ---- Data Table & Summary (placeholders) ----
  output$summary_table <- renderTable({
    head(mtcars)
  })
  
  output$summary_stats <- renderPrint({
    summary(mtcars)
  })
  
  # ---- Advanced Analysis (placeholders) ----
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
