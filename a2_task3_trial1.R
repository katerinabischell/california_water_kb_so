library(shiny)
library(bslib)
library(leaflet)
library(tidyverse)
library(DT)
library(janitor)


### UI Section ###
ui <- navbarPage(
  theme = bs_theme(bootswatch = "lux", primary = "#b3cde3", secondary = "#ccebc5"),
  title = "California Coastal Water Quality Explorer",
  
  tabPanel("Home",
           h1("Explore California Coastal Water Quality Trends"),
           p("Displaying available data from CalCOFI Cast table only."),
           img(src = "images/coast.jpg", height = "300px")),
  
  tabPanel("Interactive Map",
           sidebarLayout(
             sidebarPanel(sliderInput("year", "Select Year Range:", min = 1949, max = 2025, value = c(2000, 2025))),
             mainPanel(leafletOutput("water_quality_map"))
           )),
  
  tabPanel("Data Table & Summary",
           sidebarLayout(
             sidebarPanel(selectInput("summary_param", "Select Parameter:", choices = names(calcofi_cast))),
             mainPanel(DTOutput("summary_table"), verbatimTextOutput("summary_stats"))
           ))
)

### Server Section ###
server <- function(input, output, session) {
  output$water_quality_map <- renderLeaflet({
    leaflet(data = calcofi_cast) %>%
      addTiles() %>%
      addCircleMarkers(~lon_dec, ~lat_dec, label = ~sta_id, radius = 4, color = "#2c7fb8", opacity = 0.8) %>%
      setView(lng = -119.7, lat = 34.4, zoom = 6)
  })
  
  output$summary_table <- renderDT({
    datatable(calcofi_cast %>%
                select(year, sta_id, all_of(input$summary_param)) %>%
                head(100),
              options = list(pageLength = 10))
  })
  
  output$summary_stats <- renderPrint({
    summary(calcofi_cast[[input$summary_param]])
  })
}

### Launch App ###
shinyApp(ui = ui, server = server)