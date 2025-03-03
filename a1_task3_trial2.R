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
           p("Using CalCOFI Bottle table data with parameters like depth, temperature, and oxygen levels."),
           img(src = "images/coast.jpg", height = "300px")),
  
  tabPanel("Interactive Map",
           sidebarLayout(
             sidebarPanel(
               sliderInput("year", "Select Year Range:", min = min(calcofi_bottle$year, na.rm=TRUE), max = max(calcofi_bottle$year, na.rm=TRUE), value = c(2000, 2025)),
               selectInput("marker_label", "Marker Label:", choices = c("Station ID" = "sta_id", "Depth (m)" = "depthm", "Temperature (°C)" = "t_deg_c"))
             ),
             mainPanel(leafletOutput("water_quality_map"))
           )),
  
  tabPanel("Data Table & Summary",
           sidebarLayout(
             sidebarPanel(selectInput("summary_param", "Select Parameter:", choices = names(calcofi_bottle))),
             mainPanel(DTOutput("summary_table"), verbatimTextOutput("summary_stats"))
           ))
)

### Server Section ###
server <- function(input, output, session) {
  output$water_quality_map <- renderLeaflet({
    filtered <- calcofi_bottle %>% filter(year >= input$year[1] & year <= input$year[2])
    if (nrow(filtered) == 0) {
      leaflet() %>% addTiles() %>% addPopups(-119.7, 34.4, "No data available for selected range.")
    } else {
      leaflet(filtered) %>%
        addTiles() %>%
        addCircleMarkers(
          ~lon_dec, ~lat_dec,
          label = ~get(input$marker_label),
          popup = ~paste("Station:", sta_id, "<br>Depth:", depthm, "m<br>Temp:", t_deg_c, "°C"),
          radius = 5, color = "#2b8cbe", opacity = 0.7
        ) %>%
        setView(lng = -119.7, lat = 34.4, zoom = 6)
    }
  })
  
  output$summary_table <- renderDT({
    datatable(calcofi_bottle %>% select(year, sta_id, all_of(input$summary_param)) %>% head(100),
              options = list(pageLength = 10))
  })
  
  output$summary_stats <- renderPrint({
    summary(calcofi_bottle[[input$summary_param]])
  })
}

### Launch App ###
shinyApp(ui = ui, server = server)