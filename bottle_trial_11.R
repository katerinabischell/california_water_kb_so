library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(leaflet)
library(readxl)
library(dplyr)
library(tidyr)    # Added for drop_na() function
library(janitor)  # For automatic column name cleaning

# Load Data with Clean Column Names
file_path <- "data/cleaned_bottle.csv"
calcofi_data <- read.csv(file_path) %>% 
  clean_names()

# Load Cast Data for Latitude & Longitude with Clean Column Names
cast_file_path <- "data/cast_cleaned.xlsx"
cast_data <- read_excel(cast_file_path) %>% 
  clean_names() %>% 
  select(sta_id, lat_dec, lon_dec) %>% 
  drop_na()

# Remove duplicate sta_id
cast_data <- cast_data %>% 
  group_by(sta_id) %>%
  slice(1) %>%
  ungroup()

# Ensure proper numeric conversion for key environmental variables
calcofi_data <- calcofi_data %>%
  mutate(
    depthm = as.numeric(depthm),
    t_deg_c = as.numeric(t_deg_c),
    salnty = as.numeric(salnty),
    o2ml_l = as.numeric(o2ml_l),
    chlor_a = as.numeric(chlor_a),
    po4u_m = as.numeric(po4u_m),
    no3u_m = as.numeric(no3u_m)
  )

# Merge Hydrographic Data with Cast Data
calcofi_filtered <- calcofi_data %>% 
  select(sta_id, depthm, t_deg_c, salnty, o2ml_l, chlor_a, po4u_m, no3u_m) %>% 
  left_join(cast_data, by = "sta_id") %>% 
  filter(!is.na(po4u_m) | !is.na(no3u_m) | !is.na(chlor_a)) %>%
  head(500)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "California Coastal Water Quality Explorer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("info-circle")),
      menuItem("Map Explorer", tabName = "map", icon = icon("map")),
      menuItem("Graphs", tabName = "graphs", icon = icon("chart-line")),
      menuItem("Data Table & Stats", tabName = "data", icon = icon("table")),
      menuItem("Advanced Analysis", tabName = "analysis", icon = icon("cogs"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "intro",
              fluidRow(
                box(width = 12, title = "Introduction", status = "primary",
                    p("Welcome to the California Coastal Water Quality Explorer. This interactive application utilizes the CalCOFI Bottle Database to analyze oceanographic trends off the California coast."),
                    p("The Bottle Database contains seawater sample data collected from CalCOFI stations, spanning from 1949 to the present. Key parameters include temperature, salinity, dissolved oxygen, chlorophyll-a, and various nutrients."),
                    p("CalCOFI initially used Niskin, Nansen, and 'Wally' bottles for sample collection. Since 1993, the primary method has been the CTD-Rosette system, which allows for precise oceanographic measurements at various depths."),
                    p("This app provides tools for exploring hydrographic data through interactive maps, scatter plots, summary statistics, and advanced analyses."),
                    p("For more details, visit the official CalCOFI Bottle Database: ", a("CalCOFI Bottle Database", href="https://calcofi.org/data/oceanographic-data/bottle-database/", target="_blank"))
                )
              )
      ),
      tabItem(tabName = "map",
              fluidRow(
                box(width = 12, leafletOutput("map"))
              )
      ),
      tabItem(tabName = "graphs",
              fluidRow(
                box(width = 12,
                    selectInput("graph_type", "Select Graph:", 
                                choices = list("Temperature vs. Depth" = "temp_depth", 
                                               "Phosphate vs. Oxygen" = "phosphate_oxygen"))),
                box(width = 12, plotOutput("selected_graph"))
              )
      ),
      tabItem(tabName = "data",
              fluidRow(
                box(width = 12, DTOutput("data_table")),
                box(width = 6, verbatimTextOutput("summary_stats"))
              )
      ),
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
    leaflet(calcofi_filtered) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~lon_dec, lat = ~lat_dec,
        popup = ~paste(
          "Station ID:", sta_id, "<br>",
          "Depth:", depthm, "m<br>",
          "Temp:", t_deg_c, "°C<br>",
          "Salinity:", salnty, "<br>",
          "Oxygen:", o2ml_l, "ml/L<br>",
          "Chlorophyll-a:", ifelse(is.na(chlor_a), "NA", chlor_a), "<br>",
          "Phosphate:", po4u_m, "μM<br>",
          "Nitrate:", ifelse(is.na(no3u_m), "NA", no3u_m), "μM"
        ),
        radius = 4, color = "#8856a7", fillOpacity = 0.7
      ) %>%
      setView(lng = -120, lat = 34, zoom = 6)
  })
  
  # Render Selected Graph
  output$selected_graph <- renderPlot({
    if (input$graph_type == "temp_depth") {
      ggplot(calcofi_filtered, aes(x = depthm, y = t_deg_c)) +
        geom_point(color = "#2ca25f", alpha = 0.6) +
        geom_smooth(method = "loess", color = "#43a2ca", se = FALSE) +
        labs(
          title = "Temperature vs. Depth",
          x = "Depth (m)",
          y = "Temperature (°C)",
          caption = "Linear regression trend shown in blue, Data from CalCOFI Hydrographic Dataset"
        ) +
        theme_minimal()
    } else {
      ggplot(calcofi_filtered, aes(x = po4u_m, y = o2ml_l)) +
        geom_point(color = "#2ca25f", alpha = 0.6) +
        geom_smooth(method = "lm", color = "#43a2ca", se = FALSE) +
        labs(
          title = "Phosphate vs. Oxygen",
          x = "Phosphate (μM)",
          y = "Oxygen (ml/L)",
          caption = "Linear regression trend shown in blue, Data from CalCOFI Hydrographic Dataset"
        ) +
        theme_minimal()
    }
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
