library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(leaflet)
library(readxl)      # For Excel files
library(dplyr)
library(janitor)     # For clean column names
library(tidyr)       # Fix missing function issue
library(data.table)  # For efficient filtering

# ✅ Increase Memory Allocation (Mac-Specific)
Sys.setenv(R_MAX_VSIZE = 50 * 1024^3)

# ✅ Load Data Efficiently
calcofi_data <- fread("data/cleaned_bottle.csv") %>%
  clean_names() %>%
  mutate(across(where(is.character), ~ suppressWarnings(as.numeric(.))))  # Fix coercion errors

# ✅ Load Cast Data for Latitude & Longitude
cast_file_path <- "data/cast_cleaned.xlsx"
cast_data <- read_excel(cast_file_path) %>%
  clean_names() %>%
  select(sta_id, lat_dec, lon_dec) %>%
  mutate(sta_id = as.character(sta_id)) %>%  # Ensure same type for merging
  drop_na()

# ✅ Convert `sta_id` in calcofi_data for Safe Merge
calcofi_data <- calcofi_data %>%
  mutate(sta_id = as.character(sta_id))

# ✅ Merge Latitude & Longitude with Main Dataset
calcofi_filtered <- left_join(calcofi_data, cast_data, by = "sta_id")

# ✅ Filter to Remove Rows where ALL Key Variables are NA
calcofi_filtered <- calcofi_filtered %>%
  filter(rowSums(!is.na(select(., o2ml_l, chlor_a, phaeop, po4u_m, 
                               si_o3u_m, no2u_m, no3u_m, nh3u_m))) > 0)

# ✅ Convert to DataFrame to Reduce Memory Usage
calcofi_filtered <- as.data.frame(calcofi_filtered)

# 🔹 Define UI
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
      # 🔹 Interactive Map Tab
      tabItem(tabName = "map",
              fluidRow(
                box(width = 12, leafletOutput("map"))
              )
      ),
      # 🔹 Time Series Graphs Tab
      tabItem(tabName = "timeseries",
              fluidRow(
                box(width = 12, plotOutput("time_series_plot"))
              )
      ),
      # 🔹 Data Table & Summary Stats
      tabItem(tabName = "data",
              fluidRow(
                box(width = 12, DTOutput("data_table")),
                box(width = 6, verbatimTextOutput("summary_stats"))
              )
      ),
      # 🔹 Advanced Analysis
      tabItem(tabName = "analysis",
              fluidRow(
                box(width = 12, h4("Coming Soon: PCA & Multiple Linear Regression"))
              )
      )
    )
  )
)

# 🔹 Define Server
server <- function(input, output, session) {
  # ✅ Render Interactive Map with Data Points
  output$map <- renderLeaflet({
    leaflet(calcofi_filtered) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~lon_dec, lat = ~lat_dec,
        popup = ~paste("Station ID:", sta_id, "<br>",
                       "Depth:", depthm, "m<br>",
                       "Temp:", t_deg_c, "°C<br>",
                       "Salinity:", salnty, "<br>",
                       "Oxygen:", o2ml_l, "ml/L<br>",
                       "Chlorophyll-a:", chlor_a, "<br>",
                       "Phaeopigments:", phaeop, "<br>",
                       "Phosphate:", po4u_m, "μM<br>",
                       "Silicate:", si_o3u_m, "μM<br>",
                       "Nitrite:", no2u_m, "μM<br>",
                       "Nitrate:", no3u_m, "μM<br>",
                       "Ammonia:", nh3u_m, "μM"),
        radius = 4, color = "blue", fillOpacity = 0.7
      ) %>%
      setView(lng = -120, lat = 34, zoom = 6)
  })
  
  # ✅ Render Time Series Plot
  output$time_series_plot <- renderPlot({
    ggplot(calcofi_filtered, aes(x = depthm, y = t_deg_c)) +
      geom_line(color = "blue") +
      labs(title = "Temperature vs. Depth", x = "Depth (m)", y = "Temperature (°C)")
  })
  
  # ✅ Render Data Table
  output$data_table <- renderDT({
    datatable(calcofi_filtered)
  })
  
  # ✅ Render Summary Statistics
  output$summary_stats <- renderPrint({
    summary(calcofi_filtered)
  })
}

# 🔹 Run the Application
shinyApp(ui, server)


