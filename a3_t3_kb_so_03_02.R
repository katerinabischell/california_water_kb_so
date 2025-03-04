

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
file_path <- "data/cleaned_bottle2.csv"
# Changed to use read.csv() for CSV files
calcofi_data <- read.csv(file_path) %>% 
  clean_names()  # Convert column names to snake_case

# Load Cast Data for Latitude & Longitude with Clean Column Names
cast_file_path <- "data/cast_cleaned.xlsx"
cast_data <- read_excel(cast_file_path) %>% 
  clean_names() %>% 
  select(sta_id, lat_dec, lon_dec) %>% 
  drop_na()

# Remove duplicate sta_id if necessary
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
    phaeop = as.numeric(phaeop),
    po4u_m = as.numeric(po4u_m),
    si_o3u_m = as.numeric(si_o3u_m),
    no2u_m = as.numeric(no2u_m),
    no3u_m = as.numeric(no3u_m),
    nh3u_m = as.numeric(nh3u_m)
  )

# Merge Hydrographic Data with Cast Data
selected_columns <- c("sta_id", "depthm", "t_deg_c", "salnty", "o2ml_l", 
                      "chlor_a", "phaeop", "po4u_m", "si_o3u_m", "no2u_m", "no3u_m", "nh3u_m")
calcofi_filtered <- calcofi_data %>% 
  select(all_of(selected_columns)) %>% 
  left_join(cast_data, by = "sta_id") %>% 
  filter(!is.na(po4u_m) | !is.na(no3u_m) | !is.na(chlor_a)) %>%  # Ensure data points with phosphate, nitrate, and chlorophyll are included
  head(500)

# Define UI with Introduction Page
ui <- dashboardPage(
  dashboardHeader(title = "California Coastal Water Quality Explorer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("info-circle")),
      menuItem("Map Explorer", tabName = "map", icon = icon("map")),
      menuItem("Data Visualizations", tabName = "scatterplot", icon = icon("chart-line")),
      menuItem("Data Table & Stats", tabName = "data", icon = icon("table")),
      menuItem("Advanced Analysis", tabName = "analysis", icon = icon("cogs"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "intro",
              fluidRow(
                box(width = 12, title = "Introduction", status = "primary",
                    p("Welcome to the California Coastal Water Quality Explorer. This interactive application utilizes the CalCOFI (California Cooperative Oceanic Fisheries Investigations) Bottle Database to analyze oceanographic trends off the California coast. The database contains extensive seawater sample data collected from CalCOFI stations since 1949, providing insights into key oceanographic parameters such as temperature, salinity, dissolved oxygen, chlorophyll-a, and various nutrients. These data are essential for understanding long-term changes in the California Current Ecosystem (CCE) and assessing the impacts of climate variability and El Niño on marine life and coastal communities."),
                    p("This app provides tools for exploring hydrographic data through interactive maps, scatter plots, summary statistics, and advanced analyses. For more details, visit the official CalCOFI Bottle Database: ", a("CalCOFI Bottle Database", href="https://calcofi.org/data/oceanographic-data/bottle-database/", target="_blank"))
                )
              ),
              fluidRow(
                box(width = 12, img(src = "images/coast.jpg", width = "100%", height = "auto"))  # Display coast.jpg image
              )
      ),
      tabItem(
        tabName = "map",
        fluidRow(
          box(
            width = 12, 
            leafletOutput("map"),
            p("CalCOFI’s quarterly cruises span a broad area from north of San Francisco Bay to San Diego, reaching 300 miles (500 km) offshore. These cruises cover both state and national waters, as well as international waters. The CalCOFI survey grid includes a series of transect lines and stations designed to capture oceanographic data across California’s coastal waters. The grid’s flexibility accommodates different survey types, with core patterns that expand or contract depending on the season and research needs. The standard grid involves multiple transects and stations spaced between 20 and 40 nautical miles apart, with additional stations in coastal areas. Special patterns with more stations are sometimes used during specific surveys to monitor larger areas, including extending further north for winter and spring cruises."),
            p("This extensive sampling effort is made possible through a partnership between NOAA’s Fisheries Service, the California Department of Fish & Wildlife, and Scripps Institution of Oceanography, allowing for comprehensive monitoring of ocean conditions and marine ecosystems."),
          )
        ),
        fluidRow(
          box(
            width = 12, 
            img(src = "images/CalCOFI_sampling.jpg", width = "100%", height = "auto")  # Display CalCOFI_sampling.jpg image
          )
        )  # Corrected this line, added the closing bracket here
      ),
      tabItem(
        tabName = "scatterplot",
        fluidRow(
          box(width = 12, plotOutput("scatter_plot")),  # Correct ID here
          box(width = 12, plotOutput("scatter_plot_salinity_temp"))
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
  # Render Interactive Map with Data Points
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
                       "Phosphate:", po4u_m, "μM<br>",
                       "Nitrate:", no3u_m, "μM"),
        radius = 4, color = "blue", fillOpacity = 0.7
      ) %>%
      setView(lng = -120, lat = 34, zoom = 6)
  })
  
  # Render Scatter Plot (Temperature vs. Depth)
  output$scatter_plot <- renderPlot({
    ggplot(calcofi_filtered, aes(x = depthm, y = t_deg_c)) +
      geom_point(color = "blue", alpha = 0.6) +
      geom_smooth(method = "loess", color = "red", se = FALSE) +
      labs(
        title = "Temperature vs. Depth",
        x = "Depth (m)",
        y = "Temperature (°C)",
        caption = "Data from CalCOFI Hydrographic Dataset"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  # Render Salinity vs. Temperature Scatter Plot
  output$scatter_plot_salinity_temp <- renderPlot({
    ggplot(calcofi_filtered, aes(x = salnty, y = t_deg_c)) +
      geom_point(color = "blue", alpha = 0.6) +
      geom_smooth(method = "loess", color = "red", se = FALSE) +
      labs(
        title = "Salinity vs. Temperature",
        x = "Salinity (UNITS)",
        y = "Temperature (°C)",
        caption = "Data from CalCOFI Hydrographic Dataset"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
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