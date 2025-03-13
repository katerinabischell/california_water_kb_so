library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(leaflet)
library(readxl)
library(dplyr)
library(tidyr)    # Added for drop_na() function
library(janitor)  # For automatic column name cleaning
library(FactoMineR) # For PCA analysis
library(factoextra) # For PCA visualization
library(shinythemes)  # Added for custom theme

# Load Data with Clean Column Names
file_path <- "data/cleaned_bottle2.csv"
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
  mutate(across(c(depthm, t_deg_c, salnty, o2ml_l, chlor_a, po4u_m, no3u_m), as.numeric))

# Merge Hydrographic Data with Cast Data
calcofi_filtered <- calcofi_data %>% 
  select(sta_id, depthm, t_deg_c, salnty, o2ml_l, chlor_a, po4u_m, no3u_m) %>% 
  left_join(cast_data, by = "sta_id") %>% 
  filter(!is.na(po4u_m) | !is.na(no3u_m) | !is.na(chlor_a)) %>%
  head(500)

# Define UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(tags$style(HTML('
    /* Custom Header */
    .main-header .navbar { background-color: #3690c0 !important; }
    .main-header .logo { background-color: #a6bddb !important; font-size: 18px; }
    
    /* Custom Sidebar */
    .main-sidebar { background-color: #034e7b !important; }
    .main-sidebar .sidebar-menu > li.active > a {
      background-color: #0570b0 !important;
      color: white !important;
    }
    .main-sidebar .sidebar-menu > li > a {
      color: #ece7f2 !important;
    }
    .main-sidebar .sidebar-menu > li > a:hover {
      background-color: #a6bddb !important;
    }
  '))),
  dashboardPage(
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
                      p("CalCOFI initially used Niskin, Nansen, and 'Wally' bottles for sample collection. Since 1993, the primary method has been the CTD-Rosette system (Conductivity, Temperature, and Depth; a rosette is the metal structure the bottles are arranged inside; see image below), which allows for precise oceanographic measurements at various depths."),
                      p("This app provides tools for exploring hydrographic data through interactive maps, scatter plots, summary statistics, and advanced analyses."),
                      p("Instructions:"),
                      tags$ul(
                        tags$li("Use the 'Map Explorer' tab to view data points on an interactive map."),
                        tags$li("Explore relationships between variables in the 'Graphs' tab."),
                        tags$li("View detailed data and statistics in the 'Data Table & Stats' tab."),
                        tags$li("Run advanced PCA and Multiple Linear Regression (MLR) analyses in the 'Advanced Analysis' tab.")
                      ),
                      p("For more details, visit the official CalCOFI Bottle Database: ", a("CalCOFI Bottle Database", href="https://calcofi.org/data/oceanographic-data/bottle-database/", target="_blank")),
                      hr(),
                      tags$img(src = "coast.jpg", 
                               alt = "A image of the coast."),
                      p(strong("Created by: Katerina Bischel and Shane O'Brian"), align = "left")
                  )
                )
        ),
        tabItem(tabName = "map",
                fluidRow(
                  box(width = 12, leafletOutput("map"),
                      p("CalCOFI’s quarterly cruises span a broad area from north of San Francisco Bay to San Diego, reaching out 500 km offshore. These cruises cover both state, national, and international waters. The CalCOFI survey grid includes a series of transect lines and stations designed; the grid’s flexibility accommodates different survey types, with core patterns that expand or contract depending on the season and research needs. The standard grid involves multiple transects and stations spaced between 20 and 40 nautical miles apart, with additional stations in coastal areas. Special patterns with more stations are sometimes used during specific surveys to monitor larger areas, including extending further north for winter and spring cruises."),
                      tags$img(src = "CalCOFI_sampling.png", 
                               alt = "A map of CalCOFI's sampling methodology."),
                      tags$img(src = "CTD.png", 
                               alt = "A image of the rosette frame with sampling bottles arranged inside."),
                      p("This extensive sampling effort is made possible through a partnership between NOAA’s Fisheries Service, the California Department of Fish & Wildlife, and Scripps Institution of Oceanography."),
                  ),
                )
        ),
        tabItem(tabName = "graphs",
                fluidRow(
                  box(width = 12,
                      p("Choose variables to explore the relationships between them via visualizations."),
                      selectInput("x_var", "Select X-axis Variable:", 
                                  choices = c("Temperature" = "t_deg_c", 
                                              "Salinity" = "salnty",
                                              "Oxygen" = "o2ml_l",
                                              "Phosphate" = "po4u_m",
                                              "Chlorophyll-a" = "chlor_a", 
                                              "Depth" = "depthm",
                                              "Latitude" = "lat_dec",
                                              "Longitude" = "lon_dec"))),
                  box(width = 12,
                      selectInput("y_var", "Select Y-axis Variable:", 
                                  choices = c("Temperature" = "t_deg_c", 
                                              "Salinity" = "salnty",
                                              "Oxygen" = "o2ml_l",
                                              "Phosphate" = "po4u_m",
                                              "Chlorophyll-a" = "chlor_a", 
                                              "Depth" = "depthm",
                                              "Latitude" = "lat_dec",
                                              "Longitude" = "lon_dec"))),
                  box(width = 12, plotOutput("selected_graph"))
                )
        ),
        tabItem(tabName = "data",
                fluidRow(
                  box(width = 12,
                      selectInput("data_view", "Select View:", 
                                  choices = list("Data Table" = "table", 
                                                 "Summary Statistics" = "stats"))),
                  box(width = 12, uiOutput("selected_data_view"))
                )
        ),
        tabItem(tabName = "analysis",
                fluidRow(
                  box(width = 12, title = "Advanced Analysis Options",
                      p("Use these advanced statistical techniques to explore relationships in your data:"),
                      tags$ul(
                        tags$li(strong("Principal Component Analysis (PCA):"), " Reduces the dimensionality of data while preserving as much information as possible. Use PCA to identify key patterns and relationships between multiple variables simultaneously."),
                        tags$li(strong("MLR:"), " Models the relationship between a response variable and predictor variables. Use MLR to understand which factors most strongly influence a particular water quality parameter.")
                      ),
                      selectInput("analysis_type", "Select Analysis Type:", 
                                  choices = list("PCA" = "pca", 
                                                 "MLR" = "mlr"))),
                  uiOutput("analysis_ui")
                )
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
    req(input$x_var, input$y_var)  # Ensure both x and y variables are selected
    
    # Check if the selected variables exist in the dataset
    if (!input$x_var %in% colnames(calcofi_filtered) || !input$y_var %in% colnames(calcofi_filtered)) {
      return(NULL)  # Return nothing if the columns are invalid
    }
    
    # Generate the plot using the selected variables
    ggplot(calcofi_filtered, aes_string(x = input$x_var, y = input$y_var)) +
      geom_point(color = "#2ca25f", alpha = 0.6) +
      geom_smooth(method = "loess", color = "#43a2ca", se = FALSE) +
      labs(
        title = paste(input$x_var, "vs.", input$y_var),
        x = input$x_var,
        y = input$y_var,
        caption = "Data from CalCOFI Hydrographic Dataset."
      ) +
      theme_minimal()
  })
  
  # Render Selected Data View
  output$selected_data_view <- renderUI({
    if (input$data_view == "table") {
      DTOutput("data_table")
    } else {
      verbatimTextOutput("summary_stats")
    }
  })
  
  output$data_table <- renderDT({
    datatable(calcofi_filtered, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$summary_stats <- renderPrint({
    summary(calcofi_filtered)
  })
  
  # Render appropriate UI for selected analysis
  output$analysis_ui <- renderUI({
    if (input$analysis_type == "pca") {
      tagList(
        box(width = 12,
            p("Principal Component Analysis (PCA) is a technique that identifies patterns in multidimensional datasets by finding the directions of maximum variance."),
            p("Select at least 2 variables from the list below to include in your analysis."),
            checkboxGroupInput("pca_variables", "Select Variables for PCA:",
                               choices = c("Depth" = "depthm", 
                                           "Temperature" = "t_deg_c",
                                           "Salinity" = "salnty",
                                           "Oxygen" = "o2ml_l",
                                           "Phosphate" = "po4u_m",
                                           "Latitude" = "lat_dec",
                                           "Longitude" = "lon_dec"),
                               selected = c("depthm", "t_deg_c", "salnty", "o2ml_l")),
            actionButton("run_pca", "Run PCA", class = "btn-primary"),
            hr(),
            plotOutput("pca_plot"),
            verbatimTextOutput("pca_summary")
        )
      )
    } else {
      tagList(
        box(width = 12,
            p("MLR allows you to model the relationship between a response variable (the outcome) and multiple predictor variables. It helps us understand the relationship between the outcome and these factors."),
            p("Select one response variable and at least one predictor variable. Do not include your response variable in your predictors."),
            selectInput("mlr_response", "Select Response Variable:",
                        choices = c("Temperature" = "t_deg_c",
                                    "Salinity" = "salnty",
                                    "Oxygen" = "o2ml_l",
                                    "Phosphate" = "po4u_m")),
            checkboxGroupInput("mlr_predictors", "Select Predictor Variables:",
                               choices = c("Depth" = "depthm", 
                                           "Latitude" = "lat_dec",
                                           "Longitude" = "lon_dec",
                                           "Temperature" = "t_deg_c",
                                           "Salinity" = "salnty",
                                           "Oxygen" = "o2ml_l",
                                           "Phosphate" = "po4u_m"),
                               selected = c("depthm", "lat_dec", "lon_dec")),
            actionButton("run_mlr", "Run Regression", class = "btn-primary"),
            hr(),
            verbatimTextOutput("mlr_summary"),
            plotOutput("mlr_plot"),
        )
      )
    }
  })
  
  # PCA Analysis
  observeEvent(input$run_pca, {
    req(input$pca_variables, length(input$pca_variables) >= 2)
    
    # Create data subset for PCA with complete cases
    pca_data <- calcofi_filtered %>%
      select(all_of(input$pca_variables)) %>%
      drop_na()
    
    pca_result <- prcomp(pca_data, scale = TRUE)
    
    # Plot PCA
    output$pca_plot <- renderPlot({
      fviz_pca_ind(pca_result, geom = "point", col.ind = "cos2", 
                   palette = "jco", addEllipses = TRUE, legend.title = "Cos2")
    })
    
    output$pca_summary <- renderPrint({
      summary(pca_result)
    })
  })
  
  # MLR Analysis
  observeEvent(input$run_mlr, {
    req(input$mlr_response, input$mlr_predictors)
    
    # Create formula for MLR
    formula <- as.formula(paste(input$mlr_response, "~", paste(input$mlr_predictors, collapse = "+")))
    mlr_model <- lm(formula, data = calcofi_filtered)
    
    output$mlr_summary <- renderPrint({
      summary(mlr_model)
    })
    
    output$mlr_plot <- renderPlot({
      plot(mlr_model)
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

