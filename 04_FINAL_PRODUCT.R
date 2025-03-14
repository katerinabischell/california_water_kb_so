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
  head(8500)

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
                      p("CalCOFI initially used Niskin, Nansen, and 'Wally' bottles for sample collection. Since 1993, the primary method has been the CTD-Rosette system (Conductivity, Temperature, and Depth), which allows for precise oceanographic measurements at various depths."),
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
                      tags$div(style = "text-align: center;", 
                               tags$img(src = "coast.jpg", 
                                        alt = "An image of the coast."),
                               p(strong("Created by: Katerina Bischel and Shane O'Brian"), align = "left")),
                  )
                )
        ),
        tabItem(tabName = "map",
                fluidRow(
                  box(width = 12, leafletOutput("map"),
                      p("CalCOFI's quarterly cruises span a broad area from north of San Francisco Bay to San Diego, reaching out 500 km offshore. These cruises cover both state, national, and international waters. The CalCOFI survey grid includes a series of transect lines and stations designed; the grid's flexibility accommodates different survey types, with core patterns that expand or contract depending on the season and research needs. The standard grid involves multiple transects and stations spaced between 20 and 40 nautical miles apart, with additional stations in coastal areas. Special patterns with more stations are sometimes used during specific surveys to monitor larger areas, including extending further north for winter and spring cruises."),
                      tags$img(
                        src = "CalCOFI_sampling.png", 
                        alt = "CalCOFI Sampling Image",
                        style = "width: 300px; height: auto;"),
                      p("A vizualiation of the sampling grid collection method used by CalCOFI. The blue dots represent state and federals waters, and red international waters."),
                      style = "text-align: center;",
                      tags$img(
                        src = "CTD.png",
                        alt = "An image of the rosette frame with sampling bottles arranged inside.",
                        style = "width: 300px; height: auto,"),
                      p("A CTD-Rosette system: the gray cylinders are the water sampling devices, and the rosette is the metal structure the bottles are arranged inside."),
                      style = "text-align: center;",
                      tags$video(
                        src = "ctd_video.mp4",
                        type = "video/mp4",
                        controls = TRUE,
                        width = "100%",
                        height = "auto"),
                      p("A short video providing a more in-depth explaination of a CTD-Rosette set-up. Source: NOAA."),
                      tags$a(
                        href = "https://oceanexplorer.noaa.gov/technology/ctd/ctd.html",
                        target = "_blank",
                        "Click here to visit the NOAA CTD page."
                      ),
                  ),
                ),
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
                                              #"Chlorophyll-a" = "chlor_a", 
                                              "Depth" = "depthm",
                                              "Latitude" = "lat_dec",
                                              "Longitude" = "lon_dec"))),
                  box(width = 12,
                      selectInput("y_var", "Select Y-axis Variable:", 
                                  choices = c("Temperature" = "t_deg_c", 
                                              "Salinity" = "salnty",
                                              "Oxygen" = "o2ml_l",
                                              "Phosphate" = "po4u_m",
                                              #"Chlorophyll-a" = "chlor_a", 
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

# Mapping of variable names to user-friendly display names
variable_mapping <- c(
  "t_deg_c" = "Temperature",
  "salnty" = "Salinity",
  "o2ml_l" = "Oxygen",
  "chlor_a" = "Chlorophyll-a",
  "po4u_m" = "Phosphate",
  "no3u_m" = "Nitrate",
  "depthm" = "Depth",
  "lat_dec" = "Latitude",
  "lon_dec" = "Longitude"
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
    
    # Get the user-friendly display names from the mapping
    x_label <- variable_mapping[input$x_var]
    y_label <- variable_mapping[input$y_var]
    
    # Generate the plot using the selected variables
    ggplot(calcofi_filtered, aes_string(x = input$x_var, y = input$y_var)) +
      geom_point(color = "#2ca25f", alpha = 0.6) +
      geom_smooth(method = "loess", color = "navy", se = FALSE) +
      labs(
        title = paste(x_label, "vs.", y_label),
        x = x_label,
        y = y_label,
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
            hr()
        ),
        box(width = 12, 
            title = "PCA Results",
            plotOutput("pca_plot"),
            verbatimTextOutput("pca_summary")
        ),
        box(width = 12,
            title = "PCA Interpretation",
            uiOutput("pca_interpretation")
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
            hr()
        ),
        box(width = 12, 
            title = "Regression Results",
            verbatimTextOutput("mlr_summary"),
            plotOutput("mlr_plot")
        ),
        box(width = 12,
            title = "Regression Interpretation",
            uiOutput("mlr_interpretation")
        )
      )
    }
  })
  
  # PCA Analysis - Fixed for both ellipse and color scale issues
  observeEvent(input$run_pca, {
    req(input$pca_variables, length(input$pca_variables) >= 2)
    
    # Create data subset for PCA with complete cases
    pca_data <- calcofi_filtered %>%
      select(all_of(input$pca_variables)) %>%
      drop_na()
    
    # Check if we have enough data
    if(nrow(pca_data) < 3) {
      output$pca_plot <- renderPlot({
        plot(0, 0, type = "n", xlab = "", ylab = "", axes = FALSE,
             main = "Insufficient data for PCA after removing NA values")
        text(0, 0, "Not enough complete cases for PCA analysis.\nTry selecting different variables.")
      })
      return()
    }
    
    # Perform PCA
    pca_result <- prcomp(pca_data, scale. = TRUE)
    
    # Plot PCA with a completely fail-safe approach
    output$pca_plot <- renderPlot({
      # Calculate variance explained for axis labels
      var_explained <- round(pca_result$sdev^2 / sum(pca_result$sdev^2) * 100, 1)
      
      # Create a simple biplot using base R graphics (most reliable approach)
      par(mar = c(5, 5, 4, 2))
      
      # Plot the scores (observations)
      plot(pca_result$x[,1], pca_result$x[,2], 
           xlab = paste0("PC1 (", var_explained[1], "%)"),
           ylab = paste0("PC2 (", var_explained[2], "%)"),
           main = "PCA Biplot",
           pch = 16, col = "steelblue")
      
      # Add variable loadings as arrows
      arrows(0, 0, pca_result$rotation[,1] * 5, pca_result$rotation[,2] * 5, 
             length = 0.1, col = "darkred")
      
      # Add variable names
      text(pca_result$rotation[,1] * 5.5, pca_result$rotation[,2] * 5.5, 
           labels = rownames(pca_result$rotation), 
           col = "darkred", font = 2)
      
      # Add a legend explaining the plot
      legend("topright", 
             legend = c("Observations", "Variable loadings"), 
             col = c("steelblue", "darkred"),
             pch = c(16, NA),
             lty = c(NA, 1),
             cex = 0.8)
    })
    
    # Calculate variance explained by each component
    variance_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2) * 100
    cumulative_variance <- cumsum(variance_explained)
    
    # Create loadings table for interpretation
    loadings_data <- as.data.frame(pca_result$rotation)
    loadings_data$Variable <- rownames(loadings_data)
    loadings_data <- loadings_data %>%
      select(Variable, everything()) %>%
      mutate(across(where(is.numeric), round, 3))
    
    output$pca_summary <- renderPrint({
      summary(pca_result)
    })
    
    # Create PCA interpretation output
    output$pca_interpretation <- renderUI({
      tagList(
        h4("PCA Results Interpretation"),
        p("Principal Component Analysis (PCA) finds the main patterns in your data by creating new variables (principal components) that capture the maximum variance."),
        
        h5("Variance Explained:"),
        p("Each principal component (PC) explains a certain amount of the total variance in the data:"),
        tags$ul(
          lapply(1:min(3, length(variance_explained)), function(i) {
            tags$li(paste0("PC", i, " explains ", round(variance_explained[i], 1), 
                           "% of total variance (cumulative: ", 
                           round(cumulative_variance[i], 1), "%)"))
          })
        ),
        p("A higher percentage means the component captures more information from the original variables."),
        
        h5("Variable Contributions:"),
        p("The loadings table below shows how much each original variable contributes to each principal component:"),
        renderTable(loadings_data),
        p("Larger absolute values (positive or negative) indicate stronger contribution to that component."),
        
        h5("How to Interpret the Plot:"),
        tags$ul(
          tags$li("Each point represents a water sample observation"),
          tags$li("Points closer together have similar characteristics across the selected variables"),
          tags$li("The color indicates how well the observation is represented by the first two components (quality of representation)"),
          tags$li("The axes represent the first two principal components, which explain the most variance")
        ),
        
        h5("Oceanographic Implications:"),
        p("In oceanographic studies, PCA can reveal:"),
        tags$ul(
          tags$li("Water mass characteristics and boundaries"),
          tags$li("Relationships between physical properties (temperature, salinity) and chemical properties (nutrients, oxygen)"),
          tags$li("Depth-related patterns and spatial variations"),
          tags$li("Potential environmental gradients influencing water quality")
        )
      )
    })
  })
  
  # Multiple Linear Regression - Enhanced with interpretations
  observeEvent(input$run_mlr, {
    req(input$mlr_response, input$mlr_predictors, length(input$mlr_predictors) >= 1)
    
    # Make sure response isn't in predictors
    valid_predictors <- setdiff(input$mlr_predictors, input$mlr_response)
    
    # Create formula for regression
    formula_text <- paste(input$mlr_response, "~", paste(valid_predictors, collapse = " + "))
    
    # Create data subset with complete cases for selected variables
    mlr_data <- calcofi_filtered %>%
      select(all_of(c(input$mlr_response, valid_predictors))) %>%
      drop_na()
    
    # Run the regression
    mlr_model <- lm(as.formula(formula_text), data = mlr_data)
    
    # Get model summary for interpretation
    model_summary <- summary(mlr_model)
    
    # Display regression summary
    output$mlr_summary <- renderPrint({
      model_summary
    })
    
    # Plot regression diagnostics
    output$mlr_plot <- renderPlot({
      par(mfrow = c(2, 2))
      plot(mlr_model)
    })
    
    # Create coefficients table for easier understanding
    coef_table <- data.frame(
      Variable = names(coef(mlr_model)),
      Coefficient = round(coef(mlr_model), 4),
      Std_Error = round(coef(model_summary)[, "Std. Error"], 4),
      t_value = round(coef(model_summary)[, "t value"], 4),
      p_value = round(coef(model_summary)[, "Pr(>|t|)"], 4),
      Significance = ifelse(coef(model_summary)[, "Pr(>|t|)"] < 0.001, "***",
                            ifelse(coef(model_summary)[, "Pr(>|t|)"] < 0.01, "**",
                                   ifelse(coef(model_summary)[, "Pr(>|t|)"] < 0.05, "*", "")))
    )
    
    # Create MLR interpretation output
    output$mlr_interpretation <- renderUI({
      tagList(
        h4("Multiple Linear Regression Results Interpretation"),
        p("This analysis examines how the selected predictors influence the response variable."),
        
        h5("Model Performance:"),
        tags$ul(
          tags$li(paste0("R-squared: ", round(model_summary$r.squared, 3), 
                         " (This means ", round(model_summary$r.squared * 100, 1), 
                         "% of the variation in ", input$mlr_response, 
                         " is explained by the selected predictors)")),
          tags$li(paste0("Adjusted R-squared: ", round(model_summary$adj.r.squared, 3), 
                         " (A more conservative estimate accounting for the number of predictors)")),
          tags$li(paste0("F-statistic: ", round(model_summary$fstatistic[1], 2), 
                         " with p-value: ", 
                         ifelse(model_summary$fstatistic[1] > 0, 
                                format.pval(pf(model_summary$fstatistic[1], 
                                               model_summary$fstatistic[2], 
                                               model_summary$fstatistic[3], 
                                               lower.tail = FALSE), digits = 4), 
                                "N/A")))
        ),
        
        h5("Coefficient Interpretation:"),
        renderTable(coef_table),
        p("How to interpret the coefficients:"),
        tags$ul(
          tags$li("The Intercept represents the expected value of the response when all predictors are zero (if meaningful)"),
          tags$li("Each coefficient shows the expected change in the response variable when the predictor increases by one unit, holding all other predictors constant"),
          tags$li("Positive coefficients indicate a positive relationship; negative coefficients indicate an inverse relationship"),
          tags$li("P-values indicate statistical significance - generally values < 0.05 are considered significant (*)"),
          tags$li("The smaller the p-value, the stronger the evidence that the predictor is important (*** < 0.001, ** < 0.01, * < 0.05)")
        ),
        
        h5("Diagnostic Plot Interpretation:"),
        tags$ul(
          tags$li(strong("Residuals vs Fitted:"), " Points should be randomly scattered around the horizontal line. Patterns indicate potential non-linear relationships."),
          tags$li(strong("Normal Q-Q:"), " Points should follow the diagonal line. Deviations indicate non-normal residuals."),
          tags$li(strong("Scale-Location:"), " Points should be randomly scattered with a constant spread. Patterns indicate heteroscedasticity."),
          tags$li(strong("Residuals vs Leverage:"), " Identifies influential observations. Points outside the dashed lines may have high influence on the model.")
        ),
        
        h5("Oceanographic Implications:"),
        p("In oceanographic studies, MLR can help:"),
        tags$ul(
          tags$li("Identify the primary factors influencing water quality parameters"),
          tags$li("Quantify the relative importance of different physical and chemical drivers"),
          tags$li("Predict how changes in environmental conditions might affect water properties"),
          tags$li("Understand the complex interplay between spatial factors (latitude, longitude), depth, and water chemistry")
        )
      )
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
