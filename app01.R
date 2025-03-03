library(tidyverse)
library(palmerpenguins)
library(shiny)

### Katerina Bischel

### create the interface
ui <- fluidPage(
  titlePanel("California Coastal Water Quality Explorer"),
  sidebarLayout(
    sidebarPanel('put my widgets here',
                 radioButtons(
                   inputId = 'penguin_species',
                   label = "Choose Penguin Species",
                   choices = c('Adelie', 'Gentoo', 'Cool Chinstrap Penguins!' = 'Chinstrap')
                 ),
                 selectInput(inputId = 'pt_color',
                             label = 'Select point color',
                             choices = c('Roses are red!' = 'red',
                                         'Violets are purple!' = 'purple',
                                         'Oranges are ...' = 'orange'))
    ),
    mainPanel('put my graph here',
              plotOutput(outputId = 'penguin_plot'),
              h3('Summary table'), 
              tableOutput(outputId = 'penguin_table'))
  )
)

### create the server function
server <- function(input, output) {
  penguin_select <- reactive({
    penguins |> 
      filter(species == input$penguin_species) |> 
      drop_na(flipper_length_mm, body_mass_g)
  })
  
  output$penguin_plot <- renderPlot({
    ggplot(data = penguin_select()) +
      geom_point(aes(x = flipper_length_mm, y = body_mass_g), color = input$pt_color) +
      labs(
        title = paste(input$penguin_species, "Penguins: Flipper Length vs. Body Mass"),
        x = "Flipper Length (mm)",
        y = "Body Mass (g)"
      ) +
      theme_minimal()
  })
  
  penguin_sum_table <- reactive({
    penguins |> 
      filter(species == input$penguin_species) |> 
      group_by(sex) |> 
      summarize(mean_flipper = mean(flipper_length_mm, na.rm = TRUE),
                mean_mass = mean(body_mass_g, na.rm = TRUE))
  })
  
  output$penguin_table <- renderTable({
    penguin_sum_table()
  })
}

### combine them into an app:
shinyApp(ui = ui, server = server)