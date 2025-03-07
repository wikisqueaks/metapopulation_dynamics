library(shiny)

# Source or define all required functions used by metapop()
source("functions.R")  # Ensure this file defines metapop()

ui <- fluidPage(
  titlePanel("Metapopulation Simulation"),
  sidebarLayout(
    sidebarPanel(
      numericInput("x", "Grid width (x):", value = 20, min = 1),
      numericInput("y", "Grid height (y):", value = 20, min = 1),
      numericInput("iterations", "Iterations:", value = 200, min = 1),
      numericInput("starting_proportion", "Starting Proportion:", value = 0.5, min = 0, max = 1, step = 0.01),
      numericInput("colonization_rate", "Colonization Rate:", value = 0.7, min = 0, max = 1, step = 0.01),
      numericInput("extirpation_rate", "Extirpation Rate:", value = 0.3, min = 0, max = 1, step = 0.01),
      actionButton("run", "Run Simulation")
    ),
    mainPanel(
      plotOutput("meta_plot", height = "600px", width = "900px"),
      textOutput("status")  # Status message
    )
  )
)

server <- function(input, output) {
  sim_results <- eventReactive(input$run, {
    withProgress(message = "Running simulation...", value = 0, {
      Sys.sleep(0.5)  # Simulated delay to show progress bar
      metapop(
        x = input$x,
        y = input$y,
        iterations = input$iterations,
        starting_proportion = input$starting_proportion,
        colonization_rate = input$colonization_rate,
        extirpation_rate = input$extirpation_rate
      )
    })
  })
  
  output$meta_plot <- renderPlot({
    sim_results()  # Only runs when button is clicked
  })
  
  output$status <- renderText({
    if (input$run > 0) {
      "Simulation completed!"
    } else {
      "Click 'Run Simulation' to start."
    }
  })
}

shinyApp(ui = ui, server = server)
