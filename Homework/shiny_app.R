# Load required libraries 
library(shiny) 
library(ggplot2) 
# Define UI
ui <- fluidPage(
  titlePanel("Interactive Histogram with ggplot2"),
  sidebarLayout(
    sidebarPanel(
      selectInput("var", "Choose a Variable:",
                  choices = c("MPG" = "mpg", "Horsepower" = "hp",
                              "Weight" = "wt"),
                  selected = "mpg")
    ),
    mainPanel(
      plotOutput("histPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$histPlot <- renderPlot({
    ggplot(mtcars, aes(x = .data[[input$var]])) +  # Updated here
      geom_histogram(fill = "blue", color = "black", bins = 10) +
      labs(title = paste("Histogram of", input$var), x = input$var, y = "Count")
  })
}

# Run the app
shinyApp(ui = ui, server = server)