#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Beta-Binomial Conjugacy"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("alpha", "Prior α (successes):", min = 1, max = 10, value = 2),
      sliderInput("beta", "Prior β (failures):", min = 1, max = 10, value = 2),
      sliderInput("n", "Number of trials:", min = 1, max = 100, value = 20),
      sliderInput("k", "Number of successes:", min = 0, max = 100, value = 10)
    ),
    mainPanel(
      plotOutput("posteriorPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$posteriorPlot <- renderPlot({
    x <- seq(0, 1, length = 100)
    prior <- dbeta(x, input$alpha, input$beta)
    unif_post <- dbeta(x, input$k + 1, input$n - input$k + 1)
    posterior <- dbeta(x, input$alpha + input$k, input$beta + input$n - input$k)
    
    plot(x, posterior, type = "l", col = "blue", lwd = 2,
         ylab = "Density", xlab = "Probability of Success",
         main = "Beta-Binomial Updating")
    lines(x, prior, col = "red", lwd = 2)
    lines(x, unif_post, col = "green", lwd = 2)
    legend("topright", legend = c("Prior", "Post for U(0,1)", "Posterior"),
           col = c("red", "green", "blue"), lwd = 2)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
