# Interactive example: Adjust prior and likelihood
prior <- 0.01#"base rate" of disease
likelihood <- 0.95
specificity <- 0.90

# Marginal probability of positive test
p_positive <- prior * likelihood + (1 - prior) * (1 - specificity)

posterior <- (prior * likelihood) / p_positive

cat("Posterior Probability:", round(posterior, 4))

### Make the Above Into a Shiny App
library(shiny)
ui <- fluidPage(
  titlePanel("Bayesian Updating: Disease Diagnosis"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("prior", "Prior Probability of Disease:", min = 0, max = 1, value = 0.01, step = 0.005),
      sliderInput("likelihood", "Likelihood of Positive Test if Diseased:", min = 0, max = 1, value = 0.95, step = 0.01),
      sliderInput("specificity", "Specificity (Probability of Negative Test if Not Diseased):", min = 0, max = 1, value = 0.90, step = 0.01)
    ),
    mainPanel(
      h3("Posterior Probability of Disease Given Positive Test:"),
      verbatimTextOutput("posteriorOutput")
    )
  )
)
server <- function(input, output) {
  output$posteriorOutput <- renderText({
    prior <- input$prior
    likelihood <- input$likelihood
    specificity <- input$specificity
    
    p_positive <- prior * likelihood + (1 - prior) * (1 - specificity)
    posterior <- (prior * likelihood) / p_positive
    
    paste("Posterior Probability:", round(posterior, 4))
  })
}
shinyApp(ui = ui, server = server)