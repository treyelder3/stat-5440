# app.R
library(shiny)

ui <- fluidPage(
  titlePanel("Prior Predictive Distribution: Binomial(n, θ) with Beta Priors"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("n", "Binomial trials (n):", min = 1, max = 200, value = 20, step = 1),
      
      radioButtons(
        "prior_choice",
        "Choose a prior for θ:",
        choices = c(
          "Subjective: Beta(5, 2)" = "subj",
          "Weakly-informative: Beta(2, 2)" = "weak",
          "Non-informative: Uniform(0, 1) = Beta(1, 1)" = "unif",
          "Custom Beta(α, β)" = "custom"
        ),
        selected = "subj"
      ),
      
      conditionalPanel(
        condition = "input.prior_choice == 'custom'",
        numericInput("alpha", "α:", value = 2, min = 0.1, step = 0.1),
        numericInput("beta",  "β:", value = 2, min = 0.1, step = 0.1)
      ),
      
      helpText("Left: prior on θ. Right: prior predictive distribution of Y.")
    ),
    
    mainPanel(
      fluidRow(
        column(6, plotOutput("priorPlot", height = "350px")),
        column(6, plotOutput("ppPlot", height = "350px"))
      ),
      hr(),
      verbatimTextOutput("summaryText")
    )
  )
)

server <- function(input, output, session) {
  
  get_ab <- reactive({
    if (input$prior_choice == "subj") return(c(5, 2))
    if (input$prior_choice == "weak") return(c(2, 2))
    if (input$prior_choice == "unif") return(c(1, 1))
    c(input$alpha, input$beta)
  })
  
  output$priorPlot <- renderPlot({
    ab <- get_ab()
    a <- ab[1]; b <- ab[2]
    
    theta <- seq(0, 1, length.out = 1001)
    dens <- dbeta(theta, a, b)
    
    plot(theta, dens, type = "l",
         xlab = expression(theta),
         ylab = "Density",
         main = bquote("Prior on " * theta * ": Beta(" * .(a) * "," * .(b) * ")"))
    abline(v = a / (a + b), lty = 2)
    legend("topleft",
           legend = c("Beta density", "Mean (α/(α+β))"),
           lty = c(1, 2), bty = "n")
  })
  
  output$ppPlot <- renderPlot({
    ab <- get_ab()
    a <- ab[1]; b <- ab[2]
    n <- input$n
    
    y <- 0:n
    # Beta-Binomial prior predictive pmf:
    # p(y) = choose(n,y) * B(y+a, n-y+b) / B(a,b)
    pmf <- choose(n, y) * beta(y + a, n - y + b) / beta(a, b)
    
    plot(y, pmf, type = "h",
         xlab = "y",
         ylab = "Prior predictive P(Y=y)",
         main = "Prior predictive for Y (Beta-Binomial)")
    points(y, pmf, pch = 16)
    
    # Add mean/variance markers (prior predictive moments)
    mu_theta <- a / (a + b)
    var_theta <- (a * b) / ((a + b)^2 * (a + b + 1))
    
    mu_y <- n * mu_theta
    var_y <- n * mu_theta * (1 - mu_theta) + n * (n - 1) * var_theta
    
    abline(v = mu_y, lty = 2)
    legend("topright",
           legend = c("PMF", "Mean of Y"),
           lty = c(NA, 2),
           pch = c(16, NA),
           bty = "n")
  })
  
  output$summaryText <- renderText({
    ab <- get_ab()
    a <- ab[1]; b <- ab[2]
    n <- input$n
    
    mu_theta <- a / (a + b)
    var_theta <- (a * b) / ((a + b)^2 * (a + b + 1))
    
    mu_y <- n * mu_theta
    var_y <- n * mu_theta * (1 - mu_theta) + n * (n - 1) * var_theta
    
    paste0(
      "Model: Y ~ Binomial(n, θ)\n",
      "Prior: θ ~ Beta(", a, ", ", b, ")\n\n",
      "Prior moments:\n",
      "  E[θ]  = ", round(mu_theta, 4), "\n",
      "  Var[θ]= ", signif(var_theta, 4), "\n\n",
      "Prior predictive moments:\n",
      "  E[Y]  = ", round(mu_y, 3), "\n",
      "  Var[Y]= ", round(var_y, 3), "\n"
    )
  })
}

shinyApp(ui, server)