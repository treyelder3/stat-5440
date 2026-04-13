#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Beta-Binomial Example"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput("successes", label = h3("Successes"), value = 150),
            numericInput("sample_size", label = h3("Sample Size"), value = 200),
            
            numericInput("prior_a", label = h3("Prior a"), value = 5.264),
            numericInput("prior_b", label = h3("Prior b"), value = 2.7118),
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x_bar    <- input$successes/input$sample_size
        
        a <- input$prior_a
        b <- input$prior_b
        
        a_star <- a + input$successes
        b_star <- b + (input$sample_size - input$successes)
        
        plot <- ggplot() + 
            xlim(-0.1, 1.1) + 
            geom_function(fun = dbeta, args = list(shape1 = a, shape2 = b), color = 'black', size = 1.5) +
            geom_function(fun = dbeta, args = list(shape1 = a_star, shape2 = b_star), color = 'red', size = 1.5) + 
            geom_vline(xintercept = x_bar, linetype = 2, size = 1.5, color = 'blue') + 
            theme_bw() +
            scale_x_continuous(breaks = seq(0,1, by = 0.2),
                               limits = c(0,1)) + 
            scale_y_continuous(breaks = NULL) +
            xlab("p") + ylab("")
        
        plot
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
