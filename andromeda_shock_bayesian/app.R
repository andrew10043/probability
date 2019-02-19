# ANDROMDA-SHOCK Bayesian Re-Analysis
# Adapted from code by Dan Lane

library(shiny)
library(tidyverse)

ui <- fluidPage(
   
   sidebarLayout(
      sidebarPanel(
         sliderInput("theta",
                     "Prior Mean:",
                     min = 0.1,
                     max = 2,
                     value = 1,
                     step = 0.01),
         sliderInput("sd",
                     "Prior SD:",
                     min = 0.01,
                     max = 1,
                     value = 0.3,
                     step = 0.01)
      ),
      
      # Show a plot of the generated distributions
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

server <- function(input, output) {
   
  # Calculating MCID
  
  # Here I am using the estimated reductions from the power calculation to get an 
  # OR for the MCID (may need to be converted to RR instead of OR)#
  
  a <- 0.3 * 420 # Intervention and Outcome
  b <- 0.45 * 420 # Control and Outcome
  c <- 420 - a # Intervention No Outcome
  d <- 420 - b # Control No Outcome
  
  MCID <- ((a+0.5) * (d+0.5))/((b+0.5) * (c+0.5))
  
  #Hazard Ratio
  
  HR <- 0.75
  UC <- 1.02
  
  # Calculate Priors
  prior.theta <- reactive({log(input$theta)})
  prior.sd <- reactive({input$sd})
  
  # Calculate Likelihood
  L.theta <- log(HR)
  L.sd <- (log(UC)-log(HR))/1.96
  
  # Calculate Posterior
  post.theta <- reactive({((prior.theta()/(prior.sd())^2)+(L.theta/L.sd^2))/((1/(prior.sd())^2)+(1/L.sd^2))})
  post.sd <- reactive({sqrt(1/((1/(prior.sd())^2)+(1/L.sd^2)))})
  
  # Plot data
  x <- seq(-3, 3, by = 0.025)
  prior_plot <- reactive({dnorm(x, prior.theta(), prior.sd())})
  likelihood_plot <- dnorm(x, L.theta, L.sd)
  posterior_plot <- reactive({dnorm(x, post.theta(), post.sd())})
  
  plot_data <- reactive({
    tibble(
      x = rep(x, 3)
    ) %>%
      mutate(
        dist = rep(c("prior", "likelihood", "posterior"), each = nrow(.) / 3),
        y = c(prior_plot(), likelihood_plot, posterior_plot()),
        x = exp(x),
        y = exp(y)
      )
      
  })
  
  # Dynamic Plot
   output$distPlot <- renderPlot({
     plot_data() %>%
       ggplot(aes(x = x, y = y, group = dist)) + 
       geom_vline(xintercept = 1, linetype = "dashed",
                  color = "grey50", alpha = 0.75) + 
       geom_line(aes(color = dist),
                 size = 0.75) + 
       scale_color_brewer(name = NULL, type = "qual", palette = "Dark2",
                          breaks = c("prior", "likelihood", "posterior"),
                          labels = c("Prior", "Likelihood", "Posterior")) + 
       xlim(0, 2) + 
       labs(
         x = "Hazard Ratio",
         y = "Probability Density"
       ) + 
       annotate(geom = "text",
                label = paste("Posterior Probability HR < 1: ", 
                              round(pnorm(log(1), post.theta(), post.sd(), 
                                          lower.tail = TRUE), 3), sep = ""),
                x = 2, y = max(plot_data()$y), hjust = 1,
                fontface = "bold") + 
       annotate(geom = "text",
                label = paste("Probability of Prior > MCID: ", 
                              round(pnorm(log(MCID), prior.theta(), prior.sd(), 
                                          lower.tail = TRUE), 3), sep = ""),
                x = 2, y = max(plot_data()$y) - max(plot_data()$y) / 15, hjust = 1,
                fontface = "bold") + 
       theme_classic() + 
       theme(
         legend.position = "bottom",
         text = element_text(family = "Gill Sans MT"),
         axis.ticks.y = element_blank(),
         axis.text.y = element_blank(),
         axis.title = element_text(size = 15),
         axis.text = element_text(size = 12),
         legend.text = element_text(size = 12)
       )
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

