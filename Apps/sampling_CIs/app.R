#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(shiny)

# CI

conf.sim <- function(mu, sd) {
  require(ggplot2)
  require(dplyr)
  sample.draws <- rnorm(n = 50, mean = mu, sd = sd)
  sample.draws <- as.data.frame(sample.draws)
  colnames(sample.draws) <- "sample.values"
  
  sample.draws95 <-
    sample.draws %>%
    mutate(
      lower = sample.values - 1.96 * sd(sample.values),
      upper = sample.values + 1.96 * sd(sample.values)
    )
  
  Samples <- seq(1, 50, 1)
  Samples <- as.data.frame(Samples)
  
  ci95 <- cbind(Samples, sample.draws95)
  
  ci95 <-
    ci95 %>% mutate(Capture = ifelse(lower < mu, ifelse(upper > mu, "Yes", "No"), "No"))
  
  ci95$Capture <- factor(ci95$Capture, levels = c("No", "Yes"))
  
  colorset = c('No' = 'red', 'Yes' = 'black')
  
  null <-
    ci95 %>%
    ggplot(aes(x = Samples, y = sample.values)) +
    geom_point(aes(color = Capture), alpha = .6) + geom_errorbar(aes(
      ymin = lower,
      ymax = upper,
      color = Capture
    ), alpha = .6) +
    scale_color_manual(values = colorset) +
    geom_hline(yintercept = 0,
               linetype = "dashed",
               color = "blue") +
    labs(title = "95% Confidence Intervals") +
    ylab(label = NULL) +
    annotate(
      "text",
      x = -7,
      y = 0,
      label = "mu",
      parse = TRUE
    ) +
    coord_flip(xlim = c(0, 50), clip = "off") +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "right",      
      plot.margin = unit(c(0, 0, 1, 0.5), "cm")
    ) +
    guides(color = guide_legend(title = "CI Captures \nPopulation Value?"))
  
  alt <-
    ci95 %>%
    ggplot(aes(x = Samples, y = sample.values)) +
    geom_point(aes(color = Capture), alpha = .6) +
    geom_errorbar(aes(
      ymin = lower,
      ymax = upper,
      color = Capture
    ), alpha = .6) +
    scale_color_manual(values = colorset) +
    geom_hline(yintercept = 0,
               linetype = "dashed",
               color = "blue")  +
    geom_hline(yintercept = mu,
               linetype = "dashed",
               color = "purple") +
    labs(title = "95% Confidence Intervals") +
    ylab(label = NULL) +
    xlab(label = "Samples (N)") +
    annotate("text",
             x = -7,
             y = 0,
             label = "Null") +
    annotate(
      "text",
      x = -7,
      y = mu,
      label = "mu",
      parse = TRUE
    ) +
    coord_flip(xlim = c(0, 50), clip = "off") +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "right",
      plot.margin = unit(c(0, 0, 1, 0.5), "cm")
    ) +
    guides(color = guide_legend(title = "CI Captures \nPopulation Value?"))
  
  ifelse(mu == 0, return(null), return(alt))
}



# App -----


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
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
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
