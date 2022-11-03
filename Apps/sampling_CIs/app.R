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
library(ggthemes)

options(scipen = 999)

conf_sim <- function(mu, sd, n_samples, sample_size = 10, confidence = 95, ...) {# add n for number of samples
  samples <- replicate(n = n_samples, expr = rnorm(n = sample_size, mean = mu, sd = sd), ...)
  colnames(samples) <- 1:ncol(samples)
  samples <- as_tibble(samples)
  samples <- samples %>% 
    pivot_longer(cols = everything(), names_to = "Sample", values_to = "Sample_Value") %>% 
    mutate(Sample = as.numeric(Sample)) %>% 
    arrange(Sample)
  # get sample statistics
  samples <- samples %>%
    group_by(Sample) %>%
    summarise(Mean = mean(Sample_Value), SD = sd(Sample_Value), SE = SD / sqrt(sample_size))
  # add CIs
  z <- case_when(confidence == 90 ~ 1.645, confidence == 95 ~ 1.96, confidence == 99 ~ 2.576)
  samples <- samples %>%
    mutate(
      Lower = Mean - z * sd/sqrt(sample_size),
      Upper = Mean + z * sd/sqrt(sample_size),
      Sample = 1:nrow(.),
      "Interval Contains Population Mean?" = ifelse(Lower < mu, ifelse(Upper > mu, "Yes", "No"), "No"),
      "Interval Contains Population Mean?" = factor(`Interval Contains Population Mean?`, levels = c("No", "Yes"))
    ) %>% 
    select(Sample, Mean, Lower, Upper, `Interval Contains Population Mean?`)
  
  colorset <-  c('No' = 'red', 'Yes' = 'black')
  xlim <- c(0, ifelse(n_samples == 1, 2, n_samples))

  plot <- samples %>%
    ggplot(aes(x = Sample, y = Mean)) +
    geom_point(aes(color = `Interval Contains Population Mean?`), alpha = .6) +
    geom_errorbar(
      aes(ymin = Lower, ymax = Upper, color = `Interval Contains Population Mean?`), 
      alpha = .4,
      width = 0
      ) +
    scale_color_manual(values = colorset, name = expression(paste('CI Captures ', mu, "?", sep = ""))) +
    geom_hline(aes(yintercept = mu, linetype = "mu"), color = "purple") +
    labs(title = "95% Confidence Intervals") +
    ylab(label = NULL) +
    xlab(label = paste("Samples (", expression(N), ")", sep = "")) +
    coord_flip(xlim = xlim, ylim = c(mu - (4 * sd), mu + (4 * sd)), clip = "off") +
    theme_tufte() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "right",
      plot.margin = unit(c(0, 0, 1, 0.5), "cm")
    ) +
    # guides(color = guide_legend(title = "CI Captures \nPopulation Value?")) +
    scale_linetype_manual(
      name = NULL, 
      values = 2,
      labels = expression(mu),
      guide = guide_legend(override.aes = list(color = "purple"))
    ) 
  
  return(list(plot = plot, samples = samples))
}

# App -----
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      numericInput( # 1st (and only) entry in this column
        "mu", # object name that gets referred to in server
        label ="Enter A Population Mean (\\( \\mu \\))", # label displayed to user above text box
        value = 100, # default is the mean of data$Y above
        step = 1 # increment for if user uses clicker thing to move values up and down
      ),
      numericInput(
        "sd", 
        label = withMathJax("Enter a Population Standard Deviation \\( \\mu \\)"), 
        value = 10 
      ),
      numericInput(
        "n_samples", 
        label = "Enter the Number of Samples You Want to Draw", 
        value = 100
      ),
      numericInput(
        "confidence", 
        label = "Enter Your Confidence Level", 
        value = 95 
      ),
      numericInput(
        "sample_size", 
        label = "Enter Your Size per Sample", 
        value = 10
      ),
      DT::dataTableOutput("data_tab")
    ),
    mainPanel(
      plotOutput("plot", height = "1000px")
    )
  )
)

server <- function(input, output, session) {
  
  # create objects to use later
  # wrapping in the reactive({}) function lets you re-used object
  n_samples <- reactive({input$n_samples})
  sample_size <- reactive({input$sample_size})
  mu <- reactive({input$mu})
  sd <- reactive({input$sd})
  confidence <- reactive({input$confidence})

  CIs <- reactive({
    conf_sim(
      n_samples = n_samples(),
      sample_size = sample_size(), 
      mu = mu(),
      sd = sd(),
      confidence = confidence()
    )})
  
  output$plot <- renderPlot({
    CIs()$plot
  }, res = 96)
  
  output$data_tab <- DT::renderDataTable({
    DT::datatable(
      CIs()$samples,
      rownames = FALSE,
      options = list(pageLength = 10, lengthMenu = 1:20, dom = "plt")
    ) %>%
      DT::formatRound(columns = c("Mean", "Lower", "Upper"), digits = 2)
  })
  
}

shinyApp(ui = ui, server = server)

