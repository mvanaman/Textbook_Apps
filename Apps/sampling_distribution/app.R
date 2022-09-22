packages <- c("tidyverse", "shiny", "ggthemes", "wesanderson", "DT", "PearsonDS")
lapply(packages, require, character.only = TRUE)

# TO-DO:
## - allow user to set shape of population distribution (to demonstrate CLT)
## - add function option to specify number of samples (so samples_list can be 1)
options(scipen = 9999)
set.seed(352)
# get first sample
sample_size = 100
sample_1 <- tibble(sample_1 = rnorm(n = sample_size, mean = 0, sd = 1))
means_SDs_table <- summarise(sample_1, Sample = 1, Mean = mean(sample_1), SD = sd(sample_1))
sample_table <- sample_1

## format data
sample_table_long <- sample_table %>% 
  pivot_longer(everything(), names_to = "sample", values_to = "X") %>% 
  separate(col = sample, into = c("drop", "sample")) %>% 
  select(-drop) %>% 
  mutate(sample = as.numeric(sample))

samples_list <- list(
  sample_table = sample_table,
  means_SDs_table = means_SDs_table,
  sample_table_long = sample_table_long
)

## make function that adds new columns
add_sample <- function(
    sample_old = samples_list$sample_table, 
    means_SDs_old = samples_list$means_SDs_table, 
    long_old = samples_list$sample_table_long
) {
  sample_new <- tibble(sample_new = rnorm(n = 100, mean = 0, sd = 1))
  means_SDs_new <- summarise(sample_new, Sample = max(long_old$sample) + 1, Mean = mean(sample_new), SD = sd(sample_new))
  
  sample_table <- cbind(sample_old, sample_new)
  new_names <- paste0("sample_", 1:ncol(sample_table))
  colnames(sample_table) <- new_names
  means_SDs_table <- bind_rows(means_SDs_old, means_SDs_new)
  
  sample_table_long <- sample_table %>% 
    pivot_longer(everything(), names_to = "sample", values_to = "X") %>% 
    separate(col = sample, into = c("drop", "sample")) %>% 
    select(-drop) %>% 
    mutate(sample = as.numeric(sample))
  
  return(list(
    sample_table = sample_table, 
    means_SDs_table = means_SDs_table, 
    sample_table_long = sample_table_long, 
    means_SDs_new = means_SDs_new
  ))
}
samples_list <- add_sample()

# get sample histogram DONE --------
lastcol <- select(samples_list$sample_table, last_col()) %>% names
bw <- (max(samples_list$sample_table[, lastcol]) - min(samples_list$sample_table[, lastcol])) / 30
sample_plot <- ggplot(samples_list$sample_table, aes_string(x = lastcol)) +
  geom_histogram(
    aes(y = ..count..),
    colour = "black",
    fill = "white",
    binwidth = bw
  ) +
  geom_density(
    aes(y = bw * ..count..),
    alpha = .2,
    fill = "#FF6666"
  ) 
sample_plot
lastcol <- select(samples_list$sample_table, last_col()) %>% names
bw <- (max(samples_list$sample_table[, lastcol]) - min(samples_list$sample_table[, lastcol])) / 30
sample_plot <- ggplot(samples_list$sample_table, aes_string(x = lastcol)) +
  geom_histogram(
    aes(y = ..count..),
    colour = "black",
    fill = "white",
    binwidth = bw
  ) +
  geom_density(
    aes(y = bw * ..count..),
    alpha = .2,
    fill = "#FF6666"
  ) 
sample_plot



# get sampling distribution DONE ----
n_samples <- samples_list$sample_table_long %>% distinct(sample) %>% max
dist_plot <- function(data = samples_list$means_SDs_table, n = n_samples) {
  if(n == 1) {
    histogram <- ggplot(samples_list$means_SDs_table, aes(x = Mean)) +
      geom_histogram(
        aes(y = ..count..),
        colour = "black",
        fill = "white"
      ) +
      xlim(c(-3, 3)) 
  } else {
    bw <- (max(samples_list$means_SDs_table$Mean) - min(samples_list$means_SDs_table$Mean)) / 30
    histogram <- ggplot(samples_list$means_SDs_table, aes(x = Mean)) +
      geom_histogram(
        aes(y = ..count..),
        colour = "black",
        fill = "white",
        binwidth = bw
      ) +
      geom_density(
        aes(y = bw * ..count..),
        alpha = .2,
        fill = "#FF6666"
      ) 
  }
  histogram <- histogram +
    labs(
      title = paste("M =", round(mean(samples_list$means_SDs_table$Mean), 2)),
      subtitle = paste("SE =", round(sd(samples_list$means_SDs_table$SD), 2)),
      y = "Frequency", 
      x = "Distribution of Sample Means"
    ) +
    theme_clean() +
    theme(
      axis.line.y = element_blank(), 
      axis.ticks.y = element_blank(),
      plot.background = element_blank(),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  return(histogram)
}
dist_plot()

# so far this all works


























ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            # sliderInput( ----
            #     # 1st entry in this column
            #     "mean", # object name that gets referred to in server
            #     label = HTML("Enter a Mean"), # label displayed to user above text box
            #     value = 0, # default is the mean of data$Y above
            #     step = 0.05, # increment for if user uses clicker thing to move values up and down,
            #     min = -10,
            #     max = 10
            # ),
            # sliderInput(
            #     "sd",
            #     label = "Enter a Standard Deviation",
            #     value = 0,
            #     step = 0.25,
            #     min = 0,
            #     max = 20
            # ), ----
            actionButton("action","Submit"),
            sliderInput(
                "n",
                label = "Enter a Sample Size N",
                value = 30,
                min = 1,
                max = 100
            ),
            dataTableOutput("sample")
        ),
        mainPanel(
            plotOutput("histogram_population"),
            plotOutput("histogram_sample")
        )
    )
)

server <- function(input, output, session) {

    # create objects to use later
    # wrapping in the reactive({}) function lets you re-used object
    n <- reactive({input$n})

    sample_from_dist <- reactive({
        input$action
        sample_from_dist_fun(
            n = n()
        )})
    
    output$histogram_sample <- renderPlot({
        sample_from_dist()$histogram
    }, res = 96)

    output$sample <- DT::renderDataTable({
        DT::datatable(
            sample_from_dist()$sample,
            options = list(pageLength = 10, lengthMenu = 1:20, dom = "plt")
            ) %>%
            formatRound(1, 2)
    })
    
    output$histogram_population <- renderPlot({
        pop_plot
    }, res = 96)
}

shinyApp(ui = ui, server = server)


# test logic

# separately, pivot means columns longer and store; plot
# Step 3: make Shiny continue this loop indefinitely

