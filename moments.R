packages <- c("tidyverse", "shiny", "wesanderson", "DT")
lapply(packages, require, character.only = TRUE)


get_plot <- function(mean = 100, sd = 0) {
  set.seed(352)
  data <- tibble(Y = rnorm(n = 10, mean = mean, sd = sd))
  data <- tibble(Y = rnorm(n = 100, mean = 100, sd = 10))
  # plot <- 
    ggplot(data, aes(x = Y)) +
    geom_dotplot(binwidth = 0.5) +
      xlim(75, 125)
  # +
  #   geom_abline(
  #     aes(slope = 1, intercept = mean),
  #     size = c(1, 1.5),
  #     show.legend = TRUE
  #   ) 
  return(
    list(
      data = data, 
      plot = plot
    )
  )
}
get_plot <- function(mean = 0, variance = 1, skewness = 0, kurtosis = 3) {
  require(PearsonDS)
  moments <- c(mean = mean, variance = variance, skewness = skewness, kurtosis = kurtosis + (skewness^2 + 1))
  data <- rpearson(1000, moments = moments)
  plot <- data %>% hist()
  return(plot)
}
get_plot()
#     
#   # display plot ----
#   plot <- 
ggplot(data, aes(x = X, y = Y)) +
    geom_point(alpha = 0.5, shape = 1) +
    geom_abline(
      data = reg_table,
      aes(slope = Slope, intercept = `<i>Y</i>-Intercept`, color = Line),
      size = c(1, 1.5),
      show.legend = TRUE
      ) +
    # connects dots to line of best guess
    {if (resids_guess)
        geom_segment(
          aes(
            x = X,
            y = Y,
            xend = X,
            yend = `Predicted Y (Guess)`
          ),
          linetype = "dashed",
          color = "#C27D38",
          alpha = 0.75
        )
    } +
    {if (resids_guess)
      geom_text(aes(label = `Residual (Guess)`), hjust = -0.5, size = 3)
    } +
    {if (resids_OLS)
      # connects dots to line of OLS
      geom_segment(
        aes(
          x = X,
          y = Y,
          xend = X,
          yend = `Predicted Y (OLS)`
        ),
        linetype = "dashed",
        color = "#798E87",
        alpha = 0.75
      )
    } +
    {if (resids_OLS)
      geom_text(aes(label = `Residual (OLS)`), hjust = 1.25, size = 3)
    } +
    {if (labels)
      geom_text(aes(label = 1:10), vjust = -1.25) # label data points
    } +
    theme_classic() +
    theme(legend.position = "right") +
    scale_color_manual(
      values = wes_palette(2, name = "Moonrise2", type = "discrete"), name = ""
      ) +
    scale_y_continuous(limits = c(-4, 14), breaks = seq(-4, 14, 2))

  return( # ----
    list(
      data_tab = data_tab,
      plot = plot,
      reg_table_long = reg_table_long
    )
  )
}

# ui <- fluidPage( # ----
#   sidebarLayout(
#     sidebarPanel(
#       numericInput( # 1st (and only) entry in this column
#         "y_intercept", # object name that gets referred to in server
#         label = HTML("Guess the <i>Y</i>-Intercept"), # label displayed to user above text box
#         value = 5.5, # default is the mean of data$Y above
#         step = 0.05 # increment for if user uses clicker thing to move values up and down
#       ),
#       numericInput(
#         "slope",
#         label = "Guess the Slope",
#         value = 0,
#         step = 0.25,
#         min = 0
#       ),
#       tableOutput("reg_table_long"),
#       checkboxInput(
#         "resids_guess", 
#         label = "Plot residuals (for your guess)?", 
#         value = FALSE 
#       ),
#       checkboxInput(
#         "resids_OLS", 
#         label = "Plot residuals (for OLS)?", 
#         value = FALSE 
#       ),
#       checkboxInput(
#         "labels", 
#         label = "Label data points?", 
#         value = FALSE 
#       )
#     ),
#     mainPanel(
#       dataTableOutput("data_tab"),
#       plotOutput("plot")
#     )
#   )
# )
# 
# server <- function(input, output, session) {
#   
#   # create objects to use later
#   # wrapping in the reactive({}) function lets you re-used object
#   slope <- reactive({input$slope})
#   y_intercept <- reactive({input$y_intercept})
#   resids_guess <- reactive({input$resids_guess})
#   resids_OLS <- reactive({input$resids_OLS})
#   labels <- reactive({input$labels})
#   
#   guess_plot <- reactive({
#     plot_guess(
#       slope = slope(), 
#       y_intercept = y_intercept(),
#       resids_guess = resids_guess(),
#       resids_OLS = resids_OLS(),
#       labels = labels()
#       )})
# 
#   output$plot <- renderPlot({
#     guess_plot()$plot
#   }, res = 96)
#   
#   output$reg_table_long <- renderTable({
#     guess_plot()$reg_table_long
#     }, 
#   sanitize.text.function=function(x){x}
#   )
#   
#   output$data_tab <- DT::renderDataTable(
#     guess_plot()$data_tab, 
#     options = list(pageLength = 3, lengthMenu = 1:10, dom = "plt")
#   )
#   
# }
# 
# shinyApp(ui = ui, server = server)
# 
# 
# 
# # - DONE create plot function 
# # - DONE create function to calculate sums of squares etc. 
# # - DONE create app that plots scatter plot with slope and y-intercept 
# # - DONE print regression table
# # - DONE print data values table
# # - add options:
# #   - DONE want residual lines to guess line?
# #   - DONE want residual lines to best line?
# #   - DONE want to label data points?