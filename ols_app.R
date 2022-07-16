packages <- c("tidyverse", "shiny", "wesanderson", "huxtable", "DT")
lapply(packages, require, character.only = TRUE)

plot_guess <- function(
  slope, y_intercept, resids_guess = FALSE, resids_OLS = FALSE, labels = FALSE
  ){
  # get data ----
  x <- 1:10
  y <- c(2, 1, 5, 3, 6, 4, 8, 10, 7, 9)
  # fit OLS
  OLS <- lm(y ~ x)
  # statistics from guess
  slope <- slope
  y_intercept <- y_intercept
  y_hat_guess <- y_intercept + (slope * x)
  # sums of squares from guess
  y_resid_guess <- y - y_hat_guess
  y_resid_guess_sq <- y_resid_guess^2
  SSs <- sum(y_resid_guess_sq)
  ## combine into table
  data <- tibble(
    X = x, 
    Y = y, 
    "Predicted Y (OLS)" = OLS$fitted.values,
    "Residual (OLS)" = OLS$residuals,
    "Squared Residual (OLS)" = OLS$residuals^2,
    "Predicted Y (Guess)" = y_hat_guess, 
    "Residual (Guess)" = y_resid_guess,
    "Squared Residual (Guess)" = y_resid_guess_sq,
  ) %>% 
    mutate(across(.cols = 3:ncol(.), round, 2))
  data_tab <- data %>% 
    mutate(
      across(.cols = 3:ncol(.), round, 2), across(.cols = 3:ncol(.), format, nsmall = 2)
      )
  # get summary Stats ----
  SSs <- tibble("Sum of Sqaured Residuals" = c(sum(data$`Squared Residual (Guess)`), sum(data$`Squared Residual (OLS)`)))
  regression_guess <- tibble("<i>Y</i>-Intercept" = y_intercept, Slope = slope, )
  regression_OLS <- tibble(
    "<i>Y</i>-Intercept" = OLS$coefficients["(Intercept)"],
    Slope = OLS$coefficients["x"]
    )
  ## r-squared for OLS
  model_OLS <- sum((data$Y - OLS$fitted.values)^2)
  residual_OLS <- sum((data$Y - mean(data$Y))^2)
  r_squared_OLS <- 1 - (model_OLS / residual_OLS)
  r_squared_OLS <- tibble("<i>R</i><sup>2</sup>" = round(r_squared_OLS, 2))
  ## r-squared from guess
  model_guess <- sum((data$Y - y_hat_guess)^2)
  residual_guess <- sum((data$Y - mean(data$Y))^2)
  r_squared_guess <- 1 - (model_guess / residual_guess)
  r_squared_guess <- tibble("<i>R</i><sup>2</sup>" =  round(r_squared_guess, 2))
  ## combine into table
  r_squared <- bind_rows(r_squared_guess, r_squared_OLS)
  reg_table <- bind_rows(regression_guess, regression_OLS)
  reg_table <- cbind(Line = c("Your Guess", "OLS"), reg_table, r_squared)
  reg_table <- cbind(reg_table, SSs)
  reg_table <- arrange(reg_table, Line)
  reg_table_long <- reg_table %>% 
    pivot_longer(cols = -Line, names_to = "Statistic", values_to = "Value") 
  reg_table_guess <- reg_table_long %>% filter(Line == "Your Guess") %>% select(-Line)
  reg_table_OLS <- reg_table_long %>% filter(Line == "OLS") %>% select(-Line)
  reg_table_long <- full_join(reg_table_OLS, reg_table_guess, by = "Statistic")
  reg_table_long <- rename(reg_table_long, "OLS" = Value.x, "Your Guess" = Value.y)
  
  # display plot ----
  plot <- ggplot(data, aes(x = X, y = Y)) +
    geom_point(alpha = 0.5, shape = 1) +
    geom_abline(data = reg_table, aes(slope = Slope, intercept = `<i>Y</i>-Intercept`, color = Line), size = c(1, 1.5), show.legend = TRUE) +
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

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      numericInput( # 1st (and only) entry in this column
        "y_intercept", # object name that gets referred to in server
        label = HTML("Guess the <i>Y</i>-Intercept"), # label displayed to user above text box
        value = 5.5, # default is the mean of data$Y above
        step = 0.05 # increment for if user uses clicker thing to move values up and down
      ),
      numericInput(
        "slope",
        label = "Guess the Slope",
        value = 0,
        step = 0.25,
        min = 0
      ),
      tableOutput("reg_table_long"),
      checkboxInput(
        "resids_guess", 
        label = "Plot residuals (for your guess)?", 
        value = FALSE 
      ),
      checkboxInput(
        "resids_OLS", 
        label = "Plot residuals (for OLS)?", 
        value = FALSE 
      ),
      checkboxInput(
        "labels", 
        label = "Label data points?", 
        value = FALSE 
      )
    ),
    mainPanel(
      dataTableOutput("data_tab"),
      plotOutput("plot")
    )
  )
)

server <- function(input, output, session) {
  
  # create objects to use later
  # wrapping in the reactive({}) function lets you re-used object
  slope <- reactive({input$slope})
  y_intercept <- reactive({input$y_intercept})
  resids_guess <- reactive({input$resids_guess})
  resids_OLS <- reactive({input$resids_OLS})
  labels <- reactive({input$labels})
  
  guess_plot <- reactive({
    plot_guess(
      slope = slope(), 
      y_intercept = y_intercept(),
      resids_guess = resids_guess(),
      resids_OLS = resids_OLS(),
      labels = labels()
      )})

  output$plot <- renderPlot({
    guess_plot()$plot
  }, res = 96)
  
  output$reg_table_long <- renderTable({
    guess_plot()$reg_table_long
    }, 
  sanitize.text.function=function(x){x}
  )
  
  output$data_tab <- DT::renderDataTable(
    guess_plot()$data_tab, 
    options = list(pageLength = 3, lengthMenu = 1:10, dom = "plt")
  )
  
}

shinyApp(ui = ui, server = server)



# - DONE create plot function 
# - DONE create function to calculate sums of squares etc. 
# - DONE create app that plots scatter plot with slope and y-intercept 
# - DONE print regression table
# - DONE print data values table
# - add options:
#   - DONE want residual lines to guess line?
#   - DONE want residual lines to best line?
#   - DONE want to label data points?