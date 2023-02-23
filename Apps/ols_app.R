packages <- c("tidyverse", "shiny", "wesanderson", "DT", "ggExtra", "ggrepel", "ggiraph")
lapply(packages, require, character.only = TRUE)

plot_guess <- function(
    slope, y_intercept, resids_guess = FALSE, resids_OLS = FALSE, labels = FALSE
){
  # get data ----
  set.seed(350)
  Sigma <- matrix(c(10,3,3,2),2,2)
  data <- MASS::mvrnorm(n = 20, mu = rep(5, 2), Sigma, empirical = TRUE) %>% abs() %>% round(2)
  x <- data[, 1]
  y <- data[, 2]
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
    "Squared Residual (Guess)" = y_resid_guess_sq
  ) %>% 
    mutate(
      across(.cols = where(is.numeric), round, 1),
      interactive_label = paste(
        "Observed Y: ",
        format(Y, nsmall = 1),
        "<br>",
        "Predicted Y (OLS): ", 
        format(`Predicted Y (OLS)`, nsmall = 1),
        "<br>",
        "Predicted Y (Guess): ",
        format(`Predicted Y (Guess)`, nsmall = 1)
        )
      )
  data_tab <- data %>% 
    mutate(across(.cols = 3:(ncol(.)-1), round, 2), across(.cols = 3:ncol(.), format, nsmall = 2))
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
  plot <- ggplot(data, aes(x = X, y = Y, tooltip = interactive_label)) +
    geom_point(alpha = 0.5, shape = 1) +
    geom_point_interactive(alpha = 0.5, shape = 1) +
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
      geom_text_repel(aes(label = `Residual (Guess)`), size = 3)
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
      geom_text_repel(aes(label = `Residual (OLS)`), size = 3) 
    } +
    {if (labels)
      geom_text_repel(aes(label = 1:20), size = 3) # label data points
    } +
    theme_classic() +
    theme(legend.position = "bottom") +
    scale_color_manual(
      values = wes_palette(2, name = "Moonrise2", type = "discrete"), name = ""
    ) +
    scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +
    scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 2))

  plot <- ggMarginal(plot, type = "density")
  plot <- girafe(code = {print(plot)})
  
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
      ggiraphOutput("plot", width = 900, height = 850)
      # ,dataTableOutput("data_tab")
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
  
  output$plot <- renderggiraph({
    guess_plot()$plot
  }
  # , res = 96
  )
  
  output$reg_table_long <- renderTable({
    guess_plot()$reg_table_long
  }, 
  sanitize.text.function=function(x){x}
  )
  
  output$data_tab <- DT::renderDataTable(
    guess_plot()$data_tab, 
    options = list(pageLength = 5, lengthMenu = 1:10, dom = "plt")
  )
  
}

shinyApp(ui = ui, server = server)

# increase sample size DONE
# move legend to left DONE
# add marginals DONE  
# adjust figure dimensions DONE
# switch to ggrepel
# if needed, tweak position jitter
# increase menu size DONE
# move table to bottom DONE

