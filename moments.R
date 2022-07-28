packages <- c("tidyverse", "shiny", "wesanderson", "DT")
lapply(packages, require, character.only = TRUE)


# - sliders to adjust mean, variance, etc.
#    - this takes care of x-axis scaling
# - options:
#     - draw line for mean 
#     - draw line for mode 
#     - draw line for median
# - add boxplot underneath with bottom half violin

get_moments_data <- function(mean = 0, variance = 1, skewness = 0, kurtosis = 3, sample_size = 1000) {
  require(PearsonDS)
  moments <- c(
    mean = mean, 
    variance = variance, 
    skewness = skewness, 
    kurtosis = kurtosis + (skewness^2 + 1)
    )
  data <- rpearson(sample_size, moments = moments)
  data <- tibble(Data = round(data, 0))
  plot <- ggplot(data, aes(x = Data)) +
    geom_bar() +
    xlim(c(-20, 20)) +
    theme_classic()
    return(list(data = data, plot = plot))
}


ui <- fluidPage( # ----
  sidebarLayout(
    sidebarPanel(
      sliderInput( # 1st entry in this column
        "mean", # object name that gets referred to in server
        label = HTML("Enter a Mean"), # label displayed to user above text box
        value = 0, # default is the mean of data$Y above
        step = 0.05, # increment for if user uses clicker thing to move values up and down,
        min = -10,
        max = 10
      ),
      sliderInput(
        "variance",
        label = "Enter a Variance",
        value = 0,
        step = 0.25,
        min = 0,
        max = 20
      ),
      sliderInput(
        "skewness",
        label = "Enter a Skewness",
        value = 0,
        min = -5,
        max = 5
      ),
      sliderInput(
        "kurtosis",
        label = "Enter a Kurtosis",
        value = 3,
        min = 0.001,
        max = 25
      )
    ),
    mainPanel(
      plotOutput("plot"),
      dataTableOutput("data")
    )
  )
)

server <- function(input, output, session) {

  # create objects to use later
  # wrapping in the reactive({}) function lets you re-used object
  mean <- reactive({input$mean})
  variance <- reactive({input$variance})
  skewness <- reactive({input$skewness})
  kurtosis <- reactive({input$kurtosis})
  # sample_size <- reactive({input$sample_size})
  
  moments_data <- reactive({
    get_moments_data(
      mean = mean(),
      variance = variance(),
      skewness = skewness(),
      kurtosis = kurtosis()
      # sample_size = sample_size()
      )})
  
  output$plot <- renderPlot({
    moments_data()$plot
  }, res = 96)

  output$data <- DT::renderDataTable(
    moments_data()$data,
    options = list(pageLength = 10, lengthMenu = 1:10, dom = "plt")
  )

}

shinyApp(ui = ui, server = server)

