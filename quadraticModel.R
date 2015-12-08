library(shiny)

ui <- fluidPage(
  headerPanel('Plot linear model with quadratic term'),
  sidebarPanel(sliderInput(inputId = "b0", 
              label = "y-intercept", 
              value = 0, min = -2, max = 2, step = 0.1),
  sliderInput(inputId = "b1", 
              label = "Coefficient of the linear term", 
              value = 0, min = -2, max = 2, step = 0.1),
  sliderInput(inputId = "b2", 
              label = "Coefficient of the quadratic term", 
              value = 0, min = -2, max = 2, step = 0.1)),
  mainPanel(
    plotOutput('plot1')
  )
)

server <- function(input, output) {
  df <- reactive({
    x = seq(0,1, by = 0.01)
    y = input$b0 + input$b1*x + input$b2*x^2
    data.frame(x, y)
  })
    require(ggplot2)
  output$plot1 <- renderPlot({
    ggplot(df(), aes(x, y)) +
      geom_line(linetype = 'dashed', size = 1.15) + 
      labs(x = 'Predictor', y = 'Response') +
      theme(axis.title = element_text(size = rel(2)),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            axis.line = element_line(color ='black', size = 1.15),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank())
  })
}

shinyApp(ui = ui, server = server)
