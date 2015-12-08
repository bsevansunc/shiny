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
              value = 0, min = -2, max = 2, step = 0.1),
  br(), 
  img(src = 'siLogo.png', 
      height = 100, width = 90,
      align = 'center')
  ),
  mainPanel(
    column(3,selectInput('lineColor','Line color:',
                c('blue', 'red', 'black')),
    radioButtons('lineType','Line type',
                 c('Solid' = 'solid', 'Dashed' = 'dashed')),
    numericInput('lineW','Line width', 1, min = 0, max = 5, step = .5)),
    column(3,
    checkboxInput('showAxes','Label axis'),
    sliderInput(inputId = 'axisTitleSize',
                label = 'Title size',
                value = 1.15, min = 0, max = 5, step = 0.1)),
    column(3, img(src = 'nnLogo.gif', 
                   height = 100, width = 300,
                   align = 'center')),
    plotOutput('plot1'),
    br(),br()
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
      geom_line(linetype = input$lineType, size = input$lineW, color = input$lineColor) + 
      labs(x = 'Predictor', y = 'Response') +
      theme(axis.title = element_text(size = rel(input$axisTitleSize)),
            axis.line = element_line(color ='black'),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank()) +
      if(input$showAxes == F) theme(axis.ticks = element_blank(),
                                       axis.text = element_blank())
  })
}

shinyApp(ui = ui, server = server)
