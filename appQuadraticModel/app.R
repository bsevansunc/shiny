library(shiny)

ui <- fluidPage(
  headerPanel('Plot linear model with quadratic term'), br(),
  sidebarPanel(
    sliderInput(inputId = "b0", 
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
      align = 'center'),'    ',
    img(src = 'nnLogo.gif', 
      height = 100, width = 300,
      align = 'center')),
  mainPanel(
    plotOutput('plot1'),
    column(3, h4(strong('Line characteristics')),
       selectInput('lineColor','Line color:',
                   c('blue', 'red', 'black')),
       radioButtons('lineType','Line type',
                    c('Solid' = 'solid', 'Dashed' = 'dashed', 'Dot-dash' = 'dotdash')),
       numericInput('lineW','Line width', 1, min = 0, max = 5, step = .5)), 
    column(3, h4(strong('Labels')),
           textInput('main','Main title'),
           sliderInput(inputId = 'mainTitleSize',
                       label = 'Main title size',
                       value = 2, min = 0, max = 5, step = 0.1),
           checkboxInput('showAxes','Axis values'),
           sliderInput(inputId = 'axisTitleSize',
                       label = 'Axis title size',
                       value = 1.15, min = 0, max = 5, step = 0.1)
    )
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
      ggtitle(input$main) +
      labs(x = 'Predictor', y = 'Response') +
      theme(axis.title = element_text(size = rel(input$axisTitleSize)),
            axis.line = element_line(color ='black'),
            plot.title = element_text(size = rel(input$mainTitleSize)),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank()) +
      if(input$showAxes == F) theme(axis.ticks = element_blank(),
                                       axis.text = element_blank())
  })
}

shinyApp(ui = ui, server = server)
