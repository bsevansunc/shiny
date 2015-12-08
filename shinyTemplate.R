library(shiny)

# User interface

ui <- fluidPage(
  titlePanel ('Insert title panel here'),
  sidebarLayout(position = 'left',
                sidebarPanel('Insert name of sidebar panel'),
                mainPanel('Insert name of main panel'))
)

# Server

server <- function(input, output) {}

shinyApp(ui = ui, server = server)