library(shiny)
library(leaflet)

ui <- shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(
      checkboxInput('show','Show/Hide')
      ),
   mainPanel(
     leafletOutput('map')
     )
  )
))

server <- shinyServer(function(input, output) {
  
  pointsReactive <- reactive({
   data.frame(site = 'Hello World!', 
                        long = -77.0526,lat = 38.92934, 
                        siteType = 'existing')
  })
  
  output$map <- renderLeaflet(
#     palette <- colorFactor(c('navy','red'), domain = c('existing','potential'))
    leaflet(pointsReactive()) %>%
      addTiles() %>%
      addCircleMarkers(lng = ~long, lat = ~lat, group = 'start',
                       radius = 6, color = "black",fillOpacity = 0.8,
                       stroke = FALSE, popup = ~site) %>%
      addMarkers(group = 'markers')
    )
  
  observe({
    proxy <- leafletProxy('map')
    if(input$show)
    proxy %>% showGroup('markers')
    else proxy %>% hideGroup('markers')
  })

})

shinyApp(ui, server)
  
