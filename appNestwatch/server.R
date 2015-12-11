library(shiny)
library(leaflet)

shinyServer(function(input, output) {
#   output$table <- renderTable({
#     inFileExistingSites <- input$existingSites
#     inFilePotentialSites <- input$potentialSites
#     if (is.null(inFileExistingSites))
#       return(NULL)
#     existingSites <- read.csv(inFileExistingSites$datapath) %>%
#       mutate(siteType = 'existing', imp = 'FILL')
#     if (is.null(inFilePotentialSites))
#       return(existingSites)
#     potentialSites  <- read.csv(inFilePotentialSites$datapath) %>%
#       mutate(siteType = 'potential', imp = 'FILL')
#     rbind(existingSites, potentialSites)
#   })
#   regionPoints <- read.csv('data/regionCenterPoints.csv')
  
#   points <- eventReactive(input$existingSites, {
#     cbind(input$Long,input$Lat)
#     }, ignoreNULL = TRUE)
  
  output$map <- renderLeaflet({
    leaflet %>%
#       addTiles() %>%
#       #addProviderTiles('CartoDB.Positron') %>%
#       setView(-77.1, 38.9, zoom = 4)
    })
  
#   observe({
#     proxy <- leafletProxy('map')
#     
#     if(input$existingSites) {
#       leafletProxy('map') %>%
#         addCircleMarkers(radius = 6,
#                          color = 'navy',
#                          stroke = F,
#                          fillOpacity = 0.8)
#     }
#     })
#   
# #   observe({
#     leafletProxy('map') %>%
#       addCircleMarkers(existingPoints,
#        radius = 6,
#        color = 'navy',
#        stroke = F,
#        fillOpacity = 0.8)
#   })
  
})