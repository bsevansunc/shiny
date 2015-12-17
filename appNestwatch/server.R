library(shiny)
library(leaflet)

shinyServer(function(input, output) {
    dataTable <- reactive({
      if (is.null(input$existingSites))
        return(NULL)
      existingSites <- read.csv(input$existingSites$datapath) %>%
        mutate(siteType = 'existing', imp = 'FILL')
      if (is.null(input$potentialSites))
        return(existingSites)
      potentialSites  <- read.csv(input$potentialSites$datapath) %>%
        mutate(siteType = 'potential', imp = 'FILL')
      rbind(existingSites, potentialSites)
    })
    
  output$table <- renderTable({
    dataTable() 
  })
  
  output$plot <- renderPlot({
    hist(dataTable()$lat)
  })
  
#   output$map <- renderLeaflet({
#     leaflet() %>%
#       addTiles() %>%
#       setView(-77.1, 38.9, zoom = 2)
#   })
# 
# pointsReactive <- reactive({
#     if(is.null(input$existingSites) & is.null(input$potentialSites))
#     return(data.frame(site = 'Hello World!', 
#                       long = -77.0526,lat = 38.92934, 
#                       siteType = 'existing'))
#     dataTable()
#   })

# pointsReactive <- reactive({
#     if(!input$showExisting & !input$showPotential){
#     return(data.frame(site = 'Hello World!', 
#                       long = -77.0526,lat = 38.92934, 
#                       siteType = 'existing'))
#     } else {
#       if(input$showExisting)
#         return(dataTable())
#     }
# #     dataTable()
#   })

# pointsReactive <- reactive({
#   layersGroup <- input$mapLayers
#   if(is.null(input$existingSites))
#     return(data.frame(site = 'Hello World!', 
#                       long = -77.0526,lat = 38.92934, 
#                       siteType = 'existing'))
#   #if(layersGroup == 'existing')#& layersGroup!= 'potential')
#     dataTable()
#     # %>% dplyr::filter(siteType == 'existing'))
# })

# pointsReactive <- reactive({
#   layersGroup <- input$mapLayers
#   if(layersGroup != 'existing' & layersGroup!= 'potential')
#         return(data.frame(site = 'Hello World!', 
#                           long = -77.0526,lat = 38.92934))
#   if(layersGroup == 'existing' & layersGroup!='potential')
#     return(dataTable() %>% dplyr::filter(siteType == 'existing'))
# })

# pointsPotential <- reactive({
#   layersGroup <- input$mapLayers
#   if(layersGroup == 'potential')
#     return(dataTable() %>% dplyr::filter(siteType == 'potential'))
# })

pointsReactive <- reactive({
  layersGroup <- input$mapLayers
  if(is.null(input$existingSites))
    return(data.frame(site = 'Hello World!', 
                      long = -77.0526,lat = 38.92934, 
                      siteType = 'existing'))
  #if(layersGroup == 'existing')#& layersGroup!= 'potential')
  rbind(pointsExisting(), pointsPotential)()
  # %>% dplyr::filter(siteType == 'existing'))
})

# observe({
#   points <- pointsExisting()
#   leafletProxy('map', data = points) %>%
#     addCircleMarkers(lng = ~long, lat = ~lat, 
#                      radius = 6, color = 'navy',fillOpacity = 0.8,
#                      stroke = FALSE, popup = ~site) %>%
#     fitBounds(lng1 = max(points$long),lat1 = max(points$lat),
#               lng2 = min(points$long), lat2 = min(points$lat))
# })

# observe({
#   points <- pointsReactive()
#   palette <- colorFactor(c('navy','red'), domain = c('existing','potential'))
#   leafletProxy('map', data = points) %>% 
#     addCircleMarkers(lng = ~long, lat = ~lat, 
#                      radius = 6, color = ~palette(siteType),fillOpacity = 0.8,
#                      stroke = FALSE, popup = ~site) %>%
#     fitBounds(lng1 = max(points$long),lat1 = max(points$lat),
#               lng2 = min(points$long), lat2 = min(points$lat))
# })

# pointsReactive <- reactive({
#     if(!input$showExisting & !input$showPotential){
#     return(data.frame(site = 'Hello World!', 
#                       long = -77.0526,lat = 38.92934, 
#                       siteType = 'existing'))
#     } else {
#       if(input$showExisting & !input$showPotential){
#         return(dataTable())
#     } else {
#       if(!input$showExisting & input$showPotential){
#         return(dataTable())
#     } else {
#         return(dataTable())
#       }}} 
#   })

# points <- reactive({
#   data.frame(site = 'Hello World!', 
#     long = -77.0526,lat = 38.92934, 
#     siteType = 'existing')
# })


output$map <- renderLeaflet({
  palette <- colorFactor(c('navy','red'), domain = c('existing','potential'))
  leaflet(pointsReactive()) %>%
    addTiles() %>%
        addCircleMarkers(lng = ~long, lat = ~lat, group = 'start',
                         radius = 6, color = ~palette(siteType),fillOpacity = 0.8,
                         stroke = FALSE, popup = ~site
                   ) %>%
        addMarkers(group = 'markers')
})

observeEvent(input$show, {
  proxy <- leafletProxy('map')
  if (input$show) proxy %>% showGroup('markers')
  else proxy %>% showGroup('markers')
})

# observe({
#   if(input$showExisting){
#     existingPoints <- dataTable() <- filter(siteType == 'existing')
#     addCircleMarkers(lng = ~long, lat = ~lat,
#                    radius = 6, color = "navy",fillOpacity = 0.8,
#                    stroke = FALSE, popup = ~site)
#   }
# })


#   leafletProxy('map', data = points) %>% 
#     addCircleMarkers(lng = ~long, lat = ~lat, 
#                      radius = 6, color = ~palette(siteType),fillOpacity = 0.8,
#                      stroke = FALSE, popup = ~site) %>%
#     fitBounds(lng1 = max(points$long),lat1 = max(points$lat),
#               lng2 = min(points$long), lat2 = min(points$lat))
# })

# observe({
#   if(!input$showExisting)
#     leafletProxy()
# })

# observe({
#   points <- if(!input$showExisting & !input$showPotential){
#     data.frame(site = 'Hello World!', 
#                       long = -77.0526,lat = 38.92934, 
#                       siteType = 'existing')
#   } else {
#     if(input$showExisting){
#       dataTable()
#     }
#   }
#   palette <- colorFactor(c('navy','red'), domain = c('existing','potential'))
#   leafletProxy('map', data = points) %>% 
#     addCircleMarkers(lng = ~long, lat = ~lat, 
#                      radius = 6, color = ~palette(siteType),fillOpacity = 0.8,
#                      stroke = FALSE, popup = ~site) %>%
#     fitBounds(lng1 = max(points$long),lat1 = max(points$lat),
#               lng2 = min(points$long), lat2 = min(points$lat))
# })



# observe({
#   palette <- colorFactor(c('navy','red'), domain = c('existing','potential'))
#   mapLayers <- input$mapLayers
#     if(mapLayers != 'existing'){
#       points <- pointsReactive()
#       leafletProxy('map', data = points) %>%
#         addCircleMarkers(lng = ~long, lat = ~lat, 
#                          radius = 6, color = ~palette(siteType),fillOpacity = 0.8,
#                          stroke = FALSE, popup = ~site) %>%
#         fitBounds(lng1 = max(points$long),lat1 = max(points$lat),
#                   lng2 = min(points$long), lat2 = min(points$lat))
#     }
# })


})