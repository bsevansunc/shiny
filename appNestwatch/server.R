library(shiny)
library(leaflet)
library(raster)

shinyServer(function(input, output) {
  
  existingSiteTable <- reactive({
    if(is.null(input$existingSites))
      return(NULL)
    read.csv(input$existingSites$datapath) %>%
      mutate(siteType = 'existing', imp = 'FILL')
  })
  
  potentialSiteTable <- reactive({
    if(is.null(input$potentialSites))
      return(NULL)
    read.csv(input$potentialSites$datapath) %>%
      mutate(siteType = 'potential', imp = 'FILL')
  })
  
  
  dataTable <- reactive({
    if (is.null(input$existingSites) & is.null(input$potentialSites))
      return(NULL)
    if (!is.null(input$existingSites) & is.null(input$potentialSites))
      return(existingSiteTable())
    if (is.null(input$existingSites) & !is.null(input$potentialSites))
      return(potentialSiteTable())
    rbind(existingSiteTable(), potentialSiteTable())
  })
  
  impervious <- raster('data/impNoWaterDC')
  
  imperviousAgg <- aggregate(impervious, 4)
    
  output$table <- renderTable({
    dataTable() 
  })
  
  output$plot <- renderPlot({
    hist(dataTable()$latitude)
  })
  

output$map <- renderLeaflet({
    if(is.null(input$existingSites) & is.null(input$potentialSites)){
    leaflet() %>%
      addTiles() %>%
          addCircleMarkers(lng = ~longitude, lat = ~latitude, 
                           data = data.frame(site = 'Hello World!', 
                                             longitude = -77.0526,latitude = 38.92934, 
                                             siteType = 'start', imp = 'FILL'), 
                           group = 'start',
                           radius = 6, fillOpacity = 0.8, color = 'black', 
                           stroke = FALSE, popup = ~site) %>%
      addRasterImage(imperviousAgg, opacity = 0.8, group = 'Impervious Surface') %>%
        addLayersControl(overlayGroups = c('Impervious Surface'))
      
    } else {
  
    if(!is.null(input$existingSites) && is.null(input$potentialSites)){
      leaflet() %>%
      addTiles() %>%
          addCircleMarkers(lng = ~longitude, lat = ~latitude, 
                           data = existingSiteTable(), 
                           group = 'existing',
                           radius = 6, fillOpacity = 0.8, color = 'navy', 
                           stroke = FALSE, popup = ~site) %>%
        addRasterImage(imperviousAgg, opacity = 0.8, group = 'Impervious Surface') %>%
        addLayersControl(overlayGroups = c('Existing Sites', 'Impervious Surface'))
      
    } else {
      
    if(is.null(input$existingSites) && !is.null(input$potentialSites)){
      leaflet() %>%
        addTiles() %>%
        addCircleMarkers(lng = ~longitude, lat = ~latitude, 
                         data = potentialSiteTable(), 
                         group = 'potential',
                         radius = 6, fillOpacity = 0.8, color = 'red', 
                         stroke = FALSE, popup = ~site) %>%
        addRasterImage(imperviousAgg, opacity = 0.8, group = 'Impervious Surface') %>%
        addLayersControl(overlayGroups = c('Potential Sites', 'Impervious Surface'))
      
    } else {
 
      if(!is.null(input$existingSites) && !is.null(input$potentialSites)){
        leaflet() %>%
          addTiles() %>%
          addCircleMarkers(lng = ~longitude, lat = ~latitude, 
                           data = existingSiteTable(), 
                           group = 'Existing Sites',
                           radius = 6, fillOpacity = 0.8, color = 'navy', 
                           stroke = FALSE, popup = ~site) %>%
          addCircleMarkers(lng = ~longitude, lat = ~latitude, 
                           data = potentialSiteTable(), 
                           group = 'Potential Sites',
                           radius = 6, fillOpacity = 0.8, color = 'red', 
                           stroke = FALSE, popup = ~site) %>%
          addRasterImage(imperviousAgg, opacity = 0.8, group = 'Impervious Surface') %>%
          addLayersControl(overlayGroups = c('Existing Sites', 'Potential Sites', 'Impervious Surface'))
    }}}}
})




})