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
  existingPoints <- eventReactive(input$existingSites, ignoreNULL = TRUE)
  
  output$map <- renderLeaflet({
    leaflet()
  })
})