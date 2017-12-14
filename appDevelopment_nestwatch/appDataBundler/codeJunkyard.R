




# server scrapyard

# Bundle data into frame:
# visitData <- dataBundler('visitData')
# Clean frame:
#     visitData %>%
#       distinct() %>%
#       rename(date = dateOut) %>%
#       tidyr::gather('netLength', 'nNets', c(netCount6, netCount9, netCount12, netCount18)) %>%
#       mutate(netLength = netLength %>%
#                str_replace('netCount', '') %>% as.numeric)
#     )
#     data <- dataBundler('visitData') %>%
#       dplyr::rename(date = dateOut, obs = observer, 
#                     long = longitude, lat = latitude) %>%
#       dplyr::select(hub, site, date, obs:notes)
#     #if (input$hub != 'All regions') data =  filter(data, hub == input$hub)
#     )



#             output$vTable <- renderTable(
#               visitData()
#                 # rename(date = dateOut) %>%
#                 # select(dateOut:accuracy) %>%
#                 # distinct()
#               )

# output$bundleVisitData

#   observeEvent(input$bundleVisitData, {
#     if(input$password != hubPassword()){
#       shinyjs::toggle('wrongPasswordMessage')
#     } else {
#       shinyjs::hide('wrongPasswordMessage')
#       bundledData <- dataBundler('visitData')
#       
#       if(input$hub == 'All regions'){
#         bundledData <- dataBundler('visitData')
#       } else {
#         bundledData <- dataBundler('visitData') %>%
#           filter(hub == input$hub)
#       }
# write.csv(bundledData,'visitData.csv')
#     }
#   })

#   observeEvent(input$accessData, {
#     hubPassword <- hubPasswordFrame %>%
#       filter(hub == input$hub) %>%
#       .$password
#     if(input$password != hubPassword){
#       shinyjs::toggle("wrongPasswordMessage")
#     }
#     if(input$password == hubPassword){
#       shinyjs::hide('wrongPasswordMessage')
#     }
#   })

#   observe({
#     hubPassword <- hubPasswordFrame %>%
#       filter(hub == input$hub) %>%
#       .$password
#     if(input$password == hubPassword){
#       shinyjs::disable("bundleVisitData")
#       shinyjs::enable("bundleVisitData")
#     }
#   })