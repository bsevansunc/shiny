# Data bundler

#=================================================================================*
# ---- SET-UP ----
#=================================================================================*

library(markdown)
library(shiny)
library(dplyr)
library(stringr)
library(shinyBS)
library(R.utils)
library(rdrop2)
library(DT)
library(shinyjs)

# Data binding function:

dataBundler <- function(classOfData){
  pathToFile <- paste('nnDataStorage',
                      classOfData,
                      sep = '/')
  filePaths <- drop_dir(pathToFile) %>% 
    as.data.frame %>% 
    .$path
  outList <- vector('list', length = length(filePaths))
  for(i in 1:length(filePaths)){
    outList[[i]] <- drop_read_csv(filePaths[i], stringsAsFactors = FALSE)
  }
  outFrame <- do.call(rbind, outList)
  if(classOfData == 'encounterData'){
    outFrame <- outFrame %>%
      rename(hub = hubv, site = sitev)
  }
  return(outFrame)
}

#---------------------------------------------------------------------------------*
# ---- Drop-down choices ----
#---------------------------------------------------------------------------------*

# Regional hub:

choiceRegions <- c('Atlanta' = 'Atlanta',
                   'DC' = 'DC',
                   'Gainesville' = 'Gainesville',
                   'Pittsburgh' = 'Pittsburgh',
                   'Raleigh' = 'Raleigh',
                   'Springfield' = 'Springfield',
                   'All regions' = 'All regions')

choiceData <- c('Visit' = 'Visit',
                'Encounters' = 'Encounters',
                'Point  count' = 'Point count',
                'Nest summaries' = 'Nest summaries',
                'Nest observations' = 'Nest observations')

#---------------------------------------------------------------------------------*
# ---- Passwords ----
#---------------------------------------------------------------------------------*

hubPasswordFrame <- data.frame(
  hub = c('Atlanta', 'DC', 'Gainesville', 
          'Pittsburgh', 'Raleigh', 'Springfield', 
          'All regions'),
  password = c('falcon', 'eagle','alberta',
               'penguin', 'bartram', 'sleep in the stars',
               'macarthur'),
  stringsAsFactors = FALSE
)

#=================================================================================*
# ---- USER INTERFACE ----
#=================================================================================*

ui <- fluidPage(
  useShinyjs(),
  h1('Neighborhood Nestwatch data access portal'),
  hr(),
  #-------------------------------------------------------------------------------*
  # ---- UI: REGIONAL SELECTION ----
  #-------------------------------------------------------------------------------*
  h3('Please enter your study region and password:'),
  br(),
  fluidRow(
    column(4,
           selectInput('hub',label = 'Regional hub:', choiceRegions)),
    column(4,
           textInput('password', label = 'Password:', '')),
    column(4, '')),
  #-------------------------------------------------------------------------------*
  # ---- UI: WRONG PASSWORD ----
  #-------------------------------------------------------------------------------*
  shinyjs::hidden(
    hr(),
    div(
      id = "wrongPasswordMessage",
      h4("I'm sorry, but you entered the wrong password to access to these data.",
         style = 'color:blue')
      )
    ),
  #-------------------------------------------------------------------------------*
  # ---- UI: RIGHT PASSWORD ----
  #-------------------------------------------------------------------------------*
  hr(),
  actionButton('bundleVisitData', 'Collect Visit Data', class = 'btn-primary'),
  br(), br(),
  tableOutput('table'),
  br(),
  shinyjs::hidden(
    downloadButton("downloadVisitData", 
                   "Download visit data", 
                   class = "btn-primary")
  ), 
  br(),
  fluidRow(
    column(4, ''),
    column(4, actionButton("bundleEncounterData", 
                           "Download encounter data", 
                           class = "btn-primary")),
    column(4, actionButton("bundlePointCountData", 
                           "Download point count data", 
                           class = "btn-primary"))
    ),
  #-------------------------------------------------------------------------------*
  # ---- UI: LOGO ----
  #-------------------------------------------------------------------------------*
  hr(),
  fluidRow(
    column(4, ''),
    column(4, imageOutput('nnLogo')),
    column(4, '')
  )
)

server <- function(input, output, session) {
  # Visit data:
  visitTable <- reactive(
    data <- dataBundler('visitData') %>%
      dplyr::rename(date = dateOut, obs = observer, 
                    long = longitude, lat = latitude) %>%
      dplyr::select(hub, site, date, obs:notes)
    #if (input$hub != 'All regions') data =  filter(data, hub == input$hub)
    )
  
  # Encounter data:
  encounterTable <- reactive(dataBundler('encounterData'))
  
  #-------------------------------------------------------------------------------*
  # ---- SERVER: ACCESS PASSWORD CHECK ----
  #-------------------------------------------------------------------------------*
  hubPassword <- reactive(hubPasswordFrame %>%
    filter(hub == input$hub) %>% .$password)
  
  onclick('bundleVisitData',
          if(input$password != hubPassword()){
            shinyjs::toggle('wrongPasswordMessage')
          } else {
            shinyjs::hide('wrongPasswordMessage')
            shinyjs::show('downloadVisitData')
            # output$table <- renderTable(visitTable())
            output$table <- DT::renderDataTable({
              DT::datatable(visitTable())
            })
          }
  )
  
  output$downloadVisitData <- downloadHandler(
    filename =  str_c('visitData', 
                      as.Date(Sys.time()), 
                      input$hub, '.csv'),
    content = function(file){
      write.csv(visitTable(), file)
    }
  )
  
  content = function(file) {
    write.csv(datasetInput(), file)
  }

    downloadHandler(
      filename = function() { paste(input$dataset, '.csv', sep='') },
      content = function(file) {
        write.csv(datasetInput(), file)
      }
    )
  
  
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
  
  #-------------------------------------------------------------------------------*
  # ---- SERVER: IMAGES ----
  #-------------------------------------------------------------------------------*
  
  # Nestwatch logo:
  
  output$nnLogo <- renderImage({
    list(src = 'logo.jpg',
         width = 312,
         height = 100, 
         position = 'middle')
  }, deleteFile = FALSE)
}

shinyApp(ui, server)
