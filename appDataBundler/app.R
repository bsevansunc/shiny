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
library(tidyr)

token <- drop_auth()
saveRDS(token, 'droptoken.rds')

# Data binding function:

dataBundler <- function(classOfData){
  pathToFile <- paste('nnDataStorage',
                      classOfData,
                      sep = '/')
  filePaths <- drop_dir(pathToFile) %>% 
    as.data.frame %>% 
    .$path
  if(length(filePaths >0)){
    outList <- vector('list', length = length(filePaths))
    for(i in 1:length(filePaths)){
      outList[[i]] <- drop_read_csv(filePaths[i], stringsAsFactors = FALSE)
    }
    outFrame <- do.call(rbind, outList)
    if(classOfData == 'encounterData'){
      outFrame <- outFrame %>%
        rename(hub = hubv, site = sitev)
    }
  } else {outFrame <- 'There do not appear to be any files!'}
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
  # VISIT DATA:
  hr(),
  actionButton('bundleVisitData', 'Collect visit data', class = 'btn-primary'),
  br(), br(),
  DT::dataTableOutput("vTable"),
  br(),
  shinyjs::hidden(
    downloadButton("downloadVisitData", 
                   "Download visit data", 
                   class = "btn-primary")
  ), 
  # ENCOUNTER DATA:
  hr(),
  actionButton('bundleEncounterData', 
               'Collect encounter data', class = 'btn-primary'),
  br(), br(),
  DT::dataTableOutput("eTable"),
  br(),
  shinyjs::hidden(
    downloadButton("downloadEncounterData", 
                   "Download encounter data", 
                   class = "btn-primary")
  ), 
  # POINT COUNT DATA:
  hr(),
  actionButton('bundlePcData', 
               'Collect point count data', class = 'btn-primary'),
  br(), br(),
  DT::dataTableOutput("pTable"),
  br(),
  shinyjs::hidden(
    div(
      id = "noFilesMessage",
      h4('There do not appear to be any files!',
         style = 'color:red')
    )
  ),
  shinyjs::hidden(
    downloadButton("downloadPcData", 
                   "Download point count data", 
                   class = "btn-primary")
  ),
  br(),
  hr(),
  #-------------------------------------------------------------------------------*
  # ---- UI: LOGO ----
  #-------------------------------------------------------------------------------*
  fluidRow(
    column(4, ''),
    column(4, imageOutput('nnLogo')),
    column(4, '')
  )
)

server <- function(input, output, session) {
  
  # Password check:
  
  hubPassword <- reactive(hubPasswordFrame %>%
    filter(hub == input$hub) %>% .$password)
  
  #-------------------------------------------------------------------------------*
  # ---- SERVER: VISIT DATA BUNDLE, SHOW, AND DOWNLOAD ----
  #-------------------------------------------------------------------------------*
  
  # Password check and visit table rendering:

  onclick('bundleVisitData',
          if(input$password != hubPassword()){
            shinyjs::toggle('wrongPasswordMessage')
          } else {
            shinyjs::hide('wrongPasswordMessage')
            shinyjs::show('downloadVisitData')
            output$vTable <- DT::renderDataTable({
              DT::datatable(visitData())
            })
          }
  )
  
  
  # Bundle visit data:
  
  visitTable <- reactive(
    withProgress(message = 'Collecting visit data files', dataBundler('visitData')
    )
  )
  
  # Format visit data:
  
  visitData <- reactive(
    visitTable() %>%
      distinct() %>%
      rename(date = dateOut) %>%
      mutate(netHours = ((netCount6 * netTime6) +
                           (netCount9 * 1.5 * netTime9) +
                           (netCount12 * 2 * netTime12) +
                           (netCount18 * 3 * netTime18))/60,
             netHours = ifelse(is.na(netHours), 'NoNets', netHours)
             ) %>%
      select(-c(netCount6:netTime18)) %>%
      tidyr::gather('spUnbanded', 'countUnbanded', amroUnbanded:unchUnbanded) %>%
      group_by(site, date) %>%
      mutate(spUnbanded = spUnbanded %>%
               str_replace('Unbanded', ''),
             spUnbanded = ifelse(sum(countUnbanded, na.rm = TRUE) >= 1,
                                 spUnbanded, 'None')
             ) %>%
      ungroup %>%
      distinct %>%
      mutate(countUnbanded = ifelse(spUnbanded == 'None', 'NA', countUnbanded))
  )
  
  # Download visit data:
  
  output$downloadVisitData <- downloadHandler(
    filename =  str_c('visitData', 
                      as.Date(Sys.time()), 
                      input$hub, '.csv'),
    content = function(file){
      write.csv(visitData(), file)
    }
  )
  
  #-------------------------------------------------------------------------------*
  # ---- SERVER: ENCOUNTER DATA BUNDLE, SHOW, AND DOWNLOAD ----
  #-------------------------------------------------------------------------------*
  
  # Password check and encounter table rendering:
  
  onclick('bundleEncounterData',
          if(input$password != hubPassword()){
            shinyjs::toggle('wrongPasswordMessage')
          } else {
            shinyjs::hide('wrongPasswordMessage')
            shinyjs::show('downloadEncounterData')
            output$eTable <- DT::renderDataTable({
              DT::datatable(encounterData())
            })
          }
  )
  
  # Bundle visit data:
  
  encounterTable <- reactive(
    withProgress(message = 'Collecting encounter data files', 
                 dataBundler('encounterData')
    )
  )
  
  # Format encounter data:
  
  encounterData <- reactive(
    encounterTable() %>%
      distinct %>%
      rename(date = datev)
  )
  
  # Download encounter data:
  
  output$downloadEncounterData <- downloadHandler(
    filename =  str_c('encounterData', 
                      as.Date(Sys.time()), 
                      input$hub, '.csv'),
    content = function(file){
      write.csv(encounterData(), file)
    }
  )
  
  #-------------------------------------------------------------------------------*
  # ---- SERVER: POINT COUNT DATA BUNDLE, SHOW, AND DOWNLOAD ----
  #-------------------------------------------------------------------------------*
  
  # Password check and encounter table rendering:
  
  onclick('bundlePcData',
          if(input$password != hubPassword()){
            shinyjs::toggle('wrongPasswordMessage')
          } else {
            shinyjs::hide('wrongPasswordMessage')
            shinyjs::show('downloadPcData')
            if(pTable() != 'There do not appear to be any files!'){
              output$pTable <- DT::renderDataTable({
                DT::datatable(pData())
                })
            } else {
              shinyjs::show('noFilesMessage')
              }
          }
  )
  
  # Bundle visit data:
  
  pTable <- reactive(
    withProgress(message = 'Collecting point count data files', 
                 dataBundler('pData')
    )
  )
  
  # Format encounter data:
  
  pData <- reactive(
    pTable() %>%
      distinct %>%
      rename(date = datev)
  )
  
  # Download encounter data:
  
  output$downloadPcData <- downloadHandler(
    filename =  str_c('pData', 
                      as.Date(Sys.time()), 
                      input$hub, '.csv'),
    content = function(file){
      write.csv(pData(), file)
    }
  )
  
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
