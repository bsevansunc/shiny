#=================================================================================*
# ---- SET-UP ----
#=================================================================================*

# Sources used:

# Form input: http://deanattali.com/2015/06/14/mimicking-google-form-shiny/
# CRUD application: http://ipub.com/shiny-crud-app/
# DT instructions: http://rstudio.github.io/DT/shiny.html
# Data storage: http://deanattali.com/blog/shiny-persistent-data-storage/
# R-Dropbox Package: https://cran.r-project.org/web/packages/rdrop2/rdrop2.pdf

# Libraries:

library(markdown)
library(shiny)
library(dplyr)
library(stringr)
library(shinyBS)
library(R.utils)
library(rdrop2)
# library(googlesheets)
library(DT)
library(shinyjs)

#---------------------------------------------------------------------------------*
# ---- Some data loading ----
#---------------------------------------------------------------------------------*

# All possible AOU codes (for point counts):

aouCodes <- read.csv('speciesAouCodes.csv', stringsAsFactors = FALSE) %>%
  tbl_df %>%
  rename(SpNumber = Species.Number, 
         Alpha = Alpha.Code, 
         Common = Common.Name, 
         Scientific = Scientific.Name)

justAlphaCode <- aouCodes %>% 
  select(Alpha) %>%
  distinct %>%
  .$Alpha

# Load encounter data:

encounters <- read.csv('encounters.csv', stringsAsFactors = F) %>%
  tbl_df %>%
  mutate(date = as.Date(date),
         bandNumber = as.character(bandNumber))

# Load species by hub data:

hubSpecies <- read.csv('hubSpecies.csv', stringsAsFactors = F) %>%
  tbl_df

#---------------------------------------------------------------------------------*
# ---- Source functions ----
#---------------------------------------------------------------------------------*

# The functions that make things go:

source('helperFunctions.R', local=TRUE)

# Entries for drop-down menu items:

source('fieldOptions.R', local = TRUE)

# Paragraphs, field descriptions, and such:

source('textComponents.R', local = TRUE)

#=================================================================================*
# ---- USER INTERFACE ----
#=================================================================================*

ui <- navbarPage(
  "Neighborhood Nestwatch technician data submission interface",
  #-------------------------------------------------------------------------------*
  # ---- UI TAB PANEL: VISIT ----
  #-------------------------------------------------------------------------------*
  tabPanel(strong('Visit data'),
           sidebarLayout(
              sidebarPanel(
                br(),
                # ---- Site and observer -----------------------------------------
                h4(strong('Visit:')),
                br(),
                fluidRow(
                  column(4,
                    selectInput('hub','Regional Hub:', choiceRegions)),
                    column(8,
                      selectInput('site', 'Site', ''))),
                br(),
                fluidRow(
                  column(6,
                         dateInput('date',
                                   label = 'Date: yyyy-mm-dd',
                                   value = Sys.Date()
                         )),
                  column(6,
                         textInput("observer", "Observer initials:"))),
                br(),
                # ---- Banding effort --------------------------------------------
                h4(strong('Banding effort:')),
                br(),
                fluidRow(
                  column(6, selectizeInput('netCount', 'Number of nets:', 
                                           choices = choiceNetCount)),
                  column(6, selectizeInput('netHours', 'Net hours:', 
                                           choices = choiceNetHours))),
                br(),
                # ---- Resight effort --------------------------------------------
                h4(strong('Resight effort:')),
                br(),
                fluidRow(
                  column(4, selectizeInput('startRsTime', 'Resight, start time:', 
                                           choices = choiceTimeOfDay)),
                  column(4, selectizeInput('endRsTime', 'Resight, end time:', 
                                           choices = choiceTimeOfDay)),
                  column(4, textInput('rsPathDistance',
                                      'Path distance traveled (m):'))),
                br(),
                h5(strong('Observed, unbanded:')),
                fluidRow(column(1, 'AMRO'),
                         column(2, selectizeInput(
                           'amroUnbanded', label = NULL,
                           choiceCount)),
                         column(1, 'BCCH'),
                         column(2, selectizeInput(
                           'bcchUnbanded', label = NULL,
                           choiceCount)),
                         column(1, 'BRTH'),
                         column(2, selectizeInput(
                           'brthUnbanded', label = NULL,
                           choiceCount)),
                         column(1, 'CACH'),
                         column(2, selectizeInput(
                           'cachUnbanded', label = NULL,
                           choiceCount))),
                fluidRow(column(1, 'CARW'),
                         column(2, selectizeInput(
                           'carwUnbanded', label = NULL,
                           choiceCount)),
                         column(1, 'EAPH'),
                         column(2, selectizeInput(
                           'eaphUnbanded', label = NULL,
                           choiceCount)),
                         column(1, 'GRCA'),
                         column(2, selectizeInput(
                           'grcaUnbanded', label = NULL,
                           choiceCount)),
                         column(1, 'HOWR'),
                         column(2, selectizeInput(
                           'howrUnbanded', label = NULL,
                           choiceCount))),
                fluidRow(column(1, 'NOCA'),
                         column(2, selectizeInput(
                           'nocaUnbanded', label = NULL,
                           choiceCount)),
                         column(1, 'NOMO'),
                         column(2, selectizeInput(
                           'nomoUnbanded', label = NULL,
                           choiceCount)),
                         column(1, 'SOSP'),
                         column(2, selectizeInput(
                           'sospUnbanded', label = NULL,
                           choiceCount)),
                         column(1, 'TUTI'),
                         column(2, selectizeInput(
                           'tutiUnbanded', label = NULL,
                           choiceCount))),
                fluidRow(column(1, 'UNCH'),
                         column(2, selectizeInput(
                           'unchUnbanded', label = NULL,
                           choiceCount
                         )),
                         column(9, '')),
                br(),
                h4(strong('Did you band or encounter a banded bird during your visit?')),
                radioButtons('encounteredBirds', label = NULL,
                             choices = list('Yes' = 1, 'No' = 2), 
                             selected = 1),
                br(),
                h4(strong('Visit notes:')),
                br(),
                #div(id = "form", ...),
                # ---- Notes -----------------------------------------------------
                fluidRow(column(12, textInput('visitNotes', 
                                              label = NULL))),
                br(),
                actionButton("submitVisitData", "Submit visit data", 
                             class = "btn-primary"),
                shinyjs::hidden(
                  div(
                    id = "thankyou_msgVisit",
                    h3("Thanks, your visit data have been recorded!")
                  )
                ),
                width = 6, position = 'right'),
            # ---- Visit text ----------------------------------------------------
            mainPanel(
              textVisit, hr(),
              ttRegion, ttSite, ttVisitDate, ttVisitObserver, ttNetCount,
              ttNetHours, ttStartEndResightTime, ttPathDistance,
              ttObservedUnbanded, ttEncounteredBirds, ttVisitNotes, 
              width = 6, position = 'left'))),
  #-------------------------------------------------------------------------------*
  # ---- UI TAB PANEL: ENCOUNTERS ----
  #-------------------------------------------------------------------------------*
  tabPanel(strong('Encounter data'),
           div(id = 'encounterData', 
               sidebarLayout(
             # ---- Record entry -------------------------------------------------
             sidebarPanel(
               shinyjs::useShinyjs(),
               h3(strong('1. Enter encounter record:')),
               br(),
               fluidRow(
                 column(2, shinyjs::disabled(textInput("id", "Id", "0"))),
                 column(5, selectInput('sitev', 'Site', '')),
                 column(5, dateInput('datev',
                                     label = 'Date: yyyy-mm-dd',
                                     value = Sys.Date()
                 ))
                 ), br(),
               fluidRow(
                 column(4, selectizeInput('bandTime', 'Time:',
                                          choices = choiceTimeOfDay)),
                 column(4, textInput('bander', 'Observer initials:')),
                 column(4, selectizeInput('encounterType', 
                                          'Encounter type:',
                                          choices = choiceEncounterType, 
                                          selected = 'Band'))),
               br(),
               fluidRow(
                 column(4, selectInput('species', 'Species:', '')),
                 column(4, textInput('bandNumber', 'Band number:')),
                 column(4, selectizeInput('colorCombo', 
                                          'Color combination:',
                                          choices = choiceColorCombos))),
               hr(),
               h4(strong('Color bands:')),
               fluidRow(
                 column(3, p(strong('- :'), 'No band')),
                 column(3, p(strong('BK :'), 'Black')),
                 column(3, p(strong('PK :'), 'Pink')),
                 column(3, p(strong('Y :'), 'Yellow'))
               ),
               fluidRow(
                 column(3, p(strong('A :'), 'Aluminum')),
                 column(3, p(strong('G :'), 'Green')),
                 column(3, p(strong('P :'), 'Purple')),
                 column(3, p(strong('W :'), 'White'))
               ),
               fluidRow(
                 column(3, p(strong('BU :'), 'Blue')),
                 column(3, p(strong('O :'), 'Orange')),
                 column(3, p(strong('R :'), 'Red')),
                 column(3, ' ')
               ),
               hr(),
               fluidRow(
                 column(3, selectizeInput('age','Age:',
                                          choices = choiceAge)),
                 column(3, selectizeInput('sex', label = 'Sex:',
                                          choices = choiceSex)),
                 column(3, selectizeInput('breedingCond', label = 'CP/BP:',
                                          choices = choiceBreedingCond)),
                 column(3, selectizeInput('fat',label = 'Fat:',
                                          choices = choiceFat))),
               br(),
               fluidRow(
                 column(3, textInput('mass',label = 'Mass (g):')),
                 column(3, textInput('wing',label = 'Wing (mm):')),
                 column(3, textInput('tail',label = 'Tail (mm):')),
                 column(3, textInput('tarsus',label = 'Tarsus (mm):'))),
               br(),
               fluidRow(
                 column(6, textInput('featherID', label = 'Feather ID:')),
                 column(6, '')),
               br(),
               fluidRow(
                 column(12, textInput('notes', label = 'Notes:'))),
               br(),
               fluidRow(column(1, ''),
                        column(3, actionButton("newRecord", "Clear fields",
                                               class = 'btn-primary')),
                        column(2, ''),
                        column(3, actionButton('submitRecord', 'Add record to table',
                                               class = "btn-primary")),
                        column(3, '')),
               br(),
               width = 6, position = 'right'),
             # ---- Encounter text ----------------------------------------------------
             mainPanel(
               textBandingIntro,
               textBandingIntro1,
#                textBandingList1,
#                textBandingIntro2,
#                textBandingList2,
               hr(),
               ttId, ttBandTime, ttBanderInitials, ttEncounterType,
               ttSpecies, ttBandNumber, ttColorCombo, ttAgeThroughFat, 
               ttMassThroughTarsus,
               ttFeatherID,
               width = 6, position = 'left')
             ),
           hr(),
           # ---- QC and submission ---------------------------------------------------
           h3(strong('2. Data-proofing and submission of encounter records:')),
           br(),
           DT::dataTableOutput("responses"),
           br(),
           fluidRow(column(1, ''),
                    column(4, actionButton("delete", "Delete record", 
                                           class = "btn-primary")),
                    column(3, ' '),
                    column(4, actionButton('submitEncounterData', 
                                           'Submit encounter data',
                                             class = "btn-primary"))
                    ),
           br(), 
           shinyjs::hidden(
             div(
               id = "thankyou_msgEncounter",
               h3("Thanks, your encounter data have been recorded!")
             )
           ),
           br()
           )),
  #--------------------------------------------------------------------------------*
  # ---- UI TAB PANEL: QUERY RECORDS ----
  #--------------------------------------------------------------------------------*
  tabPanel(strong('Query encounter records'),
           textQuery,
           fluidRow(column(11, DT::dataTableOutput('encounterTable')))
  ),
  #--------------------------------------------------------------------------------*
  # ---- UI TAB PANEL: POINT COUNT ----
  #--------------------------------------------------------------------------------*
  tabPanel(strong('Point count data'),
           sidebarLayout(
             sidebarPanel(
               div(id = 'pointCountVisitInfo',
                   shinyjs::useShinyjs(),
                   h3(strong('1. Submit site-level point count data:')),
                   br(),
                   fluidRow(
                     column(3, selectInput('sitePc', 'Site:', '')),
                     column(3, textInput('observerPc', 'Observer initials:')),
                     column(3, dateInput('datePc',
                                       label = 'Date: yyyy-mm-dd',
                                       value = Sys.Date())),
                     column(3, selectizeInput('timePc', 'Start time:',
                                              choices = choiceTimeOfDay))
                   ),
                   br(),
                   h4(strong('Location')),
                   fluidRow(
                     column(4, 
                            textInput('longitude', 
                                      'Longitude (decimal degrees):')),
                     column(4, 
                            textInput('longitude', 
                                      'Longitude (decimal degrees):')),
                     column(4,
                            textInput('accuracy', 'Accuracy (m):'))
                   ),
                   fluidRow(
                     column(12, textInput('locationNotes', 
                                          label = 'Location notes:'))),
                   br(),
                   h4(strong('Weather')),
                   fluidRow(
                     column(4, selectizeInput('temperature', 'Temperature (C):',
                                              choices = c('',seq(-10, 40)))),
                     column(4, selectizeInput('sky', 'Sky (0-5):',
                                              choices = c('',seq(0, 5)))),
                     column(4, selectizeInput('wind', 
                                              'Wind (Beaufort number, 0-6):',
                                              choices = c('',seq(0, 6))))
                   ),
                   br(),
                   h4(strong('Ambient noise (dB)')),
                   fluidRow(
                     column(3, selectizeInput('splN', 'N:',
                                              choices = c('',seq(0, 130)))),
                     column(3, selectizeInput('splE', 'E:',
                                              choices = c('',seq(0, 130)))),
                     column(3, selectizeInput('splE', 'S:',
                                              choices = c('',seq(0, 130)))),
                     column(3, selectizeInput('splE', 'W:',
                                              choices = c('',seq(0, 130))))
                   ),
               br(),
               fluidRow(
                 column(12, textInput('siteLevelPcNotes', 
                                      label = 'Site-level point count notes:'))),
               br(),
               actionButton("submitSiteLevelPcData", 
                            "Submit site-level point count data", 
                            class = "btn-primary"),
               shinyjs::hidden(
                 div(
                   id = "thankyou_msgPcSiteLevel",
                   h3("Thanks, your site-level point count data have been recorded!")
                 )
               )),
               hr(),
               div(id = 'pcData',
                 # ---- Point count entry ------------------------------------------
                   h3(strong('2. Enter point count observation records:')),
                   br(),
                   fluidRow(
                     column(1, shinyjs::disabled(textInput("idPc", "Id", "0"))),
                     column(11, ' ')
                   ), 
                   fluidRow(
                     column(2, selectizeInput('timeInterval', 'Time:',
                                              choices = c('',seq(1, 10)))),
                     column(3, selectizeInput('pcSpecies', 
                                              'Species:',
                                              choices = c('',justAlphaCode))),
                     column(2, selectizeInput('distancePc', 'Distance:',
                                              choices = c('',0:100))),
                     column(2, selectizeInput('pcCount', 'Count:', 
                                              choices = c('',1:100))), 
                     column(3, selectizeInput('detectionMethodPc',
                                              'Detection:',
                                              choices = c('', 'Visual','Auditory', 'Both')))
                     ),
                 br(),
                 fluidRow(column(1, ''),
                          column(3, actionButton("newRecordPc", "Clear fields",
                                               class = 'btn-primary')),
                          column(2, ''),
                          column(3, actionButton('submitRecordPc', 'Add record to table',
                                               class = "btn-primary")),
                          column(3, '')),
                 br()),
                   width = 6, position = 'right'),
                  # ---- PC text ----------------------------------------------------
                 mainPanel(
                   textPcIntro0,
                   textPcIntro1,
                   hr(),
                   fieldDescriptionsPc,
                   hr(),
                   textAouQuery,
                   fluidRow(column(11, DT::dataTableOutput('aouTable'))),
                   width = 6, position = 'left')
               ),
               hr(),
               # ---- QC and submission ---------------------------------------------------
               h3(strong('3. Data-proofing and submission of point count records:')),
               br(),
               DT::dataTableOutput("responsesPc"),
               br(),
               fluidRow(column(1, ''),
                        column(4, actionButton("deleteRecordPc",
                                               "Delete point count record", 
                                               class = "btn-primary")),
                        column(3, ' '),
                        column(4, actionButton('submitPcData', 
                                               'Submit point count data',
                                               class = "btn-primary"))
               ),
               br(), shinyjs::hidden(
                div(
                  id = "thankyou_msgPc",
                  h3("Thanks, your point count data have been recorded!")
                )
              ),
              br()),
  #-------------------------------------------------------------------------------*
  # ---- UI TAB PANEL: NEST DATA ----
  #-------------------------------------------------------------------------------*
  tabPanel(strong('Nest data')),
  #-------------------------------------------------------------------------------*
  # ---- UI TAB PANEL: HABITAT SURVEY ----
  #-------------------------------------------------------------------------------*
  tabPanel(strong('Habitat survey data'))
  )

#=================================================================================*
# ---- SERVER ----
#=================================================================================*

server <- function(input, output, session) {
  # Update site drop-down menu as a function of the hub:
  observe({
    inHub <- input$hub
    print(inHub)
    if(is.null(inHub))
      return(NULL)
    siteNames <- encounters %>%
      select(hub, site) %>%
      distinct %>%
      filter(hub == inHub) %>%
      arrange(site) %>%
      .$site
    updateSelectInput(session, 'site', choices = siteNames)
    updateSelectInput(session, 'sitev', choices = siteNames)
  })
  
  # Update species drop-down menu by hub:
  observe({
    inHub <- input$hub
    print(inHub)
    if(is.null(inHub))
      return(NULL)
    siteNames <- hubSpecies %>%
      filter(hub == inHub) %>%
      arrange(species) %>%
      .$species
    updateSelectInput(session, 'species', choices = siteNames)
  })
  
  #-------------------------------------------------------------------------------*
  # ---- SERVER: VISIT DATA ----
  #-------------------------------------------------------------------------------*
  # Link fields to input:
  visitData <- reactive({
    dateOut <- as.character(input$date)
    data <- t(sapply(visitFields, function(x) input[[x]]))
    cbind(dateOut, data)
  })
  
  # When the Submit button is clicked, save form data:
  observeEvent(input$submitVisitData, {
    saveVisitData(visitData())
    shinyjs::show("thankyou_msgVisit")
  })
  
  #-------------------------------------------------------------------------------*
  # ---- SERVER: ENCOUNTER DATA ----
  #-------------------------------------------------------------------------------*
  
  # Link field names and data:
  
  # shinyServer(func = function(input, output){ 
    formData <- reactive({
      sapply(names(getTableMetadata()$fields), function(x) input[[x]])
    })
  
  # Click "Submit record" button to push result to table:
  
  observeEvent(input$submitRecord, {
    dateOut <<- as.character(input$datev) # added mar 25
    if (input$id != "0") {
      updateData(formData())
    } else {
      createData(formData())
      updateInputs(createDefaultRecord(), session)
    }
  }, priority = 1)
  
  # Click "Delete" to remove a single record:
  
  observeEvent(input$delete, {
    dateOut <<- as.character(input$datev) # added mar 25
    deleteData(formData())
    updateInputs(createDefaultRecord(), session)
  }, priority = 1)
  
  # Press "New record" button to display empty record:
  
  observeEvent(input$newRecord, {
    dateOut <<- as.character(input$datev) # added mar 25
    updateInputs(createDefaultRecord(), session)
  })
  
  # Select row in table to show details in inputs:
  
  observeEvent(input$responses_rows_selected, {
    dateOut <<- as.character(input$datev) # added mar 25
    if (length(input$responses_rows_selected) > 0) {
      data <- readData()[input$responses_rows_selected, ]
      updateInputs(data, session)
    }
  })
  
  shinyjs::disable("id")
  
  # Inputs and submissions to temp data file:
  
  reactiveOut <- reactive({
    dateOut <<- as.character(input$datev) # added mar 25
    input$submitRecord
    input$delete
    readData()
  })
  
  # Display output table:
  
  output$responses <- DT::renderDataTable({
    dateOut <<- as.character(input$datev) # added mar 25
    reactiveOut()
    },
    server = FALSE, selection = "single",
    colnames = unname(getTableMetadata()$fields)[-1]
    )
  
  # Submit encounter data from table:
  
  observeEvent(input$submitEncounterData, {
    saveEncounterData(reactiveOut())
    shinyjs::reset("encounterData")
    shinyjs::show("thankyou_msgEncounter")
  })
  
  #-------------------------------------------------------------------------------*
  # ---- SERVER: QUERY BANDING RECORDS ----
  #-------------------------------------------------------------------------------*
  output$encounterTable = DT::renderDataTable(
    datatable(encounters, filter = 'bottom'))
  
  #-------------------------------------------------------------------------------*
  # ---- SERVER: QUERY AOU names ----
  #-------------------------------------------------------------------------------*
  output$aouTable = DT::renderDataTable(
    datatable(aouCodes, filter = 'none', rownames = FALSE,
              options = list(pageLength = 1)))
  #-------------------------------------------------------------------------------*
  # ---- SERVER: SUBMIT BIRD-LEVEL POINT COUNT DATA ----
  #-------------------------------------------------------------------------------*
  
  # Link field names and data:
  
  formDataPc <- reactive({
    sapply(names(getTableMetadataPc()$fields), function(x) input[[x]])
  })
  
  # Click "Submit record" button to push result to table:
  
  observeEvent(input$submitRecordPc, {
    dateOutPc <<- as.character(input$datePc) 
    if (input$idPc != "0") {
      updateDataPc(formDataPc())
    } else {
      createDataPc(formDataPc())
      updateInputsPc(createDefaultRecordPc(), session)
    }
  }, priority = 1)
  
  # Click "Delete" to remove a single record:
  
  observeEvent(input$deleteRecordPc, {
    dateOutPc <<- as.character(input$datePc) 
    deleteDataPc(formDataPc())
    updateInputsPc(createDefaultRecordPc(), session)
  }, priority = 1)
  
  # Press "New record" button to display empty record:
  
  observeEvent(input$newRecordPc, {
    dateOutPc <<- as.character(input$datePc) 
    updateInputsPc(createDefaultRecordPc(), session)
  })
  
  # Select row in table to show details in inputs:
  
  observeEvent(input$responsesPc_rows_selected, {
    dateOutPc <<- as.character(input$datePc) 
    if (length(input$responsesPc_rows_selected) > 0) {
      data <- readDataPc()[input$responsesPc_rows_selected, ]
      updateInputsPc(data, session)
    }
  })
  
  shinyjs::disable("idPc")
  
  # Inputs and submissions to temp data file:
  
  reactiveOutPc <- reactive({
    dateOutPc <<- as.character(input$datePc) # added mar 25
    input$submitRecordPc
    input$deletePc
    readDataPc()
  })
  
  # Display output table:
  
  output$responsesPc <- DT::renderDataTable({
    dateOutPc <<- as.character(input$datePc) # added mar 25
    reactiveOutPc()
  },
  server = FALSE, selection = "single",
  colnames = unname(getTableMetadataPc()$fields)[-1]
  )
  
  # Submit encounter data from table:
  
  observeEvent(input$submitPcData, {
    savePcData(reactiveOutPc())
    shinyjs::reset("pcData")
    shinyjs::show("thankyou_msgPc")
  })
  # END
  }

shinyApp(ui, server)

#rsconnect::deployApp()