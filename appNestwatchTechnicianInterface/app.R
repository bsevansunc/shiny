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
library(DT)
library(shinyjs)

#---------------------------------------------------------------------------------*
# ---- Some data loading ----
#---------------------------------------------------------------------------------*

# All possible AOU codes (for point counts):

aouCodes <- read.csv('speciesAouForApp.csv', stringsAsFactors = FALSE) 

justAlphaCode <- aouCodes %>% .$Alpha

# Load encounter data:

encounters <- read.csv('encounters.csv', stringsAsFactors = FALSE) %>%
  mutate(date = as.Date(date))

# Load species by hub data:

hubSpecies <- read.csv('hubSpecies.csv', stringsAsFactors = FALSE) %>%
  tbl_df

# Load color combinations:

choiceColorCombos <- read.csv('choiceColorCombos.csv',
                              stringsAsFactors = FALSE) %>%
  .$choiceColorCombos

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
  title = p(
    h4(strong("Neighborhood Nestwatch technician data submission interface")),
    br()
    ),
  windowTitle = 'Neighborhood Nestwatch Technician data submission',
  footer = p(
    br(),
    hr(),
    fluidRow(
      column(4, ''),
      column(4, imageOutput('nnLogo')),
      column(4, '')
    )
  ),
  inverse = TRUE,
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
                hr(),
                h4(strong('Location:')),
                br(),
                fluidRow(
                  column(4, 
                         textInput('longitude', 
                                   'Longitude:')),
                  column(4, 
                         textInput('latitude', 
                                   'Latitude:')),
                  column(4,
                         textInput('accuracy', 'Accuracy (m):'))
                ),
                fluidRow(
                  column(12, textInput('locationNotes', 
                                       label = 'Location notes:'))),
                hr(),
                # ---- Banding effort --------------------------------------------
                h4(strong('Banding effort:')),
                br(),
                fluidRow(
                  column(6, selectizeInput('netCount6', 'Number of 6 m nets:', 
                                           choices = choiceNetCount)),
                  column(6, selectizeInput('netTime6', 
                                           'Total time open (minutes), 6 m nets:', 
                                           choices = choiceNetMinutes))),
                fluidRow(
                  column(6, selectizeInput('netCount9', 'Number of 9 m nets:', 
                                           choices = choiceNetCount)),
                  column(6, selectizeInput('netTime9', 
                                           'Total time open (minutes), 9 m nets:', 
                                           choices = choiceNetMinutes))),
                fluidRow(
                  column(6, selectizeInput('netCount12', 'Number of 12 m nets:', 
                                           choices = choiceNetCount)),
                  column(6, selectizeInput('netTime12', 
                                           'Total time open (minutes), 12 m nets:', 
                                           choices = choiceNetMinutes))),
                fluidRow(
                  column(6, selectizeInput('netCount18', 'Number of 18 m nets:', 
                                           choices = choiceNetCount)),
                  column(6, selectizeInput('netTime18', 
                                           'Total time open (minutes), 18 m nets:', 
                                           choices = choiceNetMinutes))),
                hr(),
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
                hr(),
                h4(strong('Did you band or encounter a banded bird during your visit?')),
                radioButtons('encounteredBirds', label = NULL,
                             choices = list('Yes' = 1, 'No' = 2), 
                             selected = 1),
                hr(),
                h4(strong('Visit notes:')),
                br(),
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
            mainPanel(textVisit, width = 6, position = 'left'))
           ),
  #-------------------------------------------------------------------------------*
  # ---- UI TAB PANEL: ENCOUNTERS ----
  #-------------------------------------------------------------------------------*
  tabPanel(strong('Encounter data'),
           div(id = 'encounterData', 
               sidebarLayout(
                 # ---- Record entry -------------------------------------------------*
                 sidebarPanel(
                   shinyjs::useShinyjs(),
                   h3(strong('1. Enter encounter record:')),
                   strong(em('IMPORTANT! Be sure to enter encounter data AFTER entering visit data!')),
                   br(), br(),
                   fluidRow(
                     column(4,
                            selectInput('hubEnc','Regional Hub:',
                                        choices = choiceRegions,
                                        selected = NULL)),
                     column(4, selectInput('siteEnc', 'Site', '')),
                     column(4, dateInput('dateEnc',
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
                     column(4, selectInput('speciesEnc', 'Species:', '')),
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
                     column(3, textInput('tl',label = 'Tail (mm):')),
                     column(3, textInput('tarsus',label = 'Tarsus (mm):'))),
                   br(),
                   fluidRow(
                     column(6, textInput('featherID', label = 'Feather ID:')),
                     column(6, textInput('toenailID', label = 'Toenail ID:'))),
                   br(),
                   fluidRow(
                     column(6, textInput('bloodID', label = 'Blood ID:')),
                     column(6, textInput('fecalID', label = 'Fecal ID:'))),
                   br(),
                   fluidRow(
                     column(6, textInput('attachmentID',
                                         label = 'Attachment ID (e.g., transmitter or geolocator):')),
                     column(6, ' ')),
                   br(),
                   fluidRow(
                     column(12, textInput('notesEnc', label = 'Notes:'))),
                   br(),
                   fluidRow(column(1, ''),
                            column(3, actionButton("newEnc", "Clear fields",
                                                   class = 'btn-primary')),
                            column(2, ''),
                            column(3, actionButton('submitEnc', 'Add record to table',
                                                   class = "btn-primary")),
                            column(3, '')),
                   br(),
                   width = 6, position = 'right'),
                 # ---- Encounter text ----------------------------------------------------
                 mainPanel(
                   introTextEncounter,
                   hr(),
                   fieldDescriptionsEncounter,
                   width = 6, position = 'left')
               ),
               hr(),
               # ---- QC and submission ---------------------------------------------------
               h3(strong('2. Data-proofing and submission of encounter records:')),
               br(),
               DT::dataTableOutput("responsesEnc"),
               br(),
               fluidRow(column(1, ''),
                        column(4, actionButton("deleteEnc", "Delete record", 
                                               class = "btn-primary")),
                        column(3, ' '),
                        column(4, actionButton('submitEnc', 
                                               'Submit encounter data',
                                               class = "btn-primary"))
               ),
               br(), 
               shinyjs::hidden(
                 div(
                   id = "thankyou_msgEnc",
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
               div(id = 'pcData',
                 # ---- Point count entry ------------------------------------------
                   h3(strong('1. Add point count records:')),
                   strong(em('IMPORTANT! Be sure to enter point count data AFTER entering visit data!')),
                   br(), br(),
                   fluidRow(
                     column(2, shinyjs::disabled(textInput("idPc", "Id", "0"))),
                     column(5, selectInput('sitePc', 'Site:', '')),
                     column(2, textInput('observerPc', 'Observer:')),
                     column(3, '')
                   ),
                   fluidRow(
                     column(5, dateInput('datePc',
                                         label = 'Date: yyyy-mm-dd',
                                         value = Sys.Date())),
                     column(3, selectizeInput('startTimePc', 'Start time:',
                                              choices = choiceTimeOfDay)),
                     column(4, '')
                   ), 
                   hr(),
                   fluidRow(column(12, textInput('notesPc', 
                                                 label = 'Point count notes:'))),
                   hr(),
                   textAouQuery,
                   fluidRow(column(11, DT::dataTableOutput('aouTable'))),
                   hr(),
                   fluidRow(
                     column(2, selectizeInput('timePc', 'Time:',
                                              choices = choiceTime)),
                     column(3, selectizeInput('speciesPc', 
                                              'Species:',
                                              choices = c('',justAlphaCode))),
                     column(2, selectizeInput('distancePc', 'Distance:',
                                              choices = choiceDistance)),
                     column(2, selectizeInput('countPc', 'Count:', 
                                              choices = c('',1:100))), 
                     column(3, selectizeInput('detectionPc',
                                              'Detection:',
                                              choices = c('', 'Visual','Auditory', 'Both')))
                     ),
                 hr(),
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
                   introTextPc,
                   hr(),
                   fieldDescriptionsPc,
                   # hr(),
                   # textAouQuery,
                   # fluidRow(column(11, DT::dataTableOutput('aouTable'))),
                   width = 6, position = 'left')
               ),
               hr(),
               # ---- QC and submission ---------------------------------------------------
               h3(strong('2. Data-proofing and submission of point count records:')),
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
  tabPanel(strong('Nest data'),
           h1('Coming Soon!')
#            sidebarLayout(
#              sidebarPanel(
#                div(id = 'summaryDataNest',
#                    h3(strong('1. Submit nest location and summary data:')),
#                    strong(em('IMPORTANT! Be sure to enter nest data AFTER entering visit data!')),
#                    br(), br(),
#                    fluidRow(
#                      column(3, strong('Species:')),
#                      column(3, strong('Color combo:')),
#                      column(3, strong('FWS Bands:')),
#                      column(3, strong('Nest: Yr-#'))
#                    ),
#                    fluidRow(
#                      column(3, selectInput('speciesNest', ' ', '')),
#                      column(3, selectInput('colorComboMaleNest', 
#                                            'Male:',
#                                            choices = choiceColorCombos)),
#                      column(3, textInput('bandNumberMaleNest', 'Male:')),
#                      column(3, textInput('nestID', ' '))
#                    ),
#                    fluidRow(
#                      column(3, ''),
#                      column(3, selectInput('colorComboFemaleNest', 
#                                            'Female:',
#                                            choices = choiceColorCombos)),
#                      column(3, textInput('bandNumberFemaleNest', 'Female:')),
#                      column(3, '')
#                    ),
#                    hr(),
#                    fluidRow(
#                      column(9, ''),
#                      column(3, tags$u(strong('Summary dates')))
#                    ),
#                    fluidRow(
#                      column(2, textInput('plotNest', 'Plot:', '')),
#                      column(3, selectInput('siteNest', 'Grid (site):', '')),
#                      column(4, selectInput('fateNest', 'Nest Fate:',
#                                            nestFateChoices)),
#                      column(3, dateInput('dateClutchNest',
#                                         label = 'Clutch completion:',
#                                         value = ''))
#                    ),
#                    fluidRow(
#                      column(7, textInput('plantSpNest', 
#                                          'Plant species (if applicable):')),
#                      column(2, selectInput('heightNest', 'Nest ht (m):',
#                                            c('', 0:12), '')),
#                      column(3, dateInput('dateHatchNest',
#                                          label = 'Hatch:',
#                                          value = ''))
#                    ),
#                    fluidRow(
#                      column(9, textInput('descriptionNest', 'Nest description:')),
#                      column(3, dateInput('dateFledgeFailNest',
#                                          label = 'Fledge/fail (date):',
#                                          value = ''))
#                    ),
#                    fluidRow(
#                      column(3, selectInput('locationNest', 'Nest Location',
#                                            nestLocationChoices)),
#                      column(9, textInput('notesFateOtherNest', 
#                                          'If nest fate was "other", explain:'))
#                    )
#                ),
#                    hr(),
#                div(id = 'nestData',
#                    h3(strong('2. Add nest observation records:')),
#                    strong(em('IMPORTANT! Be sure to enter nest observation records data AFTER nest location and summary data!')),
#                    br(), br(),
#                    fluidRow(
#                      column(3, dateInput('dateNest',
#                                          label = 'Date:',
#                                          value = '')),
#                      column(3, selectizeInput('timeNest', 'Time:',
#                                               choices = choiceTimeOfDay)),
#                      column(3, selectInput('stageNest', 'Stage:',
#                                            choices = nestStageChoices)),
#                      column(3, selectInput('adAttNest', 'Adult attending:',
#                                            choices = nestAttendChoices))),
#                    fluidRow(
#                      column(2, selectInput('nEggNest', '# egg:',
#                                            choices = c('', 0:10))),
#                      column(2, selectInput('nYoungNest', '# yng:',
#                                            choices = c('', 0:10))),
#                      column(6, textInput('notesNest', 
#                                          'Notes: Building, age, fate, behavior, etc.')),
#                      column(2, textInput('observerNest', 'Obs:'))
#                    ),
#                    hr(),
#                    br(),
#                    fluidRow(column(1, ''),
#                             column(3, actionButton("newRecordNest", 
#                                                    "Clear fields",
#                                                    class = 'btn-primary')),
#                             column(2, ''),
#                             column(3, actionButton('submitRecordNest', 
#                                                    'Add record to table',
#                                                    class = "btn-primary")),
#                             column(3, ''))
#                ),
#              width = 7, position = 'left'),
#              mainPanel = ''
#            ),
#            h3(strong('3. Data-proofing and submission of nest observation records:')),
#            br(),
#            DT::dataTableOutput("responsesNest"),
#            br(),
#            fluidRow(column(1, ''),
#                     column(4, actionButton("deleteRecordNest",
#                                            "Delete nest record", 
#                                            class = "btn-primary")),
#                     column(3, ' '),
#                     column(4, actionButton('saveNestData', 
#                                            'Submit nest data',
#                                            class = "btn-primary"))
#            ),
#            br(), shinyjs::hidden(
#              div(
#                id = "thankyou_msgNest",
#                h3("Thanks, your nest data have been recorded!")
#              )
#            ),
#            br()
           ),
  #-------------------------------------------------------------------------------*
  # ---- UI TAB PANEL: HABITAT SURVEY ----
  #-------------------------------------------------------------------------------*
  tabPanel(strong('Habitat survey data'),
           h1('Coming soon!'))
  )

#=================================================================================*
# ---- SERVER ----
#=================================================================================*

server <- function(input, output, session) {
  #-------------------------------------------------------------------------------*
  # ---- SERVER: REACTIVE INPUTS ----
  #-------------------------------------------------------------------------------*
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
    updateSelectInput(session, 'siteEnc', choices = siteNames)
    updateSelectInput(session, 'sitePc', choices = siteNames)
    updateSelectInput(session, 'siteNest', choices = siteNames)
  })
  
  # Once HUB is chosen on the visit page, have this be the default entry:
  
  observe({
    inHub <- input$hub
    print(inHub)
    if(is.null(inHub))
      return(NULL)
    updateSelectInput(session, 'hubEnc', selected = input$hub)
    updateSelectInput(session, 'hubPc', selected = input$hub)
    updateSelectInput(session, 'hubNest', selected = input$hub)
  })
  
  # Once SITE is chosen on the visit page, have this be the default entry:
  
  observe({
    inSite <- input$site
    print(inSite)
    if(is.null(inSite))
      return(NULL)
    updateSelectInput(session, 'siteEnc', selected = input$site)
    updateSelectInput(session, 'sitePc', selected = input$site)
    updateSelectInput(session, 'siteNest', selected = input$site)
  })
  
  # Once DATE is chosen on the visit page, have this be the default entry:
  
  observe({
    inDate <- input$date
    print(inDate)
    if(is.null(inDate))
      return(NULL)
    updateSelectInput(session, 'dateEnc', selected = input$date)
    updateSelectInput(session, 'datePc', selected = input$date)
  })
  
  # Once observers are written on the visit page, have this be the default entry:
  
  observe({
    inObserver <- input$observer
    print(inObserver)
    if(is.null(inObserver))
      return(NULL)
    updateTextInput(session, 'bander', value = input$observer)
    updateTextInput(session, 'observerPc', value = input$observer)
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
    updateSelectInput(session, 'speciesEnc', choices = siteNames)
    updateSelectInput(session, 'speciesNest', choices = siteNames)
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
    hub <<- as.character(input$hub)
    dateOut <<- as.character(input$datev) 
    if (input$id != "0") {
      updateData(formData())
    } else {
      createData(formData())
      updateInputs(createDefaultRecord(), session)
    }
  }, priority = 1)
  
  # Click "Delete" to remove a single record:
  
  observeEvent(input$delete, {
    hub <<- as.character(input$hub)
    dateOut <<- as.character(input$datev) 
    deleteData(formData())
    updateInputs(createDefaultRecord(), session)
  }, priority = 1)
  
  # Press "New record" button to display empty record:
  
  observeEvent(input$newRecord, {
    hub <<- as.character(input$hub)
    dateOut <<- as.character(input$datev) # added mar 25
    updateInputs(createDefaultRecord(), session)
  })
  
  # Select row in table to show details in inputs:
  
  observeEvent(input$responses_rows_selected, {
    hub <<- as.character(input$hub)
    dateOut <<- as.character(input$datev) # added mar 25
    if (length(input$responses_rows_selected) > 0) {
      data <- readData()[input$responses_rows_selected, ]
      updateInputs(data, session)
    }
  })
  
  shinyjs::disable("id")
  
  # Inputs and submissions to temp data file:
  
  reactiveOut <- reactive({
    hub <<- as.character(input$hub)
    dateOut <<- as.character(input$datev) # added mar 25
    input$submitRecord
    input$delete
    readData()
  })
  
  # Display output table:
  
  output$responses <- DT::renderDataTable({
    hub <<- as.character(input$hub)
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
  
  dataEncountersSubset <- reactive({
    switch(input$hub,
           "Atlanta" = filter(encounters, hub == 'Atlanta'),
           "DC" = filter(encounters, hub == 'DC'),
           "Gainesville" = filter(encounters, hub == 'Gainesville'),
           "Pittsburgh" = filter(encounters, hub == 'Pittsburgh'),
           "Raleigh" = filter(encounters, hub == 'Raleigh'),
           "Springfield" = filter(encounters, hub == 'Springfield'))
  })
  
  output$encounterTable <- DT::renderDataTable({
    DT::datatable(dataEncountersSubset(), filter = 'bottom')
  })

  #-------------------------------------------------------------------------------*
  # ---- SERVER: QUERY AOU names ----
  #-------------------------------------------------------------------------------*
  output$aouTable = DT::renderDataTable(
    datatable(aouCodes, filter = 'none', rownames = FALSE,
              options = list(pageLength = 1)))
  
  #-------------------------------------------------------------------------------*
  # ---- SERVER: PC DATA CONDITIONS ----
  #-------------------------------------------------------------------------------*
  # Link fields to input:
  pcDataConditions <- reactive({
    dateOut <- as.character(input$datePc)
    data <- t(sapply(pcDataConditionsFields, function(x) input[[x]]))
    cbind(dateOut, data)
  })
  
  # When the Submit button is clicked, save form data:
  observeEvent(input$submitPcDataConditions, {
    savePcDataConditions(pcDataConditions())
    shinyjs::show("thankyou_msgPcDataConditions")
  })
  
  #-------------------------------------------------------------------------------*
  # ---- SERVER: SUBMIT BIRD-LEVEL POINT COUNT DATA ----
  #-------------------------------------------------------------------------------*
  
  # Link field names and data:
  
  formDataPc <- reactive({
    sapply(names(getTableMetadataPc()$fields), function(x) input[[x]])
  })
  
  # Click "Submit record" button to push result to table:
  
  observeEvent(input$submitRecordPc, {
    sitePc <<- as.character(input$sitePc)
    dateOutPc <<- as.character(input$datePc)
    observerPc <<- as.character(input$observerPc)
    startTimePc <<- as.character(input$startTimePc)
    notesPc <<- as.character(input$notesPc)
    if (input$idPc != "0") {
      updateDataPc(formDataPc())
    } else {
      createDataPc(formDataPc())
      updateInputsPc(createDefaultRecordPc(), session)
    }
  }, priority = 1)
  
  # Click "Delete" to remove a single record:
  
  observeEvent(input$deleteRecordPc, {
    sitePc <<- as.character(input$sitePc)
    dateOutPc <<- as.character(input$datePc) 
    observerPc <<- as.character(input$observerPc)
    startTimePc <<- as.character(input$startTimePc)
    notesPc <<- as.character(input$notesPc)
    deleteDataPc(formDataPc())
    updateInputsPc(createDefaultRecordPc(), session)
  }, priority = 1)
  
  # Press "New record" button to display empty record:
  
  observeEvent(input$newRecordPc, {
    sitePc <<- as.character(input$sitePc)
    dateOutPc <<- as.character(input$datePc)
    observerPc <<- as.character(input$observerPc)
    startTimePc <<- as.character(input$startTimePc)
    notesPc <<- as.character(input$notesPc)
    updateInputsPc(createDefaultRecordPc(), session)
  })
  
  # Select row in table to show details in inputs:
  
  observeEvent(input$responsesPc_rows_selected, {
    sitePc <<- as.character(input$sitePc)
    dateOutPc <<- as.character(input$datePc)
    observerPc <<- as.character(input$observerPc)
    startTimePc <<- as.character(input$startTimePc)
    notesPc <<- as.character(input$notesPc)
    if (length(input$responsesPc_rows_selected) > 0) {
      data <- readDataPc()[input$responsesPc_rows_selected, ]
      updateInputsPc(data, session)
    }
  })
  
  shinyjs::disable("idPc")
  
  # Inputs and submissions to temp data file:
  
  reactiveOutPc <- reactive({
    sitePc <<- as.character(input$sitePc)
    dateOutPc <<- as.character(input$datePc) 
    observerPc <<- as.character(input$observerPc)
    startTimePc <<- as.character(input$startTimePc)
    notesPc <<- as.character(input$notesPc)
    input$submitRecordPc
    input$deleteRecordPc
    readDataPc()
  })
  
  # Display output table:
  
  output$responsesPc <- DT::renderDataTable({
    sitePc <<- as.character(input$sitePc)
    dateOutPc <<- as.character(input$datePc) 
    observerPc <<- as.character(input$observerPc)
    startTimePc <<- as.character(input$startTimePc)
    notesPc <<- as.character(input$notesPc)
    reactiveOutPc()
  },
  server = FALSE, selection = "single",
  colnames = unname(getTableMetadataPc()$fields)[-1]
  )
  
  # Submit encounter data from table:
  
  observeEvent(input$submitPcData, {
    savePcDataCounts(reactiveOutPc())
    shinyjs::reset("pcData")
    shinyjs::show("thankyou_msgPc")
  })
  
  #-------------------------------------------------------------------------------*
  # ---- SERVER: NEST DATA ----
  #-------------------------------------------------------------------------------*
  
  # Link field names and data:
  
  formDataNest <- reactive({
    sapply(names(getTableMetadataNest()$fields), function(x) input[[x]])
  })
  
  # Click "Submit record" button to push result to table:
  
  observeEvent(input$submitRecordNest, {
    siteNest <<- as.character(input$siteNest)
    nestID <<- as.character(input$nestID)
    speciesNest <<- as.character(input$speciesNest)
    observerNest <<- as.character(input$observerNest)
    if (input$idNest != "0") {
      updateDataNest(formDataNest())
    } else {
      createDataNest(formDataNest())
      updateInputsNest(createDefaultRecordNest(), session)
    }
  }, priority = 1)
  
  # Click "Delete" to remove a single record:
  
  observeEvent(input$deleteRecordNest, {
    siteNest <<- as.character(input$siteNest)
    nestID <<- as.character(input$nestID)
    speciesNest <<- as.character(input$speciesNest)
    observerNest <<- as.character(input$observerNest)
    deleteDataNest(formDataNest())
    updateInputsNest(createDefaultRecordNest(), session)
  }, priority = 1)
  
  # Press "New record" button to display empty record:
  
  observeEvent(input$newRecordNest, {
    siteNest <<- as.character(input$siteNest)
    nestID <<- as.character(input$nestID)
    speciesNest <<- as.character(input$speciesNest)
    observerNest <<- as.character(input$observerNest)
    updateInputsNest(createDefaultRecordNest(), session)
  })
  
  # Select row in table to show details in inputs:
  
  observeEvent(input$responsesNest_rows_selected, {
    siteNest <<- as.character(input$siteNest)
    nestID <<- as.character(input$nestID)
    speciesNest <<- as.character(input$speciesNest)
    observerNest <<- as.character(input$observerNest)
    if (length(input$responsesNest_rows_selected) > 0) {
      data <- readDataNest()[input$responsesNest_rows_selected, ]
      updateInputsNest(data, session)
    }
  })
  
  shinyjs::disable("idNest")
  
  # Inputs and submissions to temp data file:
  
  reactiveOutNest <- reactive({
    siteNest <<- as.character(input$siteNest)
    nestID <<- as.character(input$nestID)
    speciesNest <<- as.character(input$speciesNest)
    observerNest <<- as.character(input$observerNest)
    input$submitRecordNest
    input$deleteRecordNest
    readDataNest()
  })
  
  # Display output table:
  
  output$responsesNest <- DT::renderDataTable({
    siteNest <<- as.character(input$siteNest)
    nestID <<- as.character(input$nestID)
    speciesNest <<- as.character(input$speciesNest)
    observerNest <<- as.character(input$observerNest)
    reactiveOutNest()
  },
  server = FALSE, selection = "single",
  colnames = unname(getTableMetadataNest()$fields)[-1]
  )
  
  # Submit encounter data from table:
  
  observeEvent(input$saveNestData, {
    saveNestData(reactiveOutNest())
    shinyjs::reset("nestData")
    shinyjs::show("thankyou_msgNest")
  })
  
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
  # END
}

shinyApp(ui, server)

#rsconnect::deployApp()