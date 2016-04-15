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

encounters <- read.csv('encounters2.csv', stringsAsFactors = FALSE) %>%
  tbl_df %>%
  mutate(date = as.Date(date),
         hub = ifelse(str_detect(site, 'MA1'), 'Springfield', hub)) %>%
  mutate(encounterType = ifelse(encounterType == 'band'|encounterType == 'recap', 
                  capitalize(encounterType),
                  encounterType))

# Load species by hub data:

hubSpecies <- read.csv('hubSpecies.csv', stringsAsFactors = FALSE) %>%
  tbl_df %>%
  bind_rows(
    data.frame(hub = c('DC', 'Atlanta', 'Gainesville',
                       'Pittsburgh', 'Raleigh', 'Springfield'),
               species = rep('', 6))) %>%
  arrange(species)

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
      column(4, imageOutput('nnLogo', height = 293)),
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
            mainPanel(
              fluidRow(
                column(8,''),
                column(4, imageOutput('birdBranch1', height = 55))
              ),
              br(), textVisit, width = 6, position = 'left')
            )
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
                     column(4, textInput('observerEnc', 'Observer initials:')),
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
                     column(6, textInput('rsLong', 'Resight longitude:')),
                     column(6, textInput('rsLat', 'Resight latitude:'))
                   ),
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
                        column(4, actionButton('submitEncData', 
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
           sidebarLayout(
             sidebarPanel(
               br(),
               fluidRow(
                 column(5,
                        selectInput('hubQuery','Hub:', 
                                    choiceRegions)),
                 column(7,
                        selectInput('siteQuery', 'Site:', ''))
                 ),
               hr(),
               fluidRow(
                 column(7, selectInput('speciesQuery', 'Species:', '')),
                 column(5, selectInput('sexQuery', 'Sex:', 
                                       choices = choiceSex, selected = ''))
               ),
               fluidRow(
                 column(6, textInput('bandNumberQuery', 'Band number:')),
                 column(6, selectizeInput('colorComboQuery', 
                                          'Color combo:',
                                          choices = c('', choiceColorCombos)))
               ),
               hr(),
               fluidRow(
                 selectizeInput('encounterTypeQuery', 'Encounter type:',
                                choices = choiceEncounterType, 
                                selected = 'Band')
               ),
               width = 3, position = 'left'),
             mainPanel(textQuery, width = 4, position = 'right')
           ),
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
                     column(4,
                            selectInput('hubPc','Regional Hub:',
                                        choices = choiceRegions,
                                        selected = NULL)),
                     column(4, selectInput('sitePc', 'Site', '')),
                     column(4, dateInput('datePc',
                                         label = 'Date: yyyy-mm-dd',
                                         value = Sys.Date()
                     ))
                   ), br(),
                   fluidRow(
                     column(4, textInput('observerPc', 'Observer:')),
                     column(4, selectizeInput('startTimePc', 'Start time:',
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
                            column(3, actionButton("newPc", "Clear fields",
                                                   class = 'btn-primary')),
                            column(2, ''),
                            column(3, actionButton('submitPc', 'Add record to table',
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
                    column(4, actionButton("deletePc",
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
           sidebarLayout(
             sidebarPanel(
               div(id = 'summaryDataNest',
                   h3(strong('1. Submit nest location and summary data:')),
                   strong(em('IMPORTANT! Be sure to enter nest data AFTER entering visit data!')),
                   br(), br(),
                   shinyjs::hidden(textInput('hubNest', 'hub')),
                   fluidRow(
                     column(3, strong('Species:')),
                     column(3, strong('Color combo:')),
                     column(3, strong('FWS Bands:')),
                     column(3, strong('Nest: Yr-#'))
                   ),
                   fluidRow(
                     column(3, selectInput('speciesNest', ' ', '')),
                     column(3, selectInput('colorComboMaleNest', 
                                           'Male:',
                                           choices = choiceColorCombos)),
                     column(3, textInput('bandNumberMaleNest', 'Male:')),
                     column(3, textInput('nestID', ' '))
                   ),
                   fluidRow(
                     column(3, ''),
                     column(3, selectInput('colorComboFemaleNest', 
                                           'Female:',
                                           choices = choiceColorCombos)),
                     column(3, textInput('bandNumberFemaleNest', 'Female:')),
                     column(3, '')
                   ),
                   hr(),
                   fluidRow(
                     column(9, ''),
                     column(3, tags$u(strong('Summary dates')))
                   ),
                   fluidRow(
                     column(2, textInput('plotNest', 'Plot:')),
                     column(3, selectInput('siteNest', 'Grid (site):', '')),
                     column(4, selectInput('fateNest', 'Nest Fate:',
                                           nestFateChoices)),
                     column(3, dateInput('dateClutchNest',
                                         label = 'Clutch completion:',
                                         value = ''))
                   ),
                   fluidRow(
                     column(7, textInput('plantSpNest', 
                                         'Plant species (if applicable):')),
                     column(2, selectInput('heightNest', 'Nest ht (m):',
                                           c('', 0:12), '')),
                     column(3, dateInput('dateHatchNest',
                                         label = 'Hatch:',
                                         value = ''))
                   ),
                   fluidRow(
                     column(9, textInput('descriptionNest', 'Nest description:')),
                     column(3, dateInput('dateFledgeFailNest',
                                         label = 'Fledge/fail (date):',
                                         value = ''))
                   ),
                   fluidRow(
                     column(3, selectInput('locationNest', 'Nest Location',
                                           nestLocationChoices)),
                     column(9, textInput('notesFateOtherNest', 
                                         'If nest fate was "other", explain:'))
                   )
               ),
               hr(),
               div(id = 'nestData',
                   h3(strong('2. Add nest observation records:')),
                   strong(em('IMPORTANT! Be sure to enter nest observation records data AFTER nest location and summary data!')),
                   br(), br(),
                   fluidRow(
                     column(3, dateInput('dateNest', label = 'Date:',
                                         value = '')),
                     column(3, selectizeInput('timeNest', 'Time:',
                                              choices = choiceTimeOfDay,
                                              selected = '')),
                     column(3, selectInput('stageNest', 'Stage:',
                                           choices = nestStageChoices,
                                           selected = '')),
                     column(3, selectInput('adAttNest', 'Adult attending:',
                                           choices = nestAttendChoices,
                                           selected = ''))),
                   fluidRow(
                     column(2, selectInput('nEggNest', '# egg:',
                                           choices = c('', 0:10))),
                     column(2, selectInput('nYoungNest', '# yng:',
                                           choices = c('', 0:10))),
                     column(6, textInput('notesNest', 
                                         'Notes: Building, age, fate, behavior, etc.')),
                     column(2, textInput('observerNest', 'Obs:'))),
                   fluidRow(column(1, ''),
                            column(3, actionButton("newNest", 
                                                   "Clear fields",
                                                   class = 'btn-primary')),
                            column(2, ''),
                            column(3, actionButton('submitNest', 
                                                   'Add record to table',
                                                   class = "btn-primary")))
                   ),  
               width = 6, position = 'left'),
             mainPanel(
               fluidRow(
                 column(8,''),
                 column(4, imageOutput('birdBranch2', height = 55))
               ),
               br(), textNest, width = 6, position = 'right')
           ),
           h3(strong('3. Data-proofing and submission of nest records:')),
           br(),
           DT::dataTableOutput("responsesNest"),
                      br(),
                      fluidRow(column(1, ''),
                               column(4, actionButton("deleteNest",
                                                      "Delete nest record", 
                                                      class = "btn-primary")),
                               column(3, ' '),
                               column(4, actionButton('saveNestData', 
                                                      'Submit nest data',
                                                      class = "btn-primary"))
                      ),
                      br(), shinyjs::hidden(
                        div(
                          id = "thankyou_msgNest",
                          h3("Thanks, your nest data have been recorded!")
                        )
                      ),
                      br()
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
      bind_rows(
        data.frame(hub = c('DC', 'Atlanta', 'Gainesville',
                           'Pittsburgh', 'Raleigh', 'Springfield'),
                   site = rep('', 6))) %>%
      filter(hub == inHub) %>%
      arrange(site) %>%
      .$site
    updateSelectInput(session, 'site', choices = siteNames)
    updateSelectInput(session, 'siteEnc', choices = siteNames)
    updateSelectInput(session, 'sitePc', choices = siteNames)
    updateSelectInput(session, 'siteNest', choices = siteNames)
  })
  
  # For the sites in the query, make this reactive on that page's hub:
  
  observe({
    inHub <- input$hubQuery
    print(inHub)
    if(is.null(inHub))
      return(NULL)
    siteNames <- encounters %>%
      select(hub, site) %>%
      distinct %>% 
      bind_rows(
        data.frame(hub = c('DC', 'Atlanta', 'Gainesville',
                           'Pittsburgh', 'Raleigh', 'Springfield'),
                   site = rep('', 6))) %>%
      filter(hub == inHub) %>%
      arrange(site) %>%
      .$site
    updateSelectInput(session, 'siteQuery', choices = siteNames)
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
    updateSelectInput(session, 'hubQuery', selected = input$hub)
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
    updateSelectInput(session, 'siteQuery', selected = input$site)
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
    updateTextInput(session, 'observerEnc', value = input$observer)
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
    updateSelectInput(session, 'speciesQuery', choices = siteNames)
  })
  
  # Create reactive site so that site can be included in the file name:
  
  siteName <- reactive({
    as.character(input$site)
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
    saveData(visitData(), 'visitData', siteName()) #saveVisitData(visitData())
    shinyjs::show("thankyou_msgVisit")
  })
  
  #----------------------------------------------------------------------*
  # ENCOUNTER DATA
  #----------------------------------------------------------------------*
  
  # Input fields:
  
  formDataEnc <- reactive({
    sapply(names(getTableMetadata(fieldCodesEnc, fieldNamesEnc)$fields),
           function(x) as.character(input[[x]]))
  })
  
  # Click submit to add table or modify/add records:
  
  observeEvent(input$submitEnc, {
    fixedValues <- c('hubEnc', 'siteEnc', 'dateEnc','observerEnc')
    for(i in 1:length(fixedValues)){
      globalAssign(input[[fixedValues[i]]], as.character(fixedValues[i])) 
    }
    # If the data table exists, modify table else create table:
    if(exists('responseDataEnc')){
      # If no rows are selected, add a row with the new record:
      if(length(input$responsesEnc_rows_selected) < 1){
        responseDataEnc[nrow(responseDataEnc) + 1,] <- castData(formDataEnc())
      }
      # If a row has been selected, modify the selected record:
      if(length(input$responsesEnc_rows_selected == 1)){
        responseDataEnc[input$responsesEnc_rows_selected,] <- castData(formDataEnc())
      }
      # If the table is currently blank, start table with new record:
    } else {
      responseDataEnc <- castData(formDataEnc())
    }
    globalAssign(responseDataEnc, 'responseDataEnc')
    
    # After submission, clear fields to defaults:
    updateInputs(createDefaultRecord(fieldCodesEnc), fieldCodesEnc, session)
  }, priority = 1)
  
  # Press New to display empty record:
  
  observeEvent(input$newEnc, {
    updateInputs(createDefaultRecord(fieldCodesEnc), fieldCodesEnc, session)
  })
  
  # Delete a selected row:
  
  observeEvent(input$deleteEnc, {
    if(length(input$responsesEnc_rows_selected) == 1){
      responseDataEnc <<- responseDataEnc[-input$responsesEnc_rows_selected,]
      updateInputs(createDefaultRecord(fieldCodesEnc), fieldCodesEnc, session)
    }
  }, priority = 1)
  
  # Select row in table to show details in inputs:
  
  observeEvent(input$responsesEnc_rows_selected, {
    if (length(input$responsesEnc_rows_selected) == 1) {
      data <- responseDataEnc[input$responsesEnc_rows_selected, ]
      updateInputs(data, fieldCodesEnc, session)
      responseDataEnc[input$responsesEnc_rows_selected, ] 
    }
  })
  
  # Display table:
  
  output$responsesEnc <- DT::renderDataTable({
    # Update after submit is clicked
    input$submitEnc
    # Update after delete is clicked
    input$deleteEnc
    if (existCheck(responseDataEnc)) responseDataEnc    
  }, server = FALSE, selection = "single",
  colnames = unname(getTableMetadata(fieldCodesEnc, fieldNamesEnc)$fields))
  
  observeEvent(input$submitEncData, {
    saveData(formDataEnc(), 'encounterData', siteName()) #saveVisitData(visitData())
    shinyjs::show("thankyou_msgEnc")
  })
  
  #-------------------------------------------------------------------------------*
  # ---- SERVER: QUERY BANDING RECORDS ----
  #-------------------------------------------------------------------------------*
  
  dataEncountersSubset <- reactive({
#     switch(input$hubQuery,
#            "Atlanta" = filter(encounters, hub == 'Atlanta'),
#            "DC" = filter(encounters, hub == 'DC'),
#            "Gainesville" = filter(encounters, hub == 'Gainesville'),
#            "Pittsburgh" = filter(encounters, hub == 'Pittsburgh'),
#            "Raleigh" = filter(encounters, hub == 'Raleigh'),
#            "Springfield" = filter(encounters, hub == 'Springfield'))
    filteringFun(input$hubQuery, input$siteQuery,
                 input$speciesQuery, input$sexQuery, input$bandComboQuery,
                 input$bandNumberQuery)
#     inHub <- input$hubQuery
#     inSite <- input$siteQuery
#     inSpecies <- input$speciesQuery
#     inSex <- 
#     inBandCombo <-
#     inBandNumber <- 
#     encounters %>%
#       filter(if(inHub != '') hub == inHub) %>%
#       # filter(if(input$inSite != '') str_detect(site, toupper(inSite))) # %>%
#       filter(if(inSpecies != '') str_detect(species, toupper(inSpecies))) %>%
#       filter(if(input$sexQuery != '') str_detect(sex, toupper(input$sexQuery))) # %>%
#       filter(if(input$colorComboQuery != '') str_detect(bandCombo, toupper(input$colorComboQuery))) %>%
#       filter(if(input$bandNumberQuery != '') str_detect(bandNumber, toupper(input$bandNumberQuery)))
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
  # ---- SERVER: SUBMIT BIRD-LEVEL POINT COUNT DATA ----
  #-------------------------------------------------------------------------------*
  # Input fields:
  
  formDataPc <- reactive({
    sapply(names(getTableMetadata(fieldCodesPc, fieldNamesPc)$fields),
           function(x) as.character(input[[x]]))
  })
  
  # Click submit to add table or modify/add records:
  
  observeEvent(input$submitPc, {
    fixedValues <- c('hubPc', 'sitePc', 'datePc','observerPc', 
                     'startTimePc', 'notesPc')
    for(i in 1:length(fixedValues)){
      globalAssign(input[[fixedValues[i]]], as.character(fixedValues[i])) 
    }
    # If the data table exists, modify table else create table:
    if(exists('responseDataPc')){
      # If no rows are selected, add a row with the new record:
      if(length(input$responsesPc_rows_selected) < 1){
        responseDataPc[nrow(responseDataPc) + 1,] <- castData(formDataPc())
      }
      # If a row has been selected, modify the selected record:
      if(length(input$responsesPc_rows_selected == 1)){
        responseDataPc[input$responsesPc_rows_selected,] <- castData(formDataPc())
      }
      # If the table is currently blank, start table with new record:
    } else {
      responseDataPc <- castData(formDataPc())
    }
    globalAssign(responseDataPc, 'responseDataPc')
    
    # After submission, clear fields to defaults:
    updateInputs(createDefaultRecord(fieldCodesPc), fieldCodesPc, session)
  }, priority = 1)
  
  # Press New to display empty record:
  
  observeEvent(input$newPc, {
    updateInputs(createDefaultRecord(fieldCodesPc), fieldCodesPc, session)
  })
  
  # Delete a selected row:
  
  observeEvent(input$deletePc, {
    if(length(input$responsesPc_rows_selected) == 1){
      responseDataPc <<- responseDataPc[-input$responsesPc_rows_selected,]
      updateInputs(createDefaultRecord(fieldCodesPc), fieldCodesPc, session)
    }
  }, priority = 1)
  
  # Select row in table to show details in inputs:
  
  observeEvent(input$responsesPc_rows_selected, {
    if (length(input$responsesPc_rows_selected) == 1) {
      data <- responseDataPc[input$responsesPc_rows_selected, ]
      updateInputs(data, fieldCodesPc, session)
      responseDataPc[input$responsesPc_rows_selected, ] 
    }
  })
  
  # Display table:
  
  output$responsesPc <- DT::renderDataTable({
    # Update after submit is clicked
    input$submitPc
    # Update after delete is clicked
    input$deletePc
    if (existCheck(responseDataPc)) responseDataPc    
  }, server = FALSE, selection = "single",
  colnames = unname(getTableMetadata(fieldCodesPc, fieldNamesPc)$fields))
  
  # Upload data to dropbox
  
  observeEvent(input$submitPcData, {
    saveData(formDataPc(), 'pointCountData', siteName()) #saveVisitData(visitData())
    shinyjs::show("thankyou_msgPc")
  })
  
  #-------------------------------------------------------------------------------*
  # ---- SERVER: NEST DATA ----
  #-------------------------------------------------------------------------------*
  
  # Input fields:
  
  formDataNest <- reactive({
    sapply(names(getTableMetadata(fieldCodesNest, fieldNamesNest)$fields),
           function(x) as.character(input[[x]]))
  })
  
  # Click submit to add table or modify/add records:
  
  observeEvent(input$submitNest, {
    fixedValues <- c('hubNest', 'siteNest', 'plotNest',
                     'speciesNest', 'nestID', 'dateNest')
    for(i in 1:length(fixedValues)){
      globalAssign(input[[fixedValues[i]]], as.character(fixedValues[i])) 
    }
    # If the data table exists, modify table else create table:
    if(exists('responseDataNest')){
      # If no rows are selected, add a row with the new record:
      if(length(input$responsesNest_rows_selected) < 1){
        responseDataNest[nrow(responseDataNest) + 1,] <- castData(formDataNest())
      }
      # If a row has been selected, modify the selected record:
      if(length(input$responsesNest_rows_selected == 1)){
        responseDataNest[input$responsesNest_rows_selected,] <- castData(formDataNest())
      }
      # If the table is currently blank, start table with new record:
    } else {
      responseDataNest <- castData(formDataNest())
    }
    globalAssign(responseDataNest, 'responseDataNest')
    
    # After submission, clear fields to defaults:
    updateInputs(createDefaultRecord(fieldCodesNest), fieldCodesNest, session)
  }, priority = 1)
  
  # Press New to display empty record:
  
  observeEvent(input$newNest, {
    updateInputs(createDefaultRecord(fieldCodesNest), fieldCodesNest, session)
  })
  
  # Delete a selected row:
  
  observeEvent(input$deleteNest, {
    if(length(input$responsesNest_rows_selected) == 1){
      responseDataNest <<- responseDataNest[-input$responsesNest_rows_selected,]
      updateInputs(createDefaultRecord(fieldCodesNest), fieldCodesNest, session)
    }
  }, priority = 1)
  
  # Select row in table to show details in inputs:
  
  observeEvent(input$responsesNest_rows_selected, {
    if (length(input$responsesNest_rows_selected) == 1) {
      data <- responseDataNest[input$responsesNest_rows_selected, ]
      updateInputs(data, fieldCodesNest, session)
      responseDataNest[input$responsesNest_rows_selected, ] 
    }
  })
  
  # Display table:
  
  output$responsesNest <- DT::renderDataTable({
    # Update after submit is clicked
    input$submitNest
    # Update after delete is clicked
    input$deleteNest
    if (existCheck(responseDataNest)) responseDataNest    
  }, server = FALSE, selection = "single",
  colnames = unname(getTableMetadata(fieldCodesNest, fieldNamesNest)$fields))
  
  # Submit encounter data from table:
  
  observeEvent(input$saveNestData, {
    saveData(formDataNest(), 'nestData', siteName())
    shinyjs::reset("nestData")
    shinyjs::show("thankyou_msgNest")
  })
  
  #-------------------------------------------------------------------------------*
  # ---- SERVER: IMAGES ----
  #-------------------------------------------------------------------------------*
  
  # Nestwatch logo:
  
  output$nnLogo <- renderImage({
    list(src = 'logo.png',
         width = 401,
         height = 293, 
         position = 'middle')
  }, deleteFile = FALSE)
  
  output$birdBranch1 <- renderImage({
    list(src = 'nnBirdBranchImage.png',
         width = 300,
         height = 116, 
         position = 'middle')
    }, deleteFile = FALSE)
  
  output$birdBranch2 <- renderImage({
    list(src = 'nnBirdBranchImage.png',
         width = 300,
         height = 116, 
         position = 'middle')
  }, deleteFile = FALSE)
  
  # END
}

shinyApp(ui, server)

#rsconnect::deployApp()