#---------------------------------------------------------------------------------*
# ---- SET-UP ----
#=================================================================================*

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

aouCodes <- read.csv('speciesAouCodes.csv')

encounters <- read.csv('encounters.csv', stringsAsFactors = F) %>%
  tbl_df %>%
  mutate(date = as.Date(date),
         site = toupper(site)) 

# Define fields for visit data:

visitFields <- c('hub', 'site', 'date', 'observer', 'startNetTime', 'endNetTime',
                 'netCount', 'netHours', 'startRsTime', 'endRsTime',
                 'rsPathDistace', 'amroUnbanded', 'bcchUnbanded', 'brthUnbanded',
                 'cachUnbanded', 'carwUnbanded', 'grcaUnbanded', 'howrUnbanded',
                 'nocaUnbanded', 'nomoUnbanded', 'sospUnbanded',
                 'tutiUnbanded', 'unchUnbanded', 'encounteredBird','notes')

#---------------------------------------------------------------------------------*
# ---- FUNCTIONS ----
#---------------------------------------------------------------------------------*

# Save visit data via Dropbox:

saveVisitData <- function(visitData) {
  data <- t(visitData)
  # Create a unique file name
  fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  # Write the data to a temporary file locally
  filePath <- file.path(tempdir(), fileName)
  write.csv(data, filePath, row.names = FALSE, quote = TRUE)
  # Upload the file to Dropbox
  drop_upload(filePath, dest = 'visitData')
}

# Save encounter data via Dropbox:

saveEncounterData <- function(encounterData) {
  data <- encounterData
  # Create a unique file name
  fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  # Write the data to a temporary file locally
  filePath <- file.path(tempdir(), fileName)
  write.csv(data, filePath, row.names = FALSE, quote = TRUE)
  # Upload the file to Dropbox
  drop_upload(filePath, dest = 'encounterData')
}

# Google spreadsheets:
# 
# saveVisitData <- function(visitData) {
#   # Grab the Google Sheet
#   sheet <- gs_title('nnVisitData')
#   # Add the data as a new row
#   gs_add_row(sheet, input = visitData)
# }

# Helper Functions modified from http://ipub.com/shiny-crud-app/:

# This method casts from the inputs to a one-row data.frame. We use it, for instance, when the user creates a new record by typing in values into the inputs, and then clicks "Submit":

castData <- function(data) {
  datar <- data.frame(site = as.character(data["site"]),
                      date = as.character(data["date"]),
                      bandTime = as.character(data["bandTime"]),
                      bander = data["bander"],
                      encounterType = data["encounterType"],
                      species = data["species"],
                      bandNumber = as.integer(data["bandNumber"]),
                      colorCombo  = data["colorCombo"],
                      age = data["age"],
                      sex = data["sex"],
                      breedingCond = data["breedingCond"],
                      fat = data["fat"],
                      mass = data["mass"],
                      wing = data["wing"],
                      tail = data["tail"],
                      tarsus = data["tarsus"],
                      featherID = data["featherID"],
                      notes = as.character(data["notes"]),
                      stringsAsFactors = FALSE)
  
  rownames(datar) <- data["id"]
  return (datar)
}

# This creates an empty record, to be used e.g. to fill the inputs with the default values when the user clicks the "New" button:

createDefaultRecord <- function() {
  mydefault <- castData(list(id = "0", 
                             site = '',
                             date = '',
                             bandTime = '',
                             bander = '',
                             encounterType = '',
                             species = '',
                             bandNumber = '',
                             colorCombo  = '',
                             age = '',
                             sex = '',
                             breedingCond = '',
                             fat = '',
                             mass = '',
                             wing = '',
                             tail = '',
                             tarsus = '',
                             featherID = '',
                             notes = ''))
  return (mydefault)
}

# And this method takes the data as selected in the DataTable, and updates the inputs with the respective values:

updateInputs <- function(data, session) {
  updateTextInput(session, "id", value = unname(rownames(data)))
  updateTextInput(session, "date", value = unname(data['date']))
  updateTextInput(session, "site", value = unname(data['site']))
  updateTextInput(session, "bandTime", value = unname(data["bandTime"]))
  updateTextInput(session, "bander", value = unname(data["bander"]))
  updateTextInput(session, "encounterType", value = unname(data["encounterType"]))
  updateTextInput(session, "species", value = unname(data["species"]))
  updateTextInput(session, "bandNumber", value = unname(data["bandNumber"]))
  updateTextInput(session, "colorCombo", value = unname(data["colorCombo"]))
  updateTextInput(session, "age", value = unname(data["age"]))
  updateTextInput(session, "sex", value = unname(data["sex"]))
  updateTextInput(session, "breedingCond", value = unname(data["breedingCond"]))
  updateTextInput(session, "fat", value = unname(data["fat"]))
  updateTextInput(session, "mass", value = unname(data["mass"]))
  updateTextInput(session, "wing", value = unname(data["wing"]))
  updateTextInput(session, "tail", value = unname(data["tail"]))
  updateTextInput(session, "tarsus", value = unname(data["tarsus"]))
  updateTextInput(session, "featherID", value = unname(data["featherID"]))
  updateTextInput(session, "notes", value = unname(data["notes"]))
}

# This function finds the next ID of a new record. In mysql, this could be done by an incremental index, automatically. But here, we do it manually, ourselves:

getNextId <- function() {
  if (exists("responses")) {
    max(as.integer(rownames(responses))) + 1
  } else {
    return (1)
  }
}

# Create data

createData <- function(data) {
  
  data <- castData(data)
  rownames(data) <- getNextId()
  if (exists("responses")) {
    responses <<- rbind(responses, data)
  } else {
    responses <<- data
  }
}

# Read

readData <- function() {
  if (exists("responses")) {
    responses
  }
}

# Update

updateData <- function(data) {
  data <- castData(data)
  responses[row.names(responses) == row.names(data), ] <<- data
}


# Delete

deleteData <- function(data) {
  responses <<- responses[row.names(responses) != unname(data["id"]), ]
}

# The only thing that might not be straight forward is the GetTableMetadata function. We'll use it as a starting point for further development, as described below. For now, it's just a method that defines the names of the columns in our table:

getTableMetadata <- function() {
  fields <- c(id = "Id",
              site = "Site",
              date = "Date",
              bandTime = 'Time',
              bander = 'Observer',
              encounterType = 'Encounter type',
              species = 'SPP',
              bandNumber = 'Band number',
              colorCombo  = 'Color combo',
              age = 'Age',
              sex = 'Sex',
              breedingCond = 'CP/BP',
              fat = 'Fat',
              mass = 'Mass',
              wing = 'Wing',
              tail = 'Tail',
              tarsus = 'Tarsus',
              featherID = 'Feather ID',
              notes = "Notes")
  result <- list(fields = fields)
  return (result)
}

#---------------------------------------------------------------------------------*
# ---- CHOICES ----
#---------------------------------------------------------------------------------*

# Visit choices

choiceRegions <- c('Atlanta' = 'Atlanta',
                   'DC' = 'DC',
             'Gainesville' = 'Gainesville',
             'Pittsburgh' = 'Pittsburgh',
             'Raleigh' = 'Raleigh',
             'Springfield' = 'Springfield')

choiceSites <- c('', encounters$site %>% unique %>% sort)

choiceDate <- c('', seq(
  as.Date(ISOdate(2000, 1, 15)),
  as.Date(ISOdate(2030, 1, 1)), 1) %>%
    as.character)
  
timeOfDay <- format(seq(ISOdate(2000, 1, 1), ISOdate(2000,1,2), 
                        by = 'min'), '%H:%M') %>% 
  unique %>% sort %>% as.character

choiceTimeOfDay <- c('',timeOfDay)

choiceSpecies <- c('', 'AMRO', 'BCCH', 'BRTH', 'CACH', 'CARW', 'GRCA', 
                   'HOWR','NOCA','NOMO','SOSP','TUTI','UNCH')

colorValues = c('', 'AL', 'BU', 'BK', 'G', 'O','PK', 'PU','R', 'Y', 'W')

choiceColorCombos <- expand.grid(rep(list(colorValues), 4)) %>%
  tbl_df %>%
  transmute(L = paste(Var1, Var2, sep = '/'),
         R = paste(Var3, Var4, sep = '/'), 
         combo = paste(L, R, sep = ',')) %>%
  select(-c(L, R)) %>%
  filter(str_count(combo, 'AL') == 1) %>%
  mutate(combo = combo %>%
           str_replace_all('/,', ',') %>%
           str_replace_all(',/', ',') %>%
           str_replace_all(',$',',-') %>%
           str_replace_all('^,', '-,') %>%
           str_replace_all('^/', '') %>%
           str_replace_all('/$', '') %>%
           str_replace_all(',', ',')) %>%
  distinct %>% .$combo

choiceColorCombos <- c('', choiceColorCombos)

choiceNetCount <- c('', seq(0, 12, by = 0.5))

choiceNetHours <- c('', seq(0, 24, by = 0.01))

choiceCount <- c('', 1:100)

# Band choices:

choiceAge <- c('', 'HY', 'AHY', 'SY', 'ASY', 'UNK')

choiceEncounterType <- c('Band', 'Recap', 'Resight-incidental','Resight-targeted')

choiceSex <- c('', 'M', 'F', 'UNK')

choiceBreedingCond <-  c('','CP', 'BP','CP-', 'BP-','CP+', 'BP+')

choiceFat <- c('', 0, 0.5, seq(1:5))

#---------------------------------------------------------------------------------*
# ---- TOOLTIPS ----
#---------------------------------------------------------------------------------*

# ---- Visit tooltips: ------------------------------------------------------------

ttRegion <- p(strong('Regional Hub:'), ' Select your study region from the list.')

ttSite <- p(strong('Site:'), ' Begin typing the site code and a list of options will appear. Either select the site from the list or press enter once you have finished entering the site data (i.e., only one choice in the dropdown menu). The site is the first four letters of the last name of the participant, the first three letters of their first name, the two-letter state abbreviation, and a number that signifies whether a participant has moved. For example, if a participant was named Robert Reitsma, they lived in Maryland, and this was the original Nestwatch site associated with the participant, the site would be REITROBMD1. If the participant moved to a new location, the site code for the new location would be REITROBMD2.')

ttVisitDate <- p(strong('Date:'), ' Select the date of your visit from the calendar. All dates are to be provided in the international date standard format ISO 8601 (YYYY-MM-DD).', em('Note: that if you visited a site on multiple dates, you would enter visit data for each visit separately.'))

ttVisitObserver <- p(strong('Observer initials:'), ' Please enter the two or three letter initials of EACH technician who visit a site, separating entries by a comma. For example, if Robert Reitsma and Brian S. Evans visited a site, they would enter RR, BSE in this field. Note: To avoid autocorrect woes, you may want to teach your initials to your browser.')

ttNetCount <- p(strong('Number of nets:'), ' This is the total number of nets you put out, regardless of whether a net was moved or a net was put up later in the day.Begin typing the number of net and a list of options will appear. You can either select the number of nets from the list or press enter once you have finished typing the value. Number of nets is provided per 12 m length (e.g., 12 m = 1, 6 m = 0.5 nets).')

ttNetHours <- p(strong('Net hours:'), 'You can either select net hours from the list or press enter once you have finished typing the value. To calculate net hours, make sure to keep track of how long each net was open during your visit. Multiply the length of time each net was open by the net length (see net count), sum the nets, and record the number here.')

ttStartEndResightTime <- p(strong('Started/Ended resight:'), 'These are the times in which you began and ended your targeted resight effort. All times are to be entered in the 24-hour clock (ISO 8601 or military time format), for example 1 pm would be provided as 13:00.', em('Note: targeted resighting does not include time spent at a site doing other activities (e.g., nest searching or banding).'))

ttPathDistance <- p(strong('Path distance traveled:'), ' If you had a GPS unit with you, this is the path distance that you recorded on your GPS unit, in meters.')

ttObservedUnbanded <- p(strong('Observed, unbanded:'), ' These are the number of unbanded individuals observed during ', em('targeted'), 'resight efforts. Only include counts representing species that could potentially be resighting during your visit. If you encountered all individuals for a given species prior to targeted resighting, do not include counts for this species.')

ttEncounteredBirds <- p(strong('Did you band or encounter banded birds during your visit?'), ' It sometimes happens that you are not successful in your efforts to band or re-encounter a bird during a visit. If this is the case, select', em('no'), '. After you press ', em('Submit visit data'), 'your visit will be recorded and you do not have to proceed with further data entry.')

ttVisitNotes <- p(strong('Visit notes:'), ' Have anything else to say about your visit (not about individual birds)? Put it here!')

# ---- Encounter tooltips: --------------------------------------------------------

ttBandTime <- p(strong('Time:'), ' This is the time (24-hour format) in which you began processing the bird.')

ttBanderInitials <- p(strong('Observer initials:'), ' Enter the initials of the technician or techicians (comma-separated) who measured or resighted the bird.')

ttEncounterType <- p(strong('Encounter type:'), ' Select the type of encounter from the list. If you resighted a bird during your targeted resight efforts, select ', em('resight-targeted.'), ' If you resighted a bird outside of targeted resight efforts, select ', em('resight-incidental.'))

ttSpecies <- p(strong('Species:'), ' Drop-down menu choices only include Nestwatch focal species. Store all other encounter data locally.')

ttBandNumber <- p(strong('Band number:'), ' Please include only the numeric band number in this field (no dashes).')

ttColorCombo <- p(strong('Color combo:'), ' Enter color combinations as L/L,R/R. Do not include any spaces. Color abbreviations must match those used on the website for Nestwatch participants.')

ttAgeThroughFat <- p(strong('Age-Fat:'), ' Please select values for age, sex, breeding condition, and fat from the provided lists, entering', em('UNK'), 'if you were unable to identify the field value.')

ttMassThroughTarsus <- p(strong('Mass-Tarsus:'), ' Enter the measurement data in each field (please error-check prior to submission!).')

ttFeatherID <- p(strong('Feather ID:'), ' Please provide the ID of the feather sample.')

ttBandNotes <- 'Any other observations to report on this bird? Provide it here!'

#---------------------------------------------------------------------------------*
# ---- PAGE TEXT ----
#---------------------------------------------------------------------------------*

textVisit <- p(strong('Start by entering your visit data.'), 'These data only need to be entered once for each site visit. After entering data into a field, press enter and tab to the next field. When you finish entering the data for each of the fields, make sure to double-check all of your entries for accuracy then press the', em('Submit visit data'),  'button.', strong('Do not enter banding, resight, point count, nest, or habitat survey data prior to completing and submitting this form!'))

textBanding <- p(strong('AFTER entering visit data you are ready to enter your encounter records!'), ' This page is divided into two sections: ', strong('1) Encounter record:'), ' Enter one record for each individual. If you do not have data for a given field, leave that field blank. After entering all available data press the ', em('Add record'),'button. ', strong('2) Encounter query:'), ' If you are unsure of the band number of a resighted bird use this query form to find the identity of the resighted bird.', strong('3) Quality control and data submission:'), ' The table at the bottom of this page will be updated with the encounter records provided. After entering all of the records from your visit, compare table values with your paper record. If you find mistakes, delete the record from the table and submit a new record. Once you are confident with the quality of the data provided, press the ', em('Submit encounter data'), 'button.')

textQuery <- p('Query the table below to search for the band number associated with a given resight. While only 5 rows of data are shown, the table includes the initial record (band encounter) across all Neighborhood Nestwatch banding records and regional hubs. You can adjust the number of rows viewed using the "Show __ entries" drop-down button. You can sort the data table by column by clicking the column header. For example, to sort the data table by date, you would click on the "Date" column header. Query fields are at the bottom of each column. Use these fields to subset the data table. Data may be subset using partial matching. For example, typing "bu" in the Color combo field will match blue color bands on either leg or position. Likewise, typing "bu/bu" will query all records in which either leg has a blue over blue color band combo. You can use the search tool on the upper-right to query all fields simultaneously. Query fields are not case-sensitive.')

#---------------------------------------------------------------------------------*
# ---- USER INTERFACE ----
#=================================================================================*

ui <- navbarPage(
  strong("Neighborhood Nestwatch technician data submission interface"),
  #-------------------------------------------------------------------------------*
  # ---- UI TAB PANEL: VISIT ----
  #-------------------------------------------------------------------------------*
  tabPanel(strong('Visit data'),
           sidebarLayout(
              sidebarPanel(
                br(),
                # ---- Site and observer -----------------------------------------
                h4(strong('Visit')),
                fluidRow(
                  column(4,
                    selectInput('hub','Regional Hub:', choiceRegions)),
                    column(8,
                    selectizeInput('site', 'Site:', #'e2', '2. Multi-select', 
                                   choices = choiceSites,
                                   selected = NULL))),
                fluidRow(
                  column(6,
                         selectizeInput('date', label = 'Date:', 
                                   choices = choiceDate,
                                   selected = as.character(Sys.Date()))),
                  column(6,
                         textInput("observer", "Observer initials:"))),
                br(),
                # ---- Banding effort --------------------------------------------
                h4(strong('Banding effort:')),
                fluidRow(
                  column(6, selectizeInput('startNetTime', 'Nets opened:', 
                                           choices = choiceTimeOfDay)),
                  column(6, selectizeInput('endNetTime', 'Nets closed:', 
                                           choices = choiceTimeOfDay))),
                fluidRow(
                  column(6, selectizeInput('netCount', 'Number of nets:', 
                                           choices = choiceNetCount)),
                  column(6, selectizeInput('netHours', 'Net hours:', 
                                           choices = choiceNetHours))),
                br(),
                # ---- Resight effort --------------------------------------------
                h4(strong('Resight effort:')),
                fluidRow(
                  column(4, selectizeInput('startRsTime', 'Started resight:', 
                                           choices = choiceTimeOfDay)),
                  column(4, selectizeInput('endRsTime', 'Ended resight:', 
                                           choices = choiceTimeOfDay)),
                  column(4, textInput('rsPathDistance',
                                      'Path distance traveled:'))),
                h5(strong('Observed, unbanded:')),
                fluidRow(column(1, 'AMRO'),
                         column(2, selectizeInput(
                           'amroUnbanded', label = NULL,
                           choiceCount)),
                         column(1, 'CACH'),
                         column(2, selectizeInput(
                           'cachUnbanded', label = NULL,
                           choiceCount)),
                         column(1, 'HOWR'),
                         column(2, selectizeInput(
                           'howrUnbanded', label = NULL,
                           choiceCount)),
                         column(1, 'SOSP'),
                         column(2, selectizeInput(
                           'sospUnbanded', label = NULL,
                           choiceCount))),
                fluidRow(column(1, 'BCCH'),
                         column(2, selectizeInput(
                           'bcchUnbanded', label = NULL,
                           choiceCount)),
                         column(1, 'CARW'),
                         column(2, selectizeInput(
                           'carwUnbanded', label = NULL,
                           choiceCount)),
                         column(1, 'NOCA'),
                         column(2, selectizeInput(
                           'nocaUnbanded', label = NULL,
                           choiceCount)),
                         column(1, 'TUTI'),
                         column(2, selectizeInput(
                           'tutiUnbanded', label = NULL,
                           choiceCount))),
                fluidRow(column(1, 'BRTH'),
                         column(2, selectizeInput(
                           'brthUnbanded', label = NULL,
                           choiceCount)),
                         column(1, 'GRCA'),
                         column(2, selectizeInput(
                           'grcaUnbanded', label = NULL,
                           choiceCount)),
                         column(1, 'NOMO'),
                         column(2, selectizeInput(
                           'nomoUnbanded', label = NULL,
                           choiceCount)),
                         column(1, 'UNCH'),
                         column(2, selectizeInput(
                           'unchUnbanded', label = NULL,
                           choiceCount))),
                br(),
                h4(strong('Did you band or encounter a banded bird during your visit?')),
                radioButtons('encounteredBirds', label = NULL,
                             choices = list('Yes' = 1, 'No' = 2), 
                             selected = 1),
                br(),
                h4(strong('Visit notes:')),
                br(),
                # ---- Notes -----------------------------------------------------
                fluidRow(column(12, textInput('visitNotes', 
                                              label = NULL))),
                br(),
                actionButton("submitVisitData", "Submit visit data", 
                             class = "btn-primary"),
                width = 6, position = 'right'),
            # ---- Visit text ----------------------------------------------------
            mainPanel(
              textVisit, hr(),
              ttRegion, ttSite, ttVisitDate, ttVisitObserver, ttNetCount,
              ttNetHours, ttVisitNotes, ttStartEndResightTime, ttPathDistance,
              ttObservedUnbanded, ttEncounteredBirds, ttVisitNotes, 
              width = 6, position = 'left'))),
  #-------------------------------------------------------------------------------*
  # ---- UI TAB PANEL: ENCOUNTERS ----
  #-------------------------------------------------------------------------------*
  tabPanel(strong('Encounter data'),
           sidebarLayout(
             # ---- Record entry -------------------------------------------------
             sidebarPanel(
               shinyjs::useShinyjs(),
               h3(strong('Enter encounter record:')),
               br(),
               fluidRow(
                 column(2, shinyjs::disabled(textInput("id", "Id", "0"))),
                 column(3, selectizeInput('bandTime', 'Time:',
                                          choices = choiceTimeOfDay)),
                 column(3, textInput('bander', 'Observer initials:')),
                 column(4, selectizeInput('encounterType', 
                                          'Encounter type:',
                                          choices = choiceEncounterType, 
                                          selected = 'Band'))),
               fluidRow(
                 column(4, selectizeInput('species', label = 'Species:',
                                          choices = choiceSpecies)),
                 column(4, textInput('bandNumber', 'Band number:')),
                 column(4, selectizeInput('colorCombo', 
                                          'Color combination:',
                                          choices = choiceColorCombos))),
               br(),
               fluidRow(
                 column(3, selectizeInput('age','Age:',
                                          choices = choiceAge)),
                 column(3, selectizeInput('sex', label = 'Sex:',
                                          choices = choiceSex)),
                 column(3, selectizeInput('breedingCond', label = 'CP/BP:',
                                          choices = choiceBreedingCond)),
                 column(3, selectizeInput('fat',label = 'Fat:',
                                          choices = choiceFat))),
               fluidRow(
                 column(3, textInput('mass',label = 'Mass (g):')),
                 column(3, textInput('wing',label = 'Wing (mm):')),
                 column(3, textInput('tail',label = 'Tail (mm):')),
                 column(3, textInput('tarsus',label = 'Tarsus (mm):'))),
               fluidRow(
                 column(2, textInput('featherID', label = 'Feather ID:')),
                 column(10, textInput('notes', label = 'Notes:'))),
               br(),
               actionButton('submitRecord', 'Add record to table',
                                      class = "btn-primary"),
                          width = 6, position = 'right'),
             # ---- Encounter text ----------------------------------------------------
             mainPanel(
               textBanding, 
               hr(),
               ttBandTime, ttBanderInitials, ttEncounterType,
               ttSpecies, ttBandNumber, ttColorCombo, ttAgeThroughFat, 
               ttMassThroughTarsus,
               ttFeatherID,
               width = 6, position = 'left')
             ),
           hr(),
           # ---- QC and submission ---------------------------------------------------
           h3(strong('Quality Control and submission of encounter records:')),
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
           br(), br()
           ),
  #--------------------------------------------------------------------------------*
  # ---- UI TAB PANEL: QUERY RECORDS ----
  #--------------------------------------------------------------------------------*
  tabPanel(strong('Query records'),
           textQuery,
           fluidRow(column(11, DT::dataTableOutput('encounterTable')))
  ),
  #-------------------------------------------------------------------------------*
  # ---- UI TAB PANEL: POINT COUNT ----
  #-------------------------------------------------------------------------------*
  tabPanel(strong('Point count data')),
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
  #-------------------------------------------------------------------------------*
  # ---- SERVER: VISIT DATA ----
  #-------------------------------------------------------------------------------*
  # Link fields to input:
  visitData <- reactive({
    data <- sapply(visitFields, function(x) input[[x]])
    data
  })
  
  # When the Submit button is clicked, save form data:
  observeEvent(input$submitVisitData, {
    saveVisitData(visitData())
  })
  
  #-------------------------------------------------------------------------------*
  # ---- SERVER: ENCOUNTER DATA ----
  #-------------------------------------------------------------------------------*
  
  # input fields are treated as a group
  formData <- reactive({
    sapply(names(getTableMetadata()$fields), function(x) input[[x]])
  })
  
  # Click "Submit" button -> save data
  observeEvent(input$submitRecord, {
    if (input$id != "0") {
      updateData(formData())
    } else {
      createData(formData())
      updateInputs(createDefaultRecord(), session)
    }
    updateInputs(createDefaultRecord(), session)
  })
  
  # Press "Delete" button -> delete from data
  observeEvent(input$delete, {
    deleteData(formData())
    updateInputs(createDefaultRecord(), session)
  })
  
  # Select row in table -> show details in inputs
  observeEvent(input$responses_rows_selected, {
    if (length(input$responses_rows_selected) > 0) {
      data <- readData()[input$responses_rows_selected, ]
      updateInputs(data, session)
    }
  })
  
  shinyjs::disable("id")
  
  # display table
#   output$responses <- DT::renderDataTable({
#     #update after submit is clicked
#     input$submitRecord
#     #update after delete is clicked
#     input$delete
#     readData()
#   }, server = FALSE, selection = "single",
#   colnames = unname(getTableMetadata()$fields)[-1]
#   )
  
  reactiveOut <- reactive({
    input$submitRecord
    input$delete
    readData()
  })
  
  output$responses <- DT::renderDataTable({
    reactiveOut()
    },
    server = FALSE, selection = "single",
    colnames = unname(getTableMetadata()$fields)[-1]
    )
  
  # Submit encounter data from table
  observeEvent(input$submitEncounterData, {
    saveEncounterData(reactiveOut())
  })
  
  #-------------------------------------------------------------------------------*
  # ---- SERVER: QUERY BANDING RECORDS ----
  #-------------------------------------------------------------------------------*
  output$encounterTable = DT::renderDataTable(
    datatable(encounters, filter = 'bottom'))

  #-------------------------------------------------------------------------------*
  # ---- SERVER: BANDING DATA SUBMISSION ----
  #-------------------------------------------------------------------------------*
#   # input fields are treated as a group
#   formData <- reactive({
#     sapply(names(getTableMetadata()$fields), function(x) input[[x]])
#   })
#   
#   # Click "Submit" button -> save data
#   
#   observeEvent(input$submitRecord, {
#     if (input$id != "0") {
#       updateData(formData())
#     } else {
#       createData(formData())
#       updateInputs(createDefaultRecord(), session)
#     }
#   }, priority = 1)
#   
#   # Press "Delete" button -> delete from data
#   observeEvent(input$delete, {
#     DeleteData(formData())
#     updateInputs(createDefaultRecord(), session)
#   })
#   
#   # Select row in table -> show details in inputs
#   observeEvent(input$responses_rows_selected, {
#     if (length(input$responses_rows_selected) > 0) {
#       data <- readData()[input$responses_rows_selected, ]
#       updateInputs(data, session)
#     }
#   })
#   
#   shinyjs::disable("id")
#   
#   # display table
#   output$responses <- DT::renderDataTable({
#     #update after submit is clicked
#     input$submit
#     #update after delete is clicked
#     input$delete
#     readData()
#   }, server = FALSE, selection = "single",
#   colnames = unname(getTableMetadata()$fields)[-1]
#   )    
  
#       values <- reactiveValues()
#       # Blank data frame:
#       values$df <- data.frame(Site = numeric(0), Date = numeric(0),
#                               Time = numeric(0), Observer = numeric(0), 
#                               Encounter = numeric(0),SPP = numeric(0), 
#                               BandNumber = numeric(0), ColorCombo = numeric(0),
#                               Age = numeric(0), Sex = numeric(0), CP_BP = numeric(0),
#                               Fat = numeric(0), Mass = numeric(0), Wing = numeric(0),
#                               Tail = numeric(0), Tarsus = numeric(0), 
#                               FeatherID = numeric(0), Notes = numeric(0))
#       # Add entries:
#       newEntry <- observe({
#         if(input$update > 0) {
#           isolate(values$df[nrow(values$df) + 1,] <- 
#                     c(input$site, as.character(input$date),
#                                input$bandTime, input$bander, input$encounterType,
#                                input$species, input$bandNumber, input$colorCombo,
#                                input$age, input$sex, input$breedingCond, input$fat,
#                                input$mass, input$wing, input$tail, input$tarsus,
#                                input$featherID, input$notes
#                                ))
#         }
#       })
#       # Remove entries:
#       deleteEntry <- observe({
#         cat("deleteEntry\n")
#         if(input$deleteButton > 0) {
#           if(is.na(isolate(input$rowSelection))){
#             values$df <- isolate(values$df[-nrow(values$df), ])
#           } else {
#             values$df <- isolate(values$df[-input$rowSelection, ])
#           }
#         }
#       })
#       # Make table:
#       output$bandTable <- renderTable({values$df})
#       observe({
#         session$sendCustomMessage(type = 'testmessage',
#                                   message = list(a = 1, b = 'text',
#                                                  controller = input$controller))
  # })
  #-------------------------------------------------------------------------------*
  # ---- SERVER: DATA DOWNLOAD ----
  #-------------------------------------------------------------------------------*
  
  
#       output$submitBandingData <- downloadHandler(
#         # This function returns a string which tells the client
#         # browser what name to use when saving the file.
#         filename = function() {
#           siteDate <- paste('encounterData', input$site, input$date, sep = '_')
#           paste(siteDate, 'csv', sep = '.')
#         },
#         
#         # This function should write data to a file given to it by
#         # the argument 'file'.
#         content = function(file) {
#           # Write to a file specified by the 'file' argument
#           write.csv(renderTable(), file, sep = sep,
#                       row.names = FALSE)
#         }
#       )
  }

shinyApp(ui, server)