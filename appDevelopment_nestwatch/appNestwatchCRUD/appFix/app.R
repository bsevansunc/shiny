# Choices

encounters <- read.csv('encounters.csv', stringsAsFactors = F) %>%
  tbl_df %>%
  mutate(date = as.Date(date),
         site = toupper(site)) 

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

# Helper Functions modified from http://ipub.com/shiny-crud-app/:

# This method casts from the inputs to a one-row data.frame. We use it, for instance, when the user creates a new record by typing in values into the inputs, and then clicks "Submit":

castData <- function(data) {
  datar <- data.frame(site = as.character(data["site"]),
                      date = as.character(data["date"]),
                      bandTime = as.character(data["bandTime"]),
                      bander = data["bander"],
                      encounterType = data["encounterType"],
                      species = data["species"],
                      notes = as.character(data["notes"]),
                      stringsAsFactors = FALSE)
  
  rownames(datar) <- data["id"]
  return (datar)
}

# This creates an empty record, to be used e.g. to fill the inputs with the default values when the user clicks the "New" button:

createDefaultRecord <- function() {
  mydefault <- castData(list(id = "0", date = "",site = "", bandTime = '', 
                             bander = '', encounterType = '',
                             species = '', 
                             notes = ""))
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
              notes = "Notes")
  result <- list(fields = fields)
  return (result)
}

#-----------------------------------------------------------------------------------*
# ---- UI
#-----------------------------------------------------------------------------------*

ui <- fluidPage(
  #use shiny js to disable the ID field
  shinyjs::useShinyjs(),
  h3(strong('Enter encounter record:')),
  br(),
  fluidRow(
    column(4, selectizeInput('bandTime', 'Time:',
                             choices = choiceTimeOfDay)),
    column(4, textInput('bander', 'Observer initials:')),
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
  fluidRow(
    actionButton('newRecord', 'New record', class = 'btn-primary'),
    actionButton('submitRecord', 'Submit record', class = "btn-primary"),
    actionButton("delete", "Delete record", class = "btn-primary")),
    hr(),
    #data table
    DT::dataTableOutput("responses", width = 300),
    shinyjs::disabled(textInput("id", "Id", "0"))
)

# Server

server <- function(input, output, session) {
  
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
  })
  
  # Press "New" button -> display empty record
  observeEvent(input$newRecord, {
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
  output$responses <- DT::renderDataTable({
    #update after submit is clicked
    input$submitRecord
    #update after delete is clicked
    input$delete
    readData()
  }, server = FALSE, selection = "single",
  colnames = unname(getTableMetadata()$fields)[-1]
  )     
}

shinyApp(ui, server)
