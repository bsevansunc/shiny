# fieldsPc <- c('id', 'sitePc', 'observerPc', 'datePc', 'startTimePc',
#               'timePc', 'speciesPc', 'distancePc', 'countPc',
#               'detectionPc', 'notesPc')
# 
# fieldNamesPc <- c('Id', 'Site', 'Observer', 'Date', 'Start time',
#                   'Time interval', 'SPP', 'Distance', 'Count',
#                   'Detection', 'Notes')

# Field names for out table and adding records:

getTableMetadata <- function(fieldCodes, fieldNames) {
  fields <- fieldCodes
  names(fields) <- fieldNames
  result <- list(fields = fields)
  return (result)
}

# Data entering into table

castData <- function(fieldValues){
  data <- as.list(fieldValues)
  names(data) <- names(fieldValues)
  d <- data.frame(data, stringsAsFactors = FALSE)
  row.names(d) <- data['id']
  d <- d[,-1]
  return(d)
}

# Blank record (except for row number):

createDefaultRecord <- function(fieldCodes) {
  defaultFieldValues <- as.list(c(0, 
                        rep('', length(fieldCodes) -1)))
  names(defaultFieldValues) <- fieldCodes
  defaultRecord <- castData(defaultFieldValues)
  return (defaultRecord)
}

# updateInputs <- function(data, session) {
#   updateTextInput(session, "id", value = unname(rownames(data)))
#   updateTextInput(session, "name", value = unname(data["name"]))
#   updateTextInput(session, "usedS", value = unname(data["usedS"]))
#   updateTextInput(session, "rN", value = unname(data["rN"]))
# }

updateInputs <- function(data, fieldCodes, session) {
  updateTextInput(session, 'id', value = unname(rownames(data)))
  for(i in 2:length(fieldCodes)){
    updateTextInput(session, fieldCodes[i],
                    value = unname(data[fieldCodes[i]]))
  }
}

getNextId <- function() {
  if (exists('responses')) { #&& nrow('responses') > 0) {
    max(as.integer(rownames(responses))) + 1
  } else {
    return (1)
  }
}

# Create data:

createData <- function(data) {
  data <- castData(data)
  rownames(data) <- getNextId()
  if (exists("responses")) {
    responses <<- rbind(responses, data)
  } else {
    responses <<- data
  }
}

# Read responses

readData <- function() {
  if (exists('responses')) {
    responses
  }
}

# Update data with responses

updateData <- function(data) {
  data <- castData(data)
  data <<- responses[row.names(responses) == row.names(data), ]
}


# Delete

deleteData <- function(data) {
  responses <<- responses[row.names(responses) != unname(data["id"]), ]
}



#### UI -----

ui <- fluidPage(
  #use shiny js to disable the ID field
  shinyjs::useShinyjs(),
  
  #data table
  DT::dataTableOutput("responses", width = 300), 
  
  #input fields
  tags$hr(),
  shinyjs::disabled(textInput("id", "id", "0")),
  textInput("name", "Name", ""),
  textInput("usedS", "Used Shiny"),
  textInput("rN", "R Years"),
  
  #action buttons
  actionButton("submit", "Submit"),
  actionButton("new", "New"),
  actionButton("delete", "Delete")
)

# SERVER ----

server <- function(input, output, session) {
  fieldCodes <- c('id', 'name', 'usedS', 'rN')
  fieldNames <- c('id', 'Name', 'UsedS', 'RN')
  
  # Set input names:
  
  formData <- reactive({
    sapply(getTableMetadata(fieldCodes, fieldNames)$fields,
           function(x) input[[x]])
  })
  
  # Click "Submit" button to add data to table:
  
  observeEvent(input$submit, {
    if (input$id != "0") {
      updateData(formData())
    } else {
      createData(formData())
      updateInputs(createDefaultRecord(fieldCodes), fieldCodes, session)
    }
  }, priority = 1)
  
  # Press "New" button -> display empty record
  observeEvent(input$new, {
    updateInputs(createDefaultRecord(fieldCodes), fieldCodes, session)
  })
  
  # Press "Delete" button -> delete from data
  observeEvent(input$delete, {
    deleteData(formData())
    updateInputs(createDefaultRecord(fieldCodes), fieldCodes, session)
  }, priority = 1)
  
  # Select row in table -> show details in inputs
  observeEvent(input$responses_rows_selected, {
    if (length(input$responses_rows_selected) > 0) {
      data <- readData()[input$responses_rows_selected, ]
      updateInputs(data, fieldCodes, session)
    }
  })
  
  # display table
  output$responses <- DT::renderDataTable({
    #update after submit is clicked
    input$submit
    #update after delete is clicked
    input$delete
    readData()
  }, server = FALSE, selection = "single",
  colnames = unname(getTableMetadata(fieldCodes, fieldNames)$fields)
  )     
}

shinyApp(ui, server)