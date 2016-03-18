# Helper Functions modified from http://ipub.com/shiny-crud-app/:

# This method casts from the inputs to a one-row data.frame. We use it, for instance, when the user creates a new record by typing in values into the inputs, and then clicks "Submit":

castData <- function(data) {
  datar <- data.frame(site = as.character(data["site"]),
                      date = as.character(data["date"]),
                      notes = as.character(data["notes"]),
                      stringsAsFactors = FALSE)
  
  rownames(datar) <- data["id"]
  return (datar)
}

# This creates an empty record, to be used e.g. to fill the inputs with the default values when the user clicks the "New" button:

createDefaultRecord <- function() {
  mydefault <- castData(list(id = "0", date = "",site = "", notes = ""))
  return (mydefault)
}

# And this method takes the data as selected in the DataTable, and updates the inputs with the respective values:

updateInputs <- function(data, session) {
  updateTextInput(session, "id", value = unname(rownames(data)))
  updateTextInput(session, "date", value = unname(data['date']))
  updateTextInput(session, "site", value = unname(data['site']))
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
  #input fields
#   tags$hr(),
#   textInput("name", "Name", ""),
#   checkboxInput("used_shiny", "Used Shiny", FALSE),
#   sliderInput("r_num_years", "R Years", 0, 25, 2, ticks = FALSE),
  fluidRow(
    column(1, textInput('id', 'Id', '0')),
    column(1, textInput('site', 'Site:', 'Billy')),
    column(2, selectizeInput('date', 'Date:', choices = c('', 'B', 'C'))),
    column(3, textInput('notes', 'Notes:'))),
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
