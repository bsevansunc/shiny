# Helper Functions from http://ipub.com/shiny-crud-app/:

# This method casts from the inputs to a one-row data.frame. We use it, for instance, when the user creates a new record by typing in values into the inputs, and then clicks "Submit":

# CastData <- function(data) {
#   datar <- data.frame(name = data["name"], 
#                       used_shiny = as.logical(data["used_shiny"]), 
#                       r_num_years = as.integer(data["r_num_years"]),
#                       stringsAsFactors = FALSE)
#   
#   rownames(datar) <- data["id"]
#   return (datar)
# }


CastData <- function(data) {
  datar <- data.frame(name = data["name"], 
                      used_shiny = as.logical(data["used_shiny"]), 
                      r_num_years = as.integer(data["r_num_years"]),
                      stringsAsFactors = FALSE)
  rownames(datar) <- data["id"]
  return (datar)
}


# This creates an empty record, to be used e.g. to fill the inputs with the default values when the user clicks the "New" button:

# CreateDefaultRecord <- function() {
#   mydefault <- CastData(list(id = "0", name = "", used_shiny = FALSE, r_num_years = 2))
#   return (mydefault)
# }

CreateDefaultRecord <- function() {
  mydefault <- CastData(list(id = "0", name = "", used_shiny = FALSE, r_num_years = 2))
  return (mydefault)
}

# And this method takes the data as selected in the DataTable, and updates the inputs with the respective values:

# 
# UpdateInputs <- function(data, session) {
#   updateTextInput(session, "id", value = unname(rownames(data)))
#   updateTextInput(session, "name", value = unname(data["name"]))
#   updateCheckboxInput(session, "used_shiny", value = as.logical(data["used_shiny"]))
#   updateSliderInput(session, "r_num_years", value = as.integer(data["r_num_years"]))
# }

UpdateInputs <- function(data, session) {
  updateTextInput(session, "id", value = unname(rownames(data)))
  updateTextInput(session, "name", value = unname(data["name"]))
  updateCheckboxInput(session, "used_shiny", value = as.logical(data["used_shiny"]))
  updateSliderInput(session, "r_num_years", value = as.integer(data["r_num_years"]))
}

# This function finds the next ID of a new record. In mysql, this could be done by an incremental index, automatically. But here, we do it manually, ourselves:
  
# GetNextId <- function() {
#   if (exists("responses")) {
#     max(as.integer(rownames(responses))) + 1
#   } else {
#     return (1)
#   }
# }

# This one is different in the if statement (but still doesn't work):

GetNextId <- function() {
  if (exists("responses") && nrow(responses) > 0) {
    max(as.integer(rownames(responses))) + 1
  } else {
    return (1)
  }
}
  
# Create data

# CreateData <- function(data) {
#   
#   data <- CastData(data)
#   rownames(data) <- GetNextId()
#   if (exists("responses")) {
#     responses <<- rbind(responses, data)
#   } else {
#     responses <<- data
#   }
# }

CreateData <- function(data) {
  data <- CastData(data)
  rownames(data) <- GetNextId()
  if (exists("responses")) {
    responses <<- rbind(responses, data)
  } else {
    responses <<- data
  }
}
  
# Read
  
# ReadData <- function() {
#   if (exists("responses")) {
#     responses
#   }
# }

ReadData <- function() {
  if (exists("responses")) {
    responses
  }
}
  
# Update
  
# UpdateData <- function(data) {
#   data <- CastData(data)
#   responses[row.names(responses) == row.names(data), ] <<- data
# }

UpdateData <- function(data) {
  data <- CastData(data)
  responses[row.names(responses) == row.names(data), ] <<- data
}
  
# Delete
# 
# DeleteData <- function(data) {
#   responses <<- responses[row.names(responses) != unname(data["id"]), ]
# }

DeleteData <- function(data) {
  responses <<- responses[row.names(responses) != unname(data["id"]), ]
}
  
# The only thing that might not be straight forward is the GetTableMetadata function. We'll use it as a starting point for further development, as described below. For now, it's just a method that defines the names of the columns in our table:
    
# GetTableMetadata <- function() {
#   fields <- c(id = "Id", 
#               name = "Name", 
#               used_shiny = "Used Shiny", 
#               r_num_years = "R Years")
#   
#   result <- list(fields = fields)
#   return (result)
# }

GetTableMetadata <- function() {
  fields <- c(id = "Id", 
              name = "Name", 
              used_shiny = "Used Shiny", 
              r_num_years = "R Years")
  result <- list(fields = fields)
  return (result)
}

#----------------------------------------------------------------------------------------------=-*

# UI

ui <- fluidPage(
  #use shiny js to disable the ID field
  shinyjs::useShinyjs(),
  #data table
  DT::dataTableOutput("responses", width = 300), 
  #input fields
  tags$hr(),
  shinyjs::disabled(textInput("id", "Id", "0")),
  textInput("name", "Name", ""),
  checkboxInput("used_shiny", "Used Shiny", FALSE),
  sliderInput("r_num_years", "R Years", 0, 25, 2, ticks = FALSE),
  #action buttons
  actionButton("submit", "Submit"),
  actionButton("new", "New"),
  actionButton("delete", "Delete")
)



# Server

server <- function(input, output, session) {
  
  # input fields are treated as a group
  formData <- reactive({
    sapply(names(GetTableMetadata()$fields), function(x) input[[x]])
  })
  
  # Click "Submit" button -> save data
  observeEvent(input$submit, {
    if (input$id != "0") {
      UpdateData(formData())
    } else {
      CreateData(formData())
      UpdateInputs(CreateDefaultRecord(), session)
    }
  }, priority = 1)
  
  # Press "New" button -> display empty record
  observeEvent(input$new, {
    UpdateInputs(CreateDefaultRecord(), session)
  })
  
  # Press "Delete" button -> delete from data
  observeEvent(input$delete, {
    DeleteData(formData())
    UpdateInputs(CreateDefaultRecord(), session)
  }, priority = 1)
  
  # Select row in table -> show details in inputs
  observeEvent(input$responses_rows_selected, {
    if (length(input$responses_rows_selected) > 0) {
      data <- ReadData()[input$responses_rows_selected, ]
      UpdateInputs(data, session)
    }
    
  })
  
  # display table
  output$responses <- DT::renderDataTable({
    #update after submit is clicked
    input$submit
    #update after delete is clicked
    input$delete
    ReadData()
  }, server = FALSE, selection = "single",
  colnames = unname(GetTableMetadata()$fields)[-1]
  )     
  
}

shinyApp(ui, server)
