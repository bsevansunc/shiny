library(dplyr)
library(shiny)
# 
# # Existence check
# 
# existCheck <- function(dataObjectToCheck){
#   deparse(substitute(dataObjectToCheck)) %>% exists
# }
# 
# # Assign an object to the global environment (more flexible than <<- as it allows the user to specify the name of the object inside the function)
# 
# globalAssign <- function(data, dataName){
#   assign(dataName, data, envir = .GlobalEnv )
# }
# 
# # Function assigns fancy column headers to the field codes:
# 
# getTableMetadata <- function(fieldCodes, fieldNames) {
#   fields <- fieldNames
#   names(fields) <- fieldCodes
#   result <- list(fields = fields)
#   return (result)
# }
# 
# # Function takes values of inputs, if there are any and puts them in a 1-row data frame:
# 
# castData <- function(fieldValues){
#   data <- as.list(fieldValues)
#   return(data.frame(data, stringsAsFactors = FALSE))
# }
# 
# # Cast a blank record:
# 
# createDefaultRecord <- function(fieldCodes){
#   # Empty list for field code storage:
#   defaultFieldValues <- vector('list', length = length(fieldCodes))
#   for(i in 1:length(fieldCodes)){
#     # If the field is already defined, provide definition:
#     if(exists(fieldCodes[i])){
#       defaultFieldValues[[i]] <- get(fieldCodes[i])
#       # If the field is undefined, set default as blank:
#     } else {
#       defaultFieldValues[[i]] <- ''
#     }
#   }
#   # Provide object names and cast data:
#   names(defaultFieldValues) <- fieldCodes
#   castData(defaultFieldValues)
# }
# 
# # Update the field inputs on the ui:
# 
# updateInputs <- function(data, fieldCodes, session) {
#   for(i in 1:length(fieldCodes)){
#     updateTextInput(session, fieldCodes[i],
#                     value = unname(data[fieldCodes[i]]))
#   }
# }

source('helperFunctions.R')

# UI ----

ui <- fluidPage(
  #use shiny js to disable the ID field
  shinyjs::useShinyjs(),
  #data table
  DT::dataTableOutput("responses1", width = 300), 
  hr(),
  #input fields
  textInput("name", "Name", ""),
  selectInput("used_shiny", "Used Shiny", choices = c('a', 'b')),
  textInput("r_num_years", "R Years", ''),
  #action buttons
  actionButton("submit1", "Submit"),
  actionButton("new1", "New"),
  actionButton("delete1", "Delete"),
  ###
  hr(),
  #input fields
  shinyjs::hidden(textInput("id2", "Id2", "0")),
  selectInput("hello", "Hello", choices = c('yes', 'no')),
  selectInput("world", "World", choices = c('yes', 'no')),
  #action buttons
  actionButton("submit2", "Submit"),
  actionButton("new2", "New"),
  actionButton("delete2", "Delete"),
  DT::dataTableOutput("responses2", width = 300)
)

# SERVER ----

server <- function(input, output, session) {
  #----------------------------------------------------------------------*
  # TABLE ONE
  #----------------------------------------------------------------------*
  # Set field codes and names:
  
  fieldCodes1 <- c('name', 'used_shiny', 'r_num_years')
  fieldNames1 <- c('Name', 'Used Shiny', 'R Years')
  
  # Input fields:
  
  formData1 <- reactive({
    sapply(names(getTableMetadata(fieldCodes1, fieldNames1)$fields),
           function(x) input[[x]])
  })
  
  # Click submit to add table or modify/add records:
  
  observeEvent(input$submit1, {
    fixedValues <- c('name')
    for(i in 1:length(fixedValues)){
      globalAssign(input[[fixedValues[i]]], fixedValues[i]) 
    }
    # If the data table exists, modify table else create table:
    if(exists('responseData1')){
      # If no rows are selected, add a row with the new record:
      if(length(input$responses1_rows_selected) < 1){
        responseData1[nrow(responseData1) + 1,] <- castData(formData1())
      }
      # If a row has been selected, modify the selected record:
      if(length(input$responses1_rows_selected == 1)){
        responseData1[input$responses1_rows_selected,] <- castData(formData1())
      }
      # If the table is currently blank, start table with new record:
    } else {
      responseData1 <- castData(formData1())
    }
    globalAssign(responseData1, 'responseData1')
    
    # After submission, clear fields to defaults:
    updateInputs(createDefaultRecord(fieldCodes1), fieldCodes1, session)
    }, priority = 1)
  
  # Press New to display empty record:
  
  observeEvent(input$new, {
    updateInputs(createDefaultRecord(fieldCodes1), fieldCodes1, session)
  })
  
  # Delete a selected row:
  
  observeEvent(input$delete1, {
    if(length(input$responses1_rows_selected) == 1){
      responseData1 <<- responseData1[-input$responses1_rows_selected,]
      updateInputs(createDefaultRecord(fieldCodes1), fieldCodes1, session)
      }
    }, priority = 1)
  
  # Select row in table to show details in inputs:
  
  observeEvent(input$responses1_rows_selected, {
    if (length(input$responses1_rows_selected) == 1) {
      data <- responseData1[input$responses1_rows_selected, ]
      updateInputs(data, fieldCodes1, session)
      responseData1[input$responses1_rows_selected, ] 
    }
  })
  
  # Display table:
  
  output$responses1 <- DT::renderDataTable({
    # Update after submit is clicked
    input$submit1
    # Update after delete is clicked
    input$delete1
    if (existCheck(responseData1)) responseData1    
  }, server = FALSE, selection = "single",
  colnames = unname(getTableMetadata(fieldCodes1, fieldNames1)$fields))
  
  #----------------------------------------------------------------------*
  # TABLE TWO
  #----------------------------------------------------------------------*
  # Set field codes and names:
  
  fieldCodes2 <- c('name', 'hello', 'world')
  fieldNames2 <- c('Name', 'Hello', 'World')
  
  # Input fields:
  
  formData2 <- reactive({
    sapply(names(getTableMetadata(fieldCodes2, fieldNames2)$fields),
           function(x) input[[x]])
  })
  
  # Click submit to add table or modify/add records:
  
  observeEvent(input$submit2, {
    # Assign fixed values fields (will borrow from other inputs):
    fixedValues <- c('name')
    for(i in 1:length(fixedValues)){
      globalAssign(input[[fixedValues[i]]], fixedValues[i]) 
    }
    
    # If the data table exists, modify table else create table:
    if(exists('responseData2')){
      # If no rows are selected, add a row with the new record:
      if(length(input$responses2_rows_selected) < 1){
        responseData2[nrow(responseData2) + 1,] <- castData(formData2())
      }
      # If a row has been selected, modify the selected record:
      if(length(input$responses2_rows_selected == 1)){
        responseData2[input$responses2_rows_selected,] <- castData(formData2())
      }
      # If the table is currently blank, start table with new record:
    } else {
      responseData2 <- castData(formData2())
    }
    globalAssign(responseData2, 'responseData2')
    
    # After submission, clear fields to defaults:
    updateInputs(createDefaultRecord(fieldCodes2), fieldCodes2, session)
  }, priority = 1)
  
  # Press New to display empty record:
  
  observeEvent(input$new2, {
    updateInputs(createDefaultRecord(fieldCodes2), fieldCodes2, session)
  })
  
  # Delete a selected row:
  
  observeEvent(input$delete2, {
    if(length(input$responses2_rows_selected) == 1){
      responseData2 <<- responseData2[-input$responses2_rows_selected,]
      updateInputs(createDefaultRecord(fieldCodes2), fieldCodes2, session)
    }
  }, priority = 1)
  
  # Select row in table to show details in inputs:
  
  observeEvent(input$responses2_rows_selected, {
    if (length(input$responses2_rows_selected) == 1) {
      data <- responseData2[input$responses2_rows_selected, ]
      updateInputs(data, fieldCodes2, session)
      responseData2[input$responses2_rows_selected, ] 
    }
  })
  
  # Display table:
  
  output$responses2 <- DT::renderDataTable({
    # Update after submit is clicked
    input$submit2
    # Update after delete is clicked
    input$delete2
    if (existCheck(responseData2)) responseData2   
  }, server = FALSE, selection = "single",
  colnames = unname(getTableMetadata(fieldCodes2, fieldNames2)$fields))
}

shinyApp(ui, server)

# rm(list = ls())

