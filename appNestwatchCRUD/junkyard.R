# Junkyard

# Saving to google spreadsheets

# Google spreadsheets:
# 
# saveVisitData <- function(visitData) {
#   # Grab the Google Sheet
#   sheet <- gs_title('nnVisitData')
#   # Add the data as a new row
#   gs_add_row(sheet, input = visitData)
# }

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


#   output$responses <- DT::renderDataTable({
#     castData(createDefaultRecord())
#   })
# })