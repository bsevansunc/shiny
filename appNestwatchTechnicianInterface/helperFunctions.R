# FUNCTIONS

#---------------------------------------------------------------------------------*
# ---- Site list update ----
#=================================================================================*

siteNameSubsetter <- function(inHub){
  encounters %>%
    select(hub, site) %>%
    distinct %>% 
    bind_rows(
      data.frame(hub = c('DC', 'Atlanta', 'Gainesville',
                         'Pittsburgh', 'Raleigh', 'Springfield'),
                 site = rep('', 6))) %>%
    filter(hub == inHub) %>%
    arrange(site) %>%
    .$site
}

#---------------------------------------------------------------------------------*
# ---- Save data to Dropbox ----
#=================================================================================*

saveData <- function(data, dataName, siteName){
  if(dataName != 'visitData') data <- t(data)
  submissionTime <- as.character(Sys.time()) %>%
    str_replace_all(' ', '_') %>%
    str_replace_all(':','-')
  randomNumber <- sample(1:1000, 1)
  fileName <- str_c(dataName, siteName,'_',submissionTime,
                    '_', randomNumber,'.csv')
  filePath <- file.path(tempdir(), fileName)
  write.csv(data, filePath, row.names = FALSE, quote = TRUE)
  destinationFolder <- str_c('nnDataStorage/', dataName)
  # Upload file to Dropbox:
  drop_upload(filePath, dest = destinationFolder)
}

# Save visit data to Dropbox:
# 
# saveVisitData <- function(visitData) {
#   # data <- visitData 
#   # Create a unique file name
#   fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data)) #! This may be a problem!
#   # Write the data to a temporary file locally
#   filePath <- file.path(tempdir(), fileName)
#   write.csv(visitData, filePath, row.names = FALSE, quote = TRUE)
#   # Upload the file to Dropbox
#   drop_upload(filePath, dest = 'nnDataStorage/visitData')
# }

# Save encounter data to Dropbox:

# saveEncounterData <- function(encounterData) {
#   data <- encounterData
#   # Create a unique file name
#   fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
#   # Write the data to a temporary file locally
#   filePath <- file.path(tempdir(), fileName)
#   write.csv(data, filePath, row.names = FALSE, quote = TRUE)
#   # Upload the file to Dropbox
#   drop_upload(filePath, dest = 'nnDataStorage/encounterData')
# }

# Save point count data (counts) to Dropbox:

# savePcDataCounts <- function(pcData) {
#   data <- pcData
#   # Create a unique file name
#   fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
#   # Write the data to a temporary file locally
#   filePath <- file.path(tempdir(), fileName)
#   write.csv(data, filePath, row.names = FALSE, quote = TRUE)
#   # Upload the file to Dropbox
#   drop_upload(filePath, dest = 'pcDataCounts')
# }

#---------------------------------------------------------------------------------*
# ---- Create, Read, Update, Delete ----
#=================================================================================*

# Existence check

# existCheck <- function(dataObjectToCheck){
#   deparse(substitute(dataObjectToCheck)) %>% exists
# }

# Assign an object to the global environment (more flexible than <<- as it allows the user to specify the name of the object inside the function)
# 
# globalAssign <- function(data, dataName){
#   assign(dataName, data, envir = .GlobalEnv )
# }

# Function assigns fancy column headers to the field codes:

getTableMetadata <- function(fieldCodes, fieldNames) {
  fields <- fieldNames
  names(fields) <- fieldCodes
  result <- list(fields = fields)
  return (result)
}

# Function takes values of inputs, if there are any, and puts them in a 1-row data frame:

castData <- function(fieldValues){
  data <- as.list(fieldValues)
  return(data.frame(data, stringsAsFactors = FALSE))
}

# Cast a blank record:

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

# Make some submissions blank:

createBlankInputs <- function(fieldCodes, session){
  for(i in 1:length(fieldCodes)){
    updateTextInput(session, fieldCodes[i], value = '')
  }
}

# Update the field inputs on the ui:

updateInputs <- function(data, fieldCodes, session) {
  for(i in 1:length(fieldCodes)){
    updateTextInput(session, fieldCodes[i],
                    value = unname(data[fieldCodes[i]]))
  }
}

# Update vector of select inputs based on input value:
# 
# updateSelectInputVector <- function(selectInputVector, input, session){
#   siteInputs <- c('siteEnc', 'siteQuery', 'sitePc', 'siteNest')
#   for(i in 1:length(selectInputVector)){
#     updateSelectInput(session, 
#                       selectInputVector[i],
#                       selected = input[[selectInputVector[i]]])
#   }
# }

# Update input selections:

# updateSelectedInputs <- function(inputsToUpdate, )

