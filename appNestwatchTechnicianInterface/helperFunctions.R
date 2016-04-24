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

saveData <- function(data, dataName, siteName){
  # Unique file name in order to trace potential over-writes:
  submissionTime <- as.character(Sys.time()) %>%
    str_replace_all(' ', '_') %>%
    str_replace_all(':','-')
  randomNumber <- sample(1:1000, 1)
  fileNameUnique <- str_c(dataName, siteName,'_',submissionTime,
                    '_', randomNumber,'.csv')
  filePathUnique <- file.path(tempdir(), fileNameUnique)
  # The combined file:
  fileName <- str_c(dataName, '.csv')
  filePath <- file.path(tempdir(), fileName)
  write.csv(data, filePath, row.names = FALSE, quote = TRUE)
  destinationFolder <- str_c('nnDataStorage/', dataName)
  # Upload file to Dropbox:
  drop_upload(filePath, dest = destinationFolder)
  drop_upload(filePathUnique, dest = destinationFolder)
}

#---------------------------------------------------------------------------------*
# ---- Create, Read, Update, Delete ----
#=================================================================================*

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

# Make some submissions blank:

createBlankInputs <- function(fieldCodes, session){
  for(i in 1:length(fieldCodes)){
    updateTextInput(session, fieldCodes[i], value = '')
  }
}

# Update the field inputs on the ui:

updateInputs <- function(data, fieldCodes, session) {
  for(i in 1:length(fieldCodes)){
    updateTextInput(
      session, fieldCodes[i],
      value = unname(data[fieldCodes[i]])
      )
  }
}


