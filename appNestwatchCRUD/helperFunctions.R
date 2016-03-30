# FUNCTIONS

#---------------------------------------------------------------------------------*
# ---- Save data to Dropbox ----
#=================================================================================*

# Save visit data to Dropbox:

saveVisitData <- function(visitData) {
  # data <- visitData 
  # Create a unique file name
  fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data)) #! This may be a problem!
  # Write the data to a temporary file locally
  filePath <- file.path(tempdir(), fileName)
  write.csv(visitData, filePath, row.names = FALSE, quote = TRUE)
  # Upload the file to Dropbox
  drop_upload(filePath, dest = 'visitData')
}

# Save encounter data to Dropbox:

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

# Save point count data (conditions) to Dropbox:

savePcDataConditions <- function(pcDataConditions) {
  data <- pcDataConditions 
  # Create a unique file name
  fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data)) #! This may be a problem!
  # Write the data to a temporary file locally
  filePath <- file.path(tempdir(), fileName)
  write.csv(data, filePath, row.names = FALSE, quote = TRUE)
  # Upload the file to Dropbox
  drop_upload(filePath, dest = 'pcDataConditions')
}

# Save point count data (counts) to Dropbox:

savePcDataCounts <- function(pcData) {
  data <- pcData
  # Create a unique file name
  fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  # Write the data to a temporary file locally
  filePath <- file.path(tempdir(), fileName)
  write.csv(data, filePath, row.names = FALSE, quote = TRUE)
  # Upload the file to Dropbox
  drop_upload(filePath, dest = 'pcDataCounts')
}

#---------------------------------------------------------------------------------*
# ---- The complicated path from input to editable data frame ----
#=================================================================================*

# This method casts from the inputs to a one-row data.frame. We use it, for instance, when the user creates a new record by typing in values into the inputs, and then clicks "Submit":

castData <- function(data) {
  datar <- data.frame(sitev = as.character(data["sitev"]),
                      datev = dateOut, #as.character(data["datev"]),
                      bandTime = as.character(data["bandTime"]),
                      bander = data["bander"],
                      encounterType = data["encounterType"],
                      species = data["species"],
                      bandNumber = data["bandNumber"],
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
                      toenailID = data["toenailID"],
                      bloodID = data["bloodID"],
                      fecalID = data["fecalID"],
                      attachmentID = data["attachmentID"],
                      notes = as.character(data["notes"]),
                      stringsAsFactors = FALSE)
  rownames(datar) <- data["id"]
  return (datar)
}

# This creates an empty record, to be used e.g. to fill the inputs with the default values when the user clicks the "New" button:

createDefaultRecord <- function() {
  mydefault <- castData(list(id = "0", 
                             sitev = '',
                             datev = Sys.time(),
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
                             toenailID = '',
                             bloodID = '',
                             fecalID = '',
                             attachmentID = '',
                             notes = ''))
  return (mydefault)
}

# And this method takes the data as selected in the DataTable, and updates the inputs with the respective values:

updateInputs <- function(data, session) {
  updateTextInput(session, "id", value = unname(rownames(data)))
  updateTextInput(session, "sitev", value = unname(data['sitev']))
  updateTextInput(session, "datev", value = dateOut)#unname(as.character(data['datev'])))
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
  updateTextInput(session, "toenailID", value = unname(data["toenailID"]))
  updateTextInput(session, "bloodID", value = unname(data["bloodID"]))
  updateTextInput(session, "fecalID", value = unname(data["fecalID"]))
  updateTextInput(session, "attachmentID", value = unname(data["attachmentID"]))
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
              sitev = "Site",
              datev = "Date",
              bandTime = 'Time',
              bander = 'Obs.',
              encounterType = 'Encounter',
              species = 'SPP',
              bandNumber = 'Band #',
              colorCombo  = 'Color c.',
              age = 'Age',
              sex = 'Sex',
              breedingCond = 'CP/BP',
              fat = 'Fat',
              mass = 'Mass',
              wing = 'Wing',
              tail = 'Tail',
              tarsus = 'Tarsus',
              featherID = 'Feather',
              toenailID = 'Toenail',
              bloodID = 'Blood',
              fecalID = 'Fecal',
              attachmentID = 'Attachment',
              notes = "Notes")
  result <- list(fields = fields)
  return (result)
}

#---------------------------------------------------------------------------------*
# ---- Point count editable frame ----
#=================================================================================*

# This method casts from the inputs to a one-row data.frame. We use it, for instance, when the user creates a new record by typing in values into the inputs, and then clicks "Submit":

castDataPc <- function(dataPc) {
  datarPc <- data.frame(sitePc = sitePc,
                      observerPc = observerPc,
                      datePc = dateOutPc, 
                      startTimePc = startTimePc,
                      timePc = dataPc["timePc"],
                      speciesPc = dataPc["speciesPc"],
                      distancePc = dataPc["distancePc"],
                      countPc  = dataPc["countPc"],
                      detectionPc = dataPc["detectionPc"],
                      stringsAsFactors = FALSE)
  rownames(datarPc) <- dataPc["idPc"]
  return (datarPc)
}

# This creates an empty record, to be used e.g. to fill the inputs with the default values when the user clicks the "New" button:

createDefaultRecordPc <- function() {
  mydefault <- castDataPc(list(idPc = "0", 
                             sitePc = '',
                             observerPc = '',
                             datePc = '',
                             startTimePc = '',
                             timePc = '',
                             speciesPc = '',
                             distancePc = '',
                             countPc  = '',
                             detectionPc = ''
                             ))
  return (mydefault)
}

# And this method takes the data as selected in the DataTable, and updates the inputs with the respective values:

updateInputsPc <- function(dataPc, session) {
  updateTextInput(session, "idPc", value = unname(rownames(dataPc)))
  updateTextInput(session, "sitePc", value = sitePc)
  updateTextInput(session, "observerPc", value = observerPc)
  updateTextInput(session, "datePc", value = dateOutPc)
  updateTextInput(session, "startTimePc", value = startTimePc)
  updateTextInput(session, "timePc", value = unname(dataPc["timePc"]))
  updateTextInput(session, "speciesPc", value = unname(dataPc["speciesPc"]))
  updateTextInput(session, "distancePc", value = unname(dataPc["distancePc"]))
  updateTextInput(session, "countPc", value = unname(dataPc["countPc"]))
  updateTextInput(session, "detectionPc", value = unname(dataPc["detectionPc"]))
}

# This function finds the next ID of a new record. In mysql, this could be done by an incremental index, automatically. But here, we do it manually, ourselves:

getNextIdPc <- function() {
  if (exists("responsesPc")) {
    max(as.integer(rownames(responsesPc))) + 1
  } else {
    return (1)
  }
}

# Create data:

createDataPc <- function(dataPc) {
  dataPc <- castDataPc(dataPc)
  rownames(dataPc) <- getNextIdPc()
  if (exists("responsesPc")) {
    responsesPc <<- rbind(responsesPc, dataPc)
  } else {
    responsesPc <<- dataPc
  }
}

# Read

readDataPc <- function() {
  if (exists("responsesPc")) {
    responsesPc
  }
}

# Update

updateDataPc <- function(dataPc) {
  dataPc <- castDataPc(dataPc)
  responsesPc[row.names(responsesPc) == row.names(dataPc), ] <<- dataPc
}


# Delete

deleteDataPc <- function(dataPc) {
  responsesPc<<- responsesPc[row.names(responsesPc) != unname(dataPc["idPc"]), ]
}

# The only thing that might not be straight forward is the GetTableMetadata function. We'll use it as a starting point for further development, as described below. For now, it's just a method that defines the names of the columns in our table:

getTableMetadataPc <- function() {
  fields <- c(idPc = "Id",
              sitePc = "Site",
              observerPc = 'Observer',
              datePc = "Date",
              startTimePc = 'Start time',
              timePc = 'Time interval',
              speciesPc = 'SPP',
              distancePc = 'Distance',
              countPc  = 'Count',
              detectionPc = 'Detection'
              )
  result <- list(fields = fields)
  return (result)
}