#---------------------------------------------------------------------------------*
# ---- VISIT ----
#---------------------------------------------------------------------------------*

# Define fields for visit data:

visitFields <- c('hub', 'site', 'observer',
                 'longitude', 'latitude', 'accuracy', 'locationNotes',
                 'netCount6', 'netTime6','netCount9', 'netTime9',
                 'netCount12', 'netTime12','netCount18', 'netTime18',
                 'startRsTime', 'endRsTime',
                 'rsPathDistace', 'amroUnbanded', 'bcchUnbanded', 'brthUnbanded',
                 'cachUnbanded', 'carwUnbanded', 'eaphUnbanded','grcaUnbanded',
                 'howrUnbanded',
                 'nocaUnbanded', 'nomoUnbanded', 'sospUnbanded',
                 'tutiUnbanded', 'unchUnbanded', 'encounteredBird','visitNotes')

# Visit choices

choiceRegions <- c('Atlanta', 'DC', 'Gainesville', 'Pittsburgh',
                   'Raleigh', 'Springfield')

names(choiceRegions) <- choiceRegions

choiceSites <- c('', encounters$site %>% unique %>% sort)

choiceDate <- c('', seq(
  as.Date(ISOdate(2000, 1, 15)),
  as.Date(ISOdate(2030, 1, 1)), 1) %>%
    as.character)

timeOfDay0 <- format(seq(ISOdate(2000, 1, 1), ISOdate(2000,1,2), 
                         by = 'min'), '%H:%M') %>% 
  unique %>% sort

timeOfDay <- timeOfDay0[301:1321]

choiceTimeOfDay <- c('',timeOfDay)

choiceSpecies <- c('', 'AMRO', 'BCCH', 'BRTH', 'CACH', 'CARW', 
                   'EAPH','GRCA','HOWR','NOCA','NOMO','SOSP',
                   'TUTI','UNCH')

colorValues <- c('', 'A', 'BU', 'BK', 'G', 'O','PK', 'P','R', 'Y', 'W')

choiceNetCount <- c('', seq(0, 12, by = 1))

choiceNetMinutes <- c('', 0:2000)

choiceCount <- c('', 1:100)

#---------------------------------------------------------------------------------*
# ---- ENCOUNTERS ----
#---------------------------------------------------------------------------------*

# Define fields for encounter data:

fieldCodesEnc <-  c('hubEnc', 'siteEnc', 'dateEnc', 'bandTime', 
                    'observerEnc','encounterType', 'speciesEnc', 
                    'bandNumber','colorCombo', 'age', 'sex', 
                    'breedingCond','fat', 'mass', 'wing', 'tl',
                    'tarsus','featherID', 'toenailID', 'bloodID',
                    'fecalID', 'attachmentID', 'notesEnc')

# Define field names for encounter data table:

fieldNamesEnc <- c('Hub', 'Site', 'Date', 'Time', 'Obs.', 'Encounter',
                   'SPP', 'Band #', 'Color c.', 'Age', 'Sex', 'CP/BP',
                   'Fat', 'Mass', 'Wing', 'Tl', 'Tars', 'Feather', 
                   'Toenail','Blood','Fecal', 'Attachment', 'rsLong',
                   'rsLat', 'Notes')

# Define fields for encounter data that will be blank between records:

blankFieldsEnc <- c('bandTime', 'encounterType', 'speciesEnc', 
                    'bandNumber','colorCombo', 'age', 'sex', 
                    'breedingCond','fat', 'mass', 'wing', 'tl',
                    'tarsus','featherID', 'toenailID', 'bloodID',
                    'fecalID', 'attachmentID', 'rslong', 'rslat', 'notesEnc')

# Band choices:

choiceAge <- c('', 'HY', 'AHY', 'SY', 'ASY', 'UNK')

choiceEncounterType <- c('','Band', 'Recap',
                         'Resight-incidental','Resight-targeted', 'Resight-participant')

choiceSex <- c('', 'M', 'F', 'UNK')

choiceBreedingCond <-  c('','CP', 'BP','CP-', 'BP-','CP+', 'BP+')

choiceFat <- c('', 0, 0.5, seq(1:5))

choiceDistance <- c('', '0-10', '11-20', '21-30', '31-40', '41-50')

choiceTime <- c('', 3, 2, 5)

#---------------------------------------------------------------------------------*
# ---- POINT COUNTS ----
#---------------------------------------------------------------------------------*

# Define fields for point count data:

fieldCodesPc <- c('hubPc', 'sitePc', 'observerPc', 'datePc',
                  'startTimePc', 'timePc', 'speciesPc', 'distancePc',
                  'countPc', 'detectionPc','notesPc')

# Define field names for point count data table:

fieldNamesPc <- c('Hub', 'Site', 'Observer', 'Date', 'Start time', 
                  'Time interval', 'SPP', 'Distance', 'Count', 
                  'Detection', 'Notes')

# Define fields for point count data that will be blank between records:

blankFieldsPc <- c('hubPc', 'sitePc', 'observerPc', 'datePc',
                  'startTimePc', 'notesPc')

#---------------------------------------------------------------------------------*
# ---- NESTS ----
#---------------------------------------------------------------------------------*

# Define fields for nest data:

fieldCodesNest <- c('hubNest', 'siteNest', 'nestID', 'speciesNest',
                    'dateNest', 'timeNest', 'stageNest', 'adAttNest',
                    'nEggNest', 'nYoungNest', 'notesNest',
                    'observerNest')

# Define field names for nest data table:

fieldNamesNest <- c('Hub', 'Site', 'Nest ID', 'SPP',
                    'Date', 'Time', 'Stage', 'adAtt',
                    'nEgg', 'nYoung', 'Notes', 'Obs')

# Define fields for nest data that will be blank between records:

fieldCodesNest <- c('hubNest', 'siteNest', 'nestID', 'speciesNest')

# Nest choices:

nestLocationChoices <- c('', 'Nestbox', 'Shrub', 'Tree', 'Other')

nestFateChoices <- c('', 'Successful', 'Successful but parasitized', 
                     'Failed: Predated', 
                     'Failed: Starvation',
                     'Failed: Human activity related', 
                     'Failed: Weather related ',
                     'Failed: Parasitized',
                     'Failed: Unknown',
                     'Failed: Other')

nestStageChoices <- c('', 'B', 'L', 'I', 'H', 'N', 'F', 'P', 'A')

nestAttendChoices <- c('', '-', 'F', 'M', 'F+M')

nestEggsYoungChoices <- c('', 0:10)
