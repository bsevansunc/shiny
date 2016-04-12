
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

# Define fields for encounter data:

fieldCodesEnc <-  c('hubEnc', 'siteEnc', 'dateEnc', 'bandTime', 
                      'bander','encounterType', 'speciesEnc', 
                      'bandNumber','colorCombo', 'age', 'sex', 
                      'breedingCond','fat', 'mass', 'wing', 'tail',
                      'tarsus','featherID', 'toenailID', 'bloodID',
                      'fecalID', 'attachmentID', 'notesEnc')

# Define field names for encounter data table:

fieldNamesEnc <- c('Hub', 'Site', 'Date', 'Time', 'Obs.', 'Encounter',
                   'SPP', 'Band #', 'Color c.', 'Age', 'Sex', 'CP/BP',
                   'Fat', 'Mass', 'Wing', 'Tail', 'Tarsus', 'Feather', 
                   'Toenail','Blood','Fecal', 'Attachment', 'Notes')

# Define fields for PC data conditions (site-level pc records):

pcDataConditionsFields <- c('sitePc', 'observerPc', 'datePc', 'startTimePc',
                            'longitude', 'latitude', 'accuracy', 'locationNotes',
                            'temperature', 'sky', 'wind', 'splN', 'splE',
                            'splS', 'splW', 'siteLevelPcNotes')

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
# 
# choiceColorCombos <- expand.grid(rep(list(colorValues), 4)) %>%
#   tbl_df %>%
#   transmute(L = paste(Var1, Var2, sep = '/'),
#             R = paste(Var3, Var4, sep = '/'), 
#             combo = paste(L, R, sep = ',')) %>%
#   select(-c(L, R)) %>%
#   filter(str_count(combo, 'AL') == 1) %>%
#   mutate(combo = combo %>%
#            str_replace_all('/,', ',') %>%
#            str_replace_all(',/', ',') %>%
#            str_replace_all(',$',',-') %>%
#            str_replace_all('^,', '-,') %>%
#            str_replace_all('^/', '') %>%
#            str_replace_all('/$', '') %>%
#            str_replace_all(',', ', ')) %>%
#   distinct %>% .$combo
# 
# choiceColorCombos <- c('', choiceColorCombos) %>% sort

choiceNetCount <- c('', seq(0, 12, by = 1))

choiceNetMinutes <- c('', 0:2000)

choiceCount <- c('', 1:100)

# Band choices:

choiceAge <- c('', 'HY', 'AHY', 'SY', 'ASY', 'UNK')

choiceEncounterType <- c('Band', 'Recap', 'Resight-incidental','Resight-targeted', 'Resight-participant')

choiceSex <- c('', 'M', 'F', 'UNK')

choiceBreedingCond <-  c('','CP', 'BP','CP-', 'BP-','CP+', 'BP+')

choiceFat <- c('', 0, 0.5, seq(1:5))

choiceDistance <- c('', '0-10', '11-20', '21-30', '31-40', '41-50')

choiceTime <- c('', 3, 2, 5)

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

