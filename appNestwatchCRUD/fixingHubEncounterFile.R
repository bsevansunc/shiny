library(stringr)

encounters %>%
  select(hub, species) %>%
  distinct %>%
  arrange(species) %>%
  filter(species %in%
           c('AMRO', 'BCCH', 'BRTH', 'CACH', 'CARW', 'EAPH',
             'GRCA', 'HOWR', 'NOCA', 'NOMO', 'SOSP', 'TUTI',
             'UNCH'),
         !(hub == 'DC' & 
             species %in% c('AMGO', 'DOWO', 'EAPH', 'HOFI', 'RWBL'))) %>%
  write.csv('hubSpecies.csv', row.names = F)

getwd()

#setwd('/Users/bsevans/Desktop/gits/shiny/appNestwatchCRUD')
             
setwd('/Users/bsevans/Dropbox/NeighborhoodNestwatch_bandingData')

list.files()

list.files('cleanedData')

test <- read.csv('cleanedData/encounters2.csv') %>% tbl_df

filter(test, hub == 'springfield')

# Fixing hub file for app:

read.csv('encounters.csv', stringsAsFactors = FALSE) %>%
  tbl_df %>%
  mutate(hub = capitalize(hub),
       hub = ifelse(hub == 'Dc', 'DC', hub),
       date = as.Date(date),
       site = toupper(site)) %>%
  select(-c(time, notes)) %>%
  mutate(
    encounterType = ifelse(encounterType == 'rsParticipant',
                           'resight-participant',
                           encounterType),
    encounterType = ifelse(encounterType == 'rsTechnician',
                           'resight-technician',
                           encounterType),
    bandCombo =  as.character(bandCombo) %>%
      str_replace('\xa0', '') %>%
      str_replace(',', ', ') %>%
      toupper) %>%
  write.csv('encounters.csv', row.names = FALSE)

# Fixing bad color combos

read.csv('encounters.csv', stringsAsFactors = FALSE) %>%
  tbl_df %>%
  mutate(
    bandCombo = bandCombo %>%
      str_replace_all('A', 'AL') %>%
      str_replace_all('ALL', 'AL') %>%
      str_replace_all('PU', 'P') %>%
      str_replace_all('PNK', 'PK') %>%
      str_replace_all('OB', 'O') %>% # Not really sure about this one!
      str_replace_all('LB', 'BU') %>%
      str_replace_all('BL', 'BU') %>%
      str_replace_all('DB', 'BU') %>%
      str_replace_all('BU', 'XU') %>%
      str_replace_all('BK', 'XK') %>%
      str_replace_all('B|XU', 'BU') %>%
      str_replace_all('XK', 'BK') %>%
      str_replace_all('AL(R.LEG)', '-, AL') %>%
      str_replace_all('R.AL,', 'R/AL') %>%
      str_replace_all('.AL/G', ', AL/G') %>%
      str_replace_all(', .AL', ', AL') %>%
      str_replace_all('G, Y.AL', 'G, Y/AL') %>%
      str_replace_all('PK, AL.G', 'PK, AL/G') %>%
      str_replace_all('W.AL, BK', 'W/AL, BK') %>%
      str_replace_all('O, AL.O', 'O, AL/O') %>%
      str_replace_all('Y.R, AL', 'Y/R, AL') %>%
      str_replace_all('R, AL.G', 'R, AL/G') %>%
      str_replace_all('\\.', ', ') %>%
      str_replace_all(',,', ',') %>%
      str_replace_all(', ,', ',') %>%
      str_replace_all('  ', ' ') %>%
      str_replace_all('WH', 'W') %>%
      str_replace_all('NONE', '-'),
    bandCombo = ifelse(str_detect(bandCombo, 'LEG'),
                       '-, AL', bandCombo)
    ) %>%
  write.csv('encounters.csv', row.names = FALSE)

# ================================================================
# Replace AL with A, no spaces

read.csv('encounters.csv', stringsAsFactors = FALSE) %>%
  tbl_df %>%
  mutate(bandCombo = bandCombo %>%
           str_replace_all('AL', 'A') %>%
           str_replace_all(' ', '')) %>%
  write.csv('encounters.csv', row.names = FALSE)
# ================================================================
# Add dash between band prefix and suffix

read.csv('encounters.csv', stringsAsFactors = FALSE) %>%
  tbl_df %>%
  mutate(bandNumber = ifelse(str_detect(bandNumber, '[:digit:]'),
                             str_c(
                               str_sub(bandNumber, end = -6),
                               str_sub(bandNumber, start = -5),
                               sep = '-'),
                             bandNumber)) %>%
  write.csv('encounters.csv', row.names = FALSE)





  
