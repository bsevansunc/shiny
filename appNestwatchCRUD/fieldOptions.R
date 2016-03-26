
# Define fields for visit data:

visitFields <- c('hub', 'site', 'observer',
                 'netCount', 'netHours', 'startRsTime', 'endRsTime',
                 'rsPathDistace', 'amroUnbanded', 'bcchUnbanded', 'brthUnbanded',
                 'cachUnbanded', 'carwUnbanded', 'eaphUnbanded','grcaUnbanded',
                 'howrUnbanded',
                 'nocaUnbanded', 'nomoUnbanded', 'sospUnbanded',
                 'tutiUnbanded', 'unchUnbanded', 'encounteredBird','notes')

# Visit choices

choiceRegions <- c('Atlanta' = 'Atlanta',
                   'DC' = 'DC',
                   'Gainesville' = 'Gainesville',
                   'Pittsburgh' = 'Pittsburgh',
                   'Raleigh' = 'Raleigh',
                   'Springfield' = 'Springfield')

choiceSites <- c('', encounters$site %>% unique %>% sort)

choiceDate <- c('', seq(
  as.Date(ISOdate(2000, 1, 15)),
  as.Date(ISOdate(2030, 1, 1)), 1) %>%
    as.character)

timeOfDay <- format(seq(ISOdate(2000, 1, 1), ISOdate(2000,1,2), 
                        by = 'min'), '%H:%M') %>% 
  unique %>% sort %>% as.character

choiceTimeOfDay <- c('',timeOfDay)

choiceSpecies <- c('', 'AMRO', 'BCCH', 'BRTH', 'CACH', 'CARW', 
                   'EAPH','GRCA','HOWR','NOCA','NOMO','SOSP',
                   'TUTI','UNCH')

colorValues <- c('', 'AL', 'BU', 'BK', 'G', 'O','PK', 'PU','R', 'Y', 'W')

choiceColorCombos <- expand.grid(rep(list(colorValues), 4)) %>%
  tbl_df %>%
  transmute(L = paste(Var1, Var2, sep = '/'),
            R = paste(Var3, Var4, sep = '/'), 
            combo = paste(L, R, sep = ',')) %>%
  select(-c(L, R)) %>%
  filter(str_count(combo, 'AL') == 1) %>%
  mutate(combo = combo %>%
           str_replace_all('/,', ',') %>%
           str_replace_all(',/', ',') %>%
           str_replace_all(',$',',-') %>%
           str_replace_all('^,', '-,') %>%
           str_replace_all('^/', '') %>%
           str_replace_all('/$', '') %>%
           str_replace_all(',', ',')) %>%
  distinct %>% .$combo

choiceColorCombos <- c('', choiceColorCombos)

choiceNetCount <- c('', seq(0, 12, by = 0.5))

choiceNetHours <- c('', seq(0, 24, by = 0.01))

choiceCount <- c('', 1:100)

# Band choices:

choiceAge <- c('', 'HY', 'AHY', 'SY', 'ASY', 'UNK')

choiceEncounterType <- c('Band', 'Recap', 'Resight-incidental','Resight-targeted', 'Resight-participant')

choiceSex <- c('', 'M', 'F', 'UNK')

choiceBreedingCond <-  c('','CP', 'BP','CP-', 'BP-','CP+', 'BP+')

choiceFat <- c('', 0, 0.5, seq(1:5))