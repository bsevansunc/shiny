
aou1 <- read.csv('speciesAouCodes.csv', stringsAsFactors = FALSE) %>%
  tbl_df %>%
  rename(SpNumber = Species.Number, 
         Alpha = Alpha.Code, 
         Common = Common.Name, 
         Scientific = Scientific.Name) %>%
  arrange(Alpha) %>% 
  select(Alpha, Common)

dim(aou1)

aou1[1198,] <- c('UNCH', 'Unknown chickadee')

aou1 %>%
  arrange(Alpha) %>%
  write.csv('speciesAouForApp.csv', row.names = FALSE)
  