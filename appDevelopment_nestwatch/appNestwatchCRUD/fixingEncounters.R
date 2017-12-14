# fixing encounters:

read.csv('encounters.csv', stringsAsFactors = F) %>%
  tbl_df %>%
  mutate(date = as.Date(date)) %>%
  distinct %>%
  write.csv('encounters.csv', row.names = FALSE)