read_csv('trackingData.csv') %>%
  mutate(
    birdID = case_when(
      birdID == 'Green' ~ 'Grindylow',
      birdID == 'Purple' ~ 'Plimpy',
      birdID == 'Yellow' ~ 'Yeti',
      TRUE ~ birdID
      ), 
  ) %>%
  write_csv('trackingData.csv')

  # filter(species == 'BRPE') %>%
  select(birdID) %>%
  View
  distinct

