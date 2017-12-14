# Add choice color combos as data frame:

colorValues <- c('', 'A', 'BU', 'BK', 'G', 'O','PK', 'P','R', 'Y', 'W')

choiceColorCombos <- expand.grid(rep(list(colorValues), 4)) %>%
  tbl_df %>%
  transmute(L = paste(Var1, Var2, sep = '/'),
            R = paste(Var3, Var4, sep = '/'), 
            combo = paste(L, R, sep = ',')) %>%
  select(-c(L, R)) %>%
  filter(str_count(combo, 'A') == 1) %>%
  mutate(combo = combo %>%
           str_replace_all('/,', ',') %>%
           str_replace_all(',/', ',') %>%
           str_replace_all(',$',',-') %>%
           str_replace_all('^,', '-,') %>%
           str_replace_all('^/', '') %>%
           str_replace_all('/$', '')) %>%
  distinct %>% .$combo

choiceColorCombos1 <- data.frame(
  choiceColorCombos = c('', choiceColorCombos),
  stringsAsFactors = FALSE
  ) %>% 
  tbl_df %>%
  arrange(choiceColorCombos)

write.csv(choiceColorCombos1, 'choiceColorCombos.csv', row.names = FALSE)
