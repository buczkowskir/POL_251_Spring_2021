
# Loading Libraries
library(tidyverse)

# Importing data file from downloads
read_csv('C:/Users/ryanb/Downloads/1976-2020-president.csv') %>% 
  filter(year == 2016) %>% 
  select(state, candidate, candidatevotes) %>% 
  filter(str_detect(candidate, 'CLINTON|TRUMP') == TRUE) %>%
  mutate(candidate  = str_extract(candidate, 'TRUMP|CLINTON'),
         candidate  = str_to_title(candidate),
         state_name = str_to_title(state)) %>% 
  right_join(tibble(state_name = state.name)) %>% 
  select(state_name, candidate, candidatevotes) -> data_long

# Cleaning data for pivot
data_long %>% 
  group_by(state_name, candidate) %>% 
  summarize(candidatevotes = sum(candidatevotes)) %>% 
  pivot_wider(names_from = candidate, values_from = candidatevotes) %>% 
  rename('clinton_votes'   = c(2),
         'trump_votes'     = c(3)) %>% 
  ungroup() -> data_wide


# Writing CSV file 
data_wide %>% 
  write_csv(path = 'election_results_2016.csv')

# Creating complete data set
data_wide %>% 
  inner_join(read_csv('https://raw.githubusercontent.com/buczkowskir/POL_251_Spring_2021/master/women_partisanship_home_values.csv')) %>% 
  write_csv(path = 'complete_data.csv')
