
#----------------------------------------------#

# Data Analysis of Historic Home Prices

# Ryan Buczkowski

#----------------------------------------------#


# Loading Libraries
pacman::p_load('tidyverse', 'scales', 'ggrepel', 'broom', 'rvest')

# Importing Data
read_csv('https://raw.githubusercontent.com/IQSS/workshops/master/R/Rgraphics/dataSets/landdata-states.csv') -> home_values

# Looking at data
summary(home_values)

# Cleaning/summarizing data
home_values %>% 
  rename('state_abb' = State) %>% 
  group_by(state_abb) %>% 
  summarize(housing_mean = mean(Home.Value)) %>% 
  filter(state_abb != 'DC'
         
  ) -> home_values_clean

# Creating state dataset
tibble(state_name   = state.name,
       state_abb    = state.abb,
       state_region = state.region
       
       ) -> states

# Joining data together
inner_join(states, home_values_clean
           
           ) -> home_values_joined

#------------------------------------------------#

# Data Analysis of Partisanship -- 2016 Election #

# Ryan Buczkowski #

#------------------------------------------------#

# Creating URL object for partisanship table
partisanship_url <- 'https://news.gallup.com/poll/203117/gop-maintains-edge-state-party-affiliation-2016.aspx'

# Scraping partisanship table
read_html(partisanship_url) %>% 
  html_node(xpath = '//*[@id="20170126154846"]/table') %>% 
  html_table(fill = TRUE) %>% 
  as_tibble(
    
  ) -> web_table

# Cleaning web table
web_table %>% 
  select(X1, X4) %>% 
  slice(-c(1,2,53)) %>% 
  mutate(X4 = parse_number(X4)) %>% 
  rename('state_name' = X1,
         'dem_adv'    = X4
         
         ) -> partisanship_table


#----------------------------------------#

# Operationalizing Women Leadership Variable #

# Ryan Buczkowski #

#----------------------------------------#

# Creating URL object
women_url <- 'https://statusofwomendata.org/explore-the-data/political-participation/additional-state-data/political-participation-composite/'

# Scraping web table
read_html(women_url) %>% 
  html_node(xpath = '//*[@id="post-2937"]/table') %>% 
  html_table(fill = TRUE) %>% 
  as_tibble(
    
  ) -> web_table2

# Cleaning web table
women_web_table %>% 
  select(X1, X2) %>% 
  rename('state_name'  = X1,
         'women_index' = X2
         
         ) -> women_leadership_table

#------------------------------------------------------------#

# Operationalizing Dependent Variable (Election result) #

# Ryan Buczkowski #

#------------------------------------------------------------#

# Importing data file from downloads
read_csv('C:/Users/ryanb/Downloads/1976-2020-president.csv') %>% 
  filter(year == 2016) %>% 
  select(state, candidate, candidatevotes) %>% 
  filter(str_detect(candidate, 'CLINTON|TRUMP') == TRUE) %>%
  mutate(candidate  = str_extract(candidate, 'TRUMP|CLINTON'),
         candidate  = str_to_title(candidate),
         state_name = str_to_title(state)) %>% 
  right_join(tibble(state_name = state.name)) %>% 
  select(state_name, candidate, candidatevotes
         
         ) -> data_long

# Cleaning data for pivot
data_long %>% 
  group_by(state_name, candidate) %>% 
  summarize(candidatevotes = sum(candidatevotes)) %>% 
  pivot_wider(names_from = candidate, values_from = candidatevotes) %>% 
  rename('clinton_votes'   = c(2),
         'trump_votes'     = c(3)) %>% 
  ungroup(
    
  ) -> data_wide

# Joining all the data together
home_values_joined %>% 
  inner_join(partisanship_table) %>% 
  inner_join(women_table)






