
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
web_table2 %>% 
  select(X1, X2) %>% 
  rename('state_name'  = X1,
         'women_index' = X2) %>% 
  mutate(women_index = str_replace(women_index, '–', '-'),
         women_index = parse_number(women_index)) -> women_leadership_table



#------------------------------------------------------------#

# Operationalizing Dependent Variable (Election result) #

# Ryan Buczkowski #

#------------------------------------------------------------#

# Importing data file from downloads
read_csv('https://raw.githubusercontent.com/buczkowskir/POL_251_Spring_2021/master/individual_scripts_and_data/election_results_2016.csv') %>% 
  mutate(clinton_pct_lead = ((clinton_votes - trump_votes) / (trump_votes + clinton_votes)) * 100,
         clinton_pct_lead = round(clinton_pct_lead, 1)) %>% 
  select(state_name, clinton_pct_lead
         
         ) -> election_results

# Joining all the data together
home_values_joined %>% 
  inner_join(partisanship_table) %>% 
  inner_join(women_leadership_table) %>% 
  inner_join(election_results) -> complete_data

# Writing CSV file
complete_data %>% 
  write_csv(path = 'complete_data.csv')

