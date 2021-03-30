
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

home_values_joined %>% 
  ggplot(aes(x = reorder(state_abb, housing_mean), y = housing_mean)) +
  geom_col(aes(fill = state_region), color = 'black') +
  labs(x        = 'State',
       y        = 'Housing Price',
       title    = 'Mean of Historic Home Price',
       subtitle = 'By State',
       caption  = 'Visualization created using ggplot2 in RStudio \nCreator: Ryan Buczkowski - University of Mississippi - Political Science Department') +
  scale_fill_discrete(name = 'State Region') +
  theme_minimal() +
  scale_y_continuous(labels = dollar) +
  theme(
    axis.text         = element_text(face  = 'bold.italic',
                                     size  = 9),
    axis.title        = element_text(face  = 'bold',
                                     size  = 14),
    plot.title        = element_text(face  = 'bold',
                                     size  = 18),
    plot.subtitle     = element_text(face  = 'italic',
                                     size  = 9),
    legend.position   = 'bottom',
    legend.background = element_rect(color = 'black'),
    legend.title      = element_text(face  = 'bold'),
    legend.text       = element_text(face  = 'bold.italic'),
    plot.caption      = element_text(face  = 'italic',
                                     size  = 9,
                                     hjust = 0)
  )




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



partisanship_table %>% 
  ggplot(aes(x = reorder(state_name, dem_adv), y = dem_adv)) +
  geom_col(color = 'black') +
  labs(x        = 'State',
       y        = 'Democrat Advantage',
       title    = 'Partisanship Differences',
       subtitle = 'By State',
       caption  = 'Visualization created using ggplot2 in RStudio \nCreator: Ryan Buczkowski - University of Mississippi - Political Science Department') +
  theme_minimal() +
  theme(
    axis.text         = element_text(face  = 'bold.italic',
                                     size  = 9),
    axis.title        = element_text(face  = 'bold',
                                     size  = 14),
    plot.title        = element_text(face  = 'bold',
                                     size  = 18),
    plot.subtitle     = element_text(face  = 'italic',
                                     size  = 9),
    legend.position   = 'bottom',
    legend.background = element_rect(color = 'black'),
    legend.title      = element_text(face  = 'bold'),
    legend.text       = element_text(face  = 'bold.italic'),
    plot.caption      = element_text(face  = 'italic',
                                     size  = 9,
                                     hjust = 0)
  )








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
read_csv('https://raw.githubusercontent.com/buczkowskir/POL_251_Spring_2021/master/election_results_2016.csv') %>% 
  mutate(clinton_pct_lead = ((clinton_votes - trump_votes) / (trump_votes + clinton_votes)) * 100,
         clinton_pct_lead = round(clinton_pct_lead, 1)) %>% 
  select(state_name, clinton_pct_lead
         
         ) -> election_results

# Joining all the data together
home_values_joined %>% 
  inner_join(partisanship_table) %>% 
  inner_join(women_leadership_table) %>% 
  inner_join(election_results) -> complete_data


complete_data %>% 
  ggplot(aes(y = clinton_pct_lead, x = women_index)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_minimal() +
  theme(
    axis.text         = element_text(face  = 'bold.italic',
                                     size  = 9),
    axis.title        = element_text(face  = 'bold',
                                     size  = 14),
    plot.title        = element_text(face  = 'bold',
                                     size  = 18),
    plot.subtitle     = element_text(face  = 'italic',
                                     size  = 9),
    legend.position   = 'bottom',
    legend.background = element_rect(color = 'black'),
    legend.title      = element_text(face  = 'bold'),
    legend.text       = element_text(face  = 'bold.italic'),
    plot.caption      = element_text(face  = 'italic',
                                     size  = 9,
                                     hjust = 0)
  )

  





