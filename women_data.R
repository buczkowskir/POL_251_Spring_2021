
#-----------------------------------------#

# Operationalizing Women Leadership Variable #

# Ryan Buczkowski #

#-----------------------------------------#

# Loading Libraries
pacman::p_load('tidyverse', 'rvest')


# Creating URL object
women_url <- 'https://statusofwomendata.org/explore-the-data/political-participation/additional-state-data/political-participation-composite/'

# Scraping web table
read_html(women_url) %>% 
  html_node(xpath = '//*[@id="post-2937"]/table') %>% 
  html_table(fill = TRUE) %>% 
  as_tibble() -> web_table2

# Cleaning web table
women_web_table %>% 
  select(X1, X2) %>% 
  rename('state_name'  = X1,
         'women_index' = X2) -> women_leadership_table

# Join all data together
read_csv('home_values.csv') %>% 
  inner_join(read_csv('partisanship_table.csv')) %>% 
  inner_join(women_table) -> joined_unclean

# Cleaning joined data
joined_unclean %>% 
  mutate(women_index = str_replace(women_index, '–', '-'),
         women_index = parse_number(women_index)
         
  ) -> joined_clean

# Creating CSV file
joined_clean %>% 
  write_csv(path = 'women_partisanship_home_values.csv')

# Visualizing women data
joined_clean %>% 
  ggplot(aes(x = state_region, y = women_index)) +
  geom_boxplot(aes(fill = state_region))

joined_clean %>% 
  arrange(women_index)

# Seeing relationship between variables
joined_clean %>% 
  select(housing_mean, dem_adv, women_index) %>% 
  cor()

# Visualizing relationship between variables
joined_clean %>% 
  ggplot(aes(x = log(housing_mean), y = dem_adv)) +
  geom_point() +
  geom_smooth(method = 'lm')
