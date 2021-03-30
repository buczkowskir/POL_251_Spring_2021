
# Data Analysis of Partisanship -- 2016 Election #

# Ryan Buczkowski #

#------------------------------------------#

# Loading Libraries
pacman::p_load('tidyverse', 'rvest', 'ggrepel')

# Creating URL object for partisanship table
partisanship_url <- 'https://news.gallup.com/poll/203117/gop-maintains-edge-state-party-affiliation-2016.aspx'

# Scraping partisanship table
read_html(partisanship_url) %>% 
  html_node(xpath = '//*[@id="20170126154846"]/table') %>% 
  html_table(fill = TRUE) %>% 
  as_tibble() -> web_table

# Cleaning web table
web_table %>% 
  select(X1, X4) %>% 
  slice(-c(1,2,53)) %>% 
  mutate(X4 = parse_number(X4)) %>% 
  rename('state_name' = X1,
         'dem_adv'    = X4) -> partisanship_table

# Creating CSV file
partisanship_table %>% 
  write_csv(path = 'partisanship_table.csv')

# Joining data together
read_csv('home_values.csv') %>% 
  inner_join(partisanship_table) -> partisanship_home_values

# Visualizing data
partisanship_home_values %>% 
  ggplot(aes(x = reorder(state_abb, dem_adv), y = dem_adv)) +
  geom_col(aes(fill = dem_adv), color = 'black') +
  scale_fill_gradient2(low      = 'red',
                       mid      = 'white',
                       high     = 'blue',
                       midpoint = 0,
                       name = 'Democrat Advantage') +
  theme_minimal() +
  labs(x = 'State',
       y = 'Democratic Advantage',
       title = 'Democratic Advantage by State') +
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


