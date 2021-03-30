

# Data Analysis of Historic Home Prices

# Ryan Buczkowski

#----------------------------------------------#


# Loading Libraries
pacman::p_load('tidyverse', 'scales')

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
       state_region = state.region) -> states

# Joining data together
inner_join(states, home_values_clean) -> home_values_joined

# Creating CSV file
home_values_joined %>% 
  write_csv(path = 'home_values.csv')

# Visualizing data
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






