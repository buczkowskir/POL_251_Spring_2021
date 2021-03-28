
# Loading Libraries
pacman::p_load('tidyverse', 'broom', 'cowplot', 'scales')

# Importing data
read_csv('https://raw.githubusercontent.com/buczkowskir/POL_251_Spring_2021/master/complete_data.csv') -> df

# Looking at distribution of variables
df %>% 
  ggplot(aes(x = clinton_pct)) +
  geom_density()

df %>% 
  ggplot(aes(x = housing_mean)) +
  geom_density()

df %>% 
  ggplot(aes(x = dem_adv)) +
  geom_density()

df %>% 
  ggplot(aes(x = women_index)) +
  geom_density()


# Looking at variables across state region
df %>% 
  ggplot(aes(x = state_region, y = clinton_pct)) +
  geom_boxplot(aes(fill = state_region)) +
  theme_minimal() +
  labs(x = 'State Region',
       y = 'Percent Share of Vote for Clinton') +
  scale_fill_discrete(guide = FALSE
                      
  ) -> box1

df %>% 
  ggplot(aes(x = state_region, y = housing_mean)) +
  geom_boxplot(aes(fill = state_region)) +
  theme_minimal() +
  labs(x = 'State Region',
       y = 'Mean Home Price') +
  scale_y_continuous(labels = dollar, trans = 'log') +
  scale_fill_discrete(guide = FALSE
                      
  ) -> box2

df %>% 
  ggplot(aes(x = state_region, y = dem_adv)) +
  geom_boxplot(aes(fill = state_region)) +
  theme_minimal() +
  labs(x = 'State Region',
       y = 'Democrat Advantage') +
  scale_fill_discrete(guide = FALSE
                      
  ) -> box3

df %>% 
  ggplot(aes(x = state_region, y = women_index)) +
  geom_boxplot(aes(fill = state_region)) +
  theme_minimal() +
  labs(x = 'State Region',
       y = 'Political Participation of Women') +
  scale_fill_discrete(guide = FALSE
                      
  ) -> box4

plot_grid(box1, box2, box3, box4)

# first regression model
reg1 <- lm(clinton_pct ~ housing_mean + women_index + dem_adv,
           data = df)

summary(reg1)

# Second regression model with log of the housing mean
reg2 <- lm(clinton_pct ~ log(housing_mean) + women_index + dem_adv,
           data = df)

summary(reg2)

# Pulling apart the model
augment(reg2) %>% 
  ggplot(aes(x = .fitted, y = clinton_pct)) +
  geom_point() +
  geom_smooth(method = 'lm')

tidy(reg2)
glance(reg2)