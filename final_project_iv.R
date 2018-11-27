library(SASxport)
library(dplyr)
library(ggplot2)

total_nutrition <- read.xport('DR1TOT_I.xpt')
demographics <- read.xport('DEMO_I.xpt')
individual_foods <- read.xport('DR1IFF_I.xpt')
food_codes <- read.xport('DRXFCD_I.xpt')
body_measures <- read.xport('BMX_I.xpt')

data <- individual_foods %>%
  select(id = SEQN, sodium_content = DR1ISODI, location = DR1.040Z, day_of_week = DR1DAY, meal = DR1.030Z) %>%
      filter(location == 1 | location == 2) %>%
        mutate(home_or_away = factor(location, labels=c("at home","away"))) %>%
          filter(meal == 1 | meal == 2 | meal == 3) %>%
            mutate(meal_name = factor(meal,labels = c("breakfast","lunch","dinner"))) %>%
              mutate(day_type = factor(day_of_week == 1 | day_of_week == 7,labels=c("weekday","weekend"))) %>%
                filter(!is.na(sodium_content))

# One variable: at home or not

summary(lm(sodium_content ~ home_or_away, data = hw4_data))

ggplot(hw4_data,aes(x=home_or_away,y=sodium_content)) + 
  geom_boxplot() +
  coord_cartesian(ylim = quantile(hw4_data$sodium_content, c(0, 0.8))) +
  labs(x="",y="Sodium content (mg)") 

# Two variables: add weekday or weekend
summary(lm(sodium_content ~ home_or_away + day_type, data = hw4_data))

# Three variables: add ratio of family income to federal poverty line (capped at 5)

# Match with demographic

id_and_income <- demographics %>%
  select(id = SEQN, income = INDFMPIR) %>% #
    filter(!is.na(income))

hw4_data <- merge(hw4_data,id_and_income)
  
summary(lm(sodium_content ~ home_or_away + day_type + income, data = hw4_data))

# Four variables: add meal (breakfast, lunch, dinner)

summary(lm(sodium_content ~ home_or_away + day_type + income + meal_name, data = hw4_data))

hw4_data %>%
  group_by(meal_name, home_or_away) %>%
      summarise(mean(sodium_content))

hw4_data <- hw4_data %>%
  mutate(poor_or_not = factor(income > 1,labels=c("not poor","poor")))
    
hw4_data %>%
  group_by(poor_or_not,home_or_away) %>%
    summarise(n())

home_income <- hw4_data %>%
  filter(home_or_away == 'at home') %>%
    select(home_or_away,income)

away_income <- hw4_data %>%
  filter(home_or_away == 'away') %>%
    select(home_or_away,income)
