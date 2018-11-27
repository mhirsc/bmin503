library(SASxport)
library(dplyr)
library(ggplot2)

demographics <- read.xport('DEMO_I.xpt')
individual_foods_day1 <- read.xport('DR1IFF_I.xpt')
individual_foods_day2 <- read.xport('DR2IFF_I.xpt')

day1_meals <- individual_foods_day1 %>%
  select(id = SEQN, sodium_content = DR1ISODI, calorie_content = DR1IKCAL, meal = DR1.030Z, day_of_week = DR1DAY, location = DR1.040Z, source = DR1FS, code = DR1IFDCD) %>%
    filter(source %in% c(1,2,3,27)) %>% # grocery store, fast food, restaurant or convenience store
      filter(meal %in% c(1,2,3,4)) %>% # breakfast, lunch, dinner, or supper
          filter(location == 1 | location == 2) %>%
            mutate(day_type = factor(day_of_week == 1 | day_of_week == 7,labels=c("weekday","weekend"))) %>%
              mutate(premade = factor(source != 1 | code %in% c(58106200, 58106205, 58106300, 
                                                                58106305, 58106512,58106514,
                                                                58106516, 58106602,58106604, 
                                                                58106606, 58106700,58106702,
                                                                58106705, # pizza
                                                                71401020, 71401032, # French fries
                                                                24198736, 24198746, # chicken nuggets and tenders
                                                                51121035, # garlic bread
                                                                55200020, 55200030, 
                                                                55200040, 55200050, 
                                                                55200060, 55200070, 
                                                                55200080, 55200090, # waffles
                                                                55100010, 55100015, 
                                                                55100020, 55100025, 
                                                                55100030, 55100035, 
                                                                55100040, # pancakes
                                                                55300020, 55300030,
                                                                55300040, 55301031, # French toast
                                                                58145112, 58145113, 
                                                                58145119, 58145136, # mac n cheese
                                                                58131323, # canned ravioli
                                                                58146223, 58146303,
                                                                58146323, 58146333,
                                                                58146343, 58146353,
                                                                58146363, 58146373,
                                                                58146383, 58146393,
                                                                58146403, 58146413,
                                                                58146423, 58146433,
                                                                58146443, 58146453,
                                                                58146603, 58146683, # ready pasta
                                                                71305070, 71403050, # potatoes
                                                                71405040, 71405050, # hash browns
                                                                71501007, 71501061,
                                                                71501071, 71501075, # mashed potatoes
                                                                25230210, 25230220,
                                                                25230320, 25230340,
                                                                25230420, 25230530,
                                                                25230540, 25230550,
                                                                25230560, 25230610,
                                                                25230780, 25230785,
                                                                25230800, 25231110,
                                                                25231120 # deli meat
                                                                ), labels=c("not premade","premade"))) %>% 
                  mutate(location_type = factor(location, labels = c("home","away"))) %>%
                  group_by(id,meal) %>%
                    mutate(sodium_day1 = sum(sodium_content)) %>%
                      mutate(calories_day1 = sum(calorie_content))

day2_meals <- individual_foods_day2 %>%
  select(id = SEQN, sodium_content = DR2ISODI, calorie_content = DR2IKCAL, meal = DR2.030Z, day_of_week = DR2DAY, location = DR2.040Z, source = DR2FS, code = DR2IFDCD) %>%
    filter(source %in% c(1,2,3,27)) %>% # grocery store, fast food, restaurant or convenience store
      filter(meal %in% c(1,2,3,4)) %>% # breakfast, lunch, dinner, or supper
          filter(location == 1 | location == 2) %>%
            mutate(day_type = factor(day_of_week == 1 | day_of_week == 7,labels=c("weekday","weekend"))) %>%
              mutate(premade = factor(source != 1 | code %in% c(58106200, 58106205, 58106300, 
                                                                58106305, 58106512,58106514,
                                                                58106516, 58106602,58106604, 
                                                                58106606, 58106700,58106702,
                                                                58106705, # pizza
                                                                71401020, 71401032, # French fries
                                                                24198736, 24198746, # chicken nuggets and tenders
                                                                51121035, # garlic bread
                                                                55200020, 55200030, 
                                                                55200040, 55200050, 
                                                                55200060, 55200070, 
                                                                55200080, 55200090, # waffles
                                                                55100010, 55100015, 
                                                                55100020, 55100025, 
                                                                55100030, 55100035, 
                                                                55100040, # pancakes
                                                                55300020, 55300030,
                                                                55300040, 55301031, # French toast
                                                                58145112, 58145113, 
                                                                58145119, 58145136, # mac n cheese
                                                                58131323, # canned ravioli
                                                                58146223, 58146303,
                                                                58146323, 58146333,
                                                                58146343, 58146353,
                                                                58146363, 58146373,
                                                                58146383, 58146393,
                                                                58146403, 58146413,
                                                                58146423, 58146433,
                                                                58146443, 58146453,
                                                                58146603, 58146683, # ready pasta
                                                                71305070, 71403050, # potatoes
                                                                71405040, 71405050, # hash browns
                                                                71501007, 71501061,
                                                                71501071, 71501075, # mashed potatoes
                                                                25230210, 25230220,
                                                                25230320, 25230340,
                                                                25230420, 25230530,
                                                                25230540, 25230550,
                                                                25230560, 25230610,
                                                                25230780, 25230785,
                                                                25230800, 25231110,
                                                                25231120 # deli meat
                                                                ), labels=c("not premade","premade"))) %>% # 
                mutate(location_type = factor(location, labels = c("home","away"))) %>%
                group_by(id,meal) %>%
                  mutate(sodium_day2 = sum(sodium_content)) %>%
                    mutate(calories_day2 = sum(calorie_content))
                

person_and_meals_day1 <- day1_meals %>%
  select(id, sodium_day1,calories_day1, location_type, day_type, premade, meal) %>%
    distinct(.keep_all=TRUE) %>%
      group_by(id) %>%
        arrange(desc(premade),.by_group=TRUE) %>% # a meal with a premade ingredient is considered premade
          distinct(meal,.keep_all = TRUE)

person_and_meals_day2 <- day2_meals %>%
  select(id, sodium_day2,calories_day2, location_type, day_type, premade, meal) %>%
    distinct(.keep_all=TRUE) %>%
      group_by(id) %>%
        arrange(desc(premade),.by_group=TRUE) %>% # a meal with a premade ingredient is considered premade
          distinct(meal,.keep_all = TRUE)

person_and_meals <- inner_join(person_and_meals_day1,person_and_meals_day2,by=c("id","meal"))

# Diff in diff: 
  # Home as untreated, away as treated: away - home
      # Home then away 
      # Home then home
      # Day2 - Day1
  
  # Away as untreated, home as treated: away - home
     # Away then home
     # Away then away
     # Day1 - Day2

# Sodium at home vs away in general: 97.80213, p-value = 0.001826092

away1_away2 <- person_and_meals %>%
  filter(location_type.x == 'away') %>%
    filter(location_type.y == 'away') %>%
      ungroup() %>%
        mutate(sodium = sodium_day1 - sodium_day2) %>%
          select(sodium)

home1_home2 <- person_and_meals %>%
  filter(location_type.x == 'home') %>%
    filter(location_type.y == 'home') %>%
      ungroup() %>%
        mutate(sodium = sodium_day2 - sodium_day1) %>%
          select(sodium)

away1_home2 <- person_and_meals %>%
  filter(location_type.x == 'away') %>%
    filter(location_type.y == 'home') %>%
      ungroup() %>%
        mutate(sodium = sodium_day1 - sodium_day2) %>%
          select(sodium)

home1_away2 <- person_and_meals %>%
  filter(location_type.x == 'home') %>%
    filter(location_type.y == 'away') %>%
  ungroup() %>%
    mutate(sodium = sodium_day2 - sodium_day1) %>%
      select(sodium)

no_treatment <- rbind(home1_home2[1:length(home1_away2$sodium),], away1_away2)

treatment <- rbind(home1_away2,away1_home2[1:length(away1_away2$sodium),])

t.test(treatment$sodium,no_treatment$sodium)$estimate[1] - t.test(treatment$sodium,no_treatment$sodium)$estimate[2]

t.test(treatment$sodium,no_treatment$sodium)$p.value

# Sodium at home vs away on a weekday: 169.1601, p-value =  0.0002059149

person_and_meals_weekday <- person_and_meals %>%
  filter(day_type.x == 'weekday') %>%
    filter(day_type.y == 'weekday')

weekday_away1_away2 <- person_and_meals_weekday %>%
  filter(location_type.x == 'away') %>%
    filter(location_type.y == 'away') %>%
      ungroup() %>%
        mutate(sodium = sodium_day1 - sodium_day2) %>%
          select(sodium)

weekday_home1_home2 <- person_and_meals_weekday %>%
  filter(location_type.x == 'home') %>%
    filter(location_type.y == 'home') %>%
      ungroup() %>%
        mutate(sodium = sodium_day2 - sodium_day1) %>%
          select(sodium)

weekday_away1_home2 <- person_and_meals_weekday %>%
  filter(location_type.x == 'away') %>%
    filter(location_type.y == 'home') %>%
      ungroup() %>%
        mutate(sodium = sodium_day1 - sodium_day2) %>%
          select(sodium)

weekday_home1_away2 <- person_and_meals_weekday %>%
  filter(location_type.x == 'home') %>%
    filter(location_type.y == 'away') %>%
      ungroup() %>%
        mutate(sodium = sodium_day2 - sodium_day1) %>%
          select(sodium)

weekday_no_treatment <- rbind(weekday_home1_home2[1:length(weekday_home1_away2$sodium),], weekday_away1_away2[1:length(weekday_away1_home2$sodium),])

weekday_treatment <- rbind(weekday_home1_away2,weekday_away1_home2)

t.test(weekday_treatment$sodium,weekday_no_treatment$sodium)$estimate[1] - t.test(weekday_treatment$sodium,weekday_no_treatment$sodium)$estimate[2]

t.test(weekday_treatment$sodium,weekday_no_treatment$sodium)$p.value

# Sodium at home vs away on a weekend: -318.7407, p-value = 0.05916613

person_and_meals_weekend <- person_and_meals %>%
  filter(day_type.x == 'weekend') %>%
    filter(day_type.y == 'weekend')

weekend_away1_away2 <- person_and_meals_weekend %>%
  filter(location_type.x == 'away') %>%
    filter(location_type.y == 'away') %>%
      ungroup() %>%
        mutate(sodium = sodium_day1 - sodium_day2) %>%
          select(sodium)

weekend_home1_home2 <- person_and_meals_weekend %>%
  filter(location_type.x == 'home') %>%
    filter(location_type.y == 'home') %>%
      ungroup() %>%
        mutate(sodium = sodium_day2 - sodium_day1) %>%
          select(sodium)

weekend_away1_home2 <- person_and_meals_weekend %>%
  filter(location_type.x == 'away') %>%
    filter(location_type.y == 'home') %>%
      ungroup() %>%
        mutate(sodium = sodium_day1 - sodium_day2) %>%
          select(sodium)

weekend_home1_away2 <- person_and_meals_weekend %>%
  filter(location_type.x == 'home') %>%
    filter(location_type.y == 'away') %>%
      ungroup() %>%
        mutate(sodium = sodium_day2 - sodium_day1) %>%
          select(sodium)

weekend_no_treatment <- rbind(weekend_home1_home2[1:length(weekend_home1_away2$sodium),], weekend_away1_away2)

weekend_treatment <- rbind(weekend_home1_away2,weekend_away1_home2[1:length(weekend_away1_away2$sodium),])

t.test(weekend_treatment$sodium,weekend_no_treatment$sodium)$estimate[1] - t.test(weekend_treatment$sodium,weekend_no_treatment$sodium)$estimate[2]

t.test(weekend_treatment$sodium,weekend_no_treatment$sodium)$p.value

# Sodium at home vs away, premade: 20.66396, p-value = 0.7592822

person_and_meals_premade <- person_and_meals %>%
  filter(premade.x == 'premade') %>%
    filter(premade.y == 'premade')

premade_away1_away2 <- person_and_meals_premade %>%
  filter(location_type.x == 'away') %>%
    filter(location_type.y == 'away') %>%
      ungroup() %>%
        mutate(sodium = sodium_day1 - sodium_day2) %>%
          select(sodium)

premade_home1_home2 <- person_and_meals_premade %>%
  filter(location_type.x == 'home') %>%
    filter(location_type.y == 'home') %>%
      ungroup() %>%
        mutate(sodium = sodium_day2 - sodium_day1) %>%
          select(sodium)

premade_away1_home2 <- person_and_meals_premade %>%
  filter(location_type.x == 'away') %>%
    filter(location_type.y == 'home') %>%
      ungroup() %>%
        mutate(sodium = sodium_day1 - sodium_day2) %>%
          select(sodium)

premade_home1_away2 <- person_and_meals_premade %>%
  filter(location_type.x == 'home') %>%
    filter(location_type.y == 'away') %>%
      ungroup() %>%
        mutate(sodium = sodium_day2 - sodium_day1) %>%
          select(sodium)

premade_no_treatment <- rbind(premade_home1_home2[1:length(premade_home1_away2$sodium),], premade_away1_away2[1:length(premade_away1_home2$sodium),])

premade_treatment <- rbind(premade_home1_away2,premade_away1_home2)

t.test(premade_treatment$sodium,premade_no_treatment$sodium)$estimate[1] - t.test(premade_treatment$sodium,premade_no_treatment$sodium)$estimate[2]
t.test(premade_treatment$sodium,premade_no_treatment$sodium)$p.value

# Sodium at home vs away, not premade: -132.5703, p-value = 0.009761815

person_and_meals_notpremade <- person_and_meals %>%
  filter(premade.x == 'not premade') %>%
  filter(premade.y == 'not premade')

notpremade_away1_away2 <- person_and_meals_notpremade %>%
  filter(location_type.x == 'away') %>%
    filter(location_type.y == 'away') %>%
      ungroup() %>%
        mutate(sodium = sodium_day1 - sodium_day2) %>%
          select(sodium)

notpremade_home1_home2 <- person_and_meals_notpremade %>%
  filter(location_type.x == 'home') %>%
    filter(location_type.y == 'home') %>%
      ungroup() %>%
        mutate(sodium = sodium_day2 - sodium_day1) %>%
          select(sodium)

notpremade_away1_home2 <- person_and_meals_notpremade %>%
  filter(location_type.x == 'away') %>%
    filter(location_type.y == 'home') %>%
      ungroup() %>%
        mutate(sodium = sodium_day1 - sodium_day2) %>%
          select(sodium)

notpremade_home1_away2 <- person_and_meals_notpremade %>%
  filter(location_type.x == 'home') %>%
    filter(location_type.y == 'away') %>%
      ungroup() %>%
        mutate(sodium = sodium_day2 - sodium_day1) %>%
          select(sodium)

notpremade_no_treatment <- rbind(notpremade_home1_home2[1:length(notpremade_home1_away2$sodium),], notpremade_away1_away2)

notpremade_treatment <- rbind(notpremade_home1_away2,notpremade_away1_home2[1:length(notpremade_away1_away2$sodium),])

t.test(notpremade_treatment$sodium,notpremade_no_treatment$sodium)$estimate[1] - t.test(notpremade_treatment$sodium,notpremade_no_treatment$sodium)$estimate[2]

t.test(notpremade_treatment$sodium,notpremade_no_treatment$sodium)$p.value

# Sodium at home vs away, premade on a weekday: 104.5401, p-value = 0.3466185

person_and_meals_premade_weekday <- person_and_meals_weekday %>%
  filter(premade.x == 'premade') %>%
    filter(premade.y == 'premade')

premade_weekday_away1_away2 <- person_and_meals_premade_weekday %>%
  filter(location_type.x == 'away') %>%
    filter(location_type.y == 'away') %>%
      ungroup() %>%
        mutate(sodium = sodium_day1 - sodium_day2) %>%
          select(sodium)

premade_weekday_home1_home2 <- person_and_meals_premade_weekday %>%
  filter(location_type.x == 'home') %>%
    filter(location_type.y == 'home') %>%
      ungroup() %>%
        mutate(sodium = sodium_day2 - sodium_day1) %>%
          select(sodium)

premade_weekday_away1_home2 <- person_and_meals_premade_weekday %>%
  filter(location_type.x == 'away') %>%
    filter(location_type.y == 'home') %>%
      ungroup() %>%
        mutate(sodium = sodium_day1 - sodium_day2) %>%
          select(sodium)

premade_weekday_home1_away2 <- person_and_meals_premade_weekday %>%
  filter(location_type.x == 'home') %>%
    filter(location_type.y == 'away') %>%
    ungroup() %>%
      mutate(sodium = sodium_day2 - sodium_day1) %>%
        select(sodium)

premade_weekday_no_treatment <- rbind(premade_weekday_home1_home2[1:length(premade_weekday_home1_away2$sodium),], premade_weekday_away1_away2[1:length(premade_weekday_away1_home2$sodium),])

premade_weekday_treatment <- rbind(premade_weekday_home1_away2,premade_weekday_away1_home2)

t.test(premade_weekday_treatment$sodium,premade_weekday_no_treatment$sodium)$estimate[1] - t.test(premade_weekday_treatment$sodium,premade_weekday_no_treatment$sodium)$estimate[2]

t.test(premade_weekday_treatment$sodium,premade_weekday_no_treatment$sodium)$p.value

# Sodium at home vs away, not premade on a weekday: -61.82437, p-value = 0.3789736

person_and_meals_notpremade_weekday <- person_and_meals_weekday %>%
  filter(premade.x == 'not premade') %>%
    filter(premade.y == 'not premade')

notpremade_weekday_away1_away2 <- person_and_meals_notpremade_weekday %>%
  filter(location_type.x == 'away') %>%
    filter(location_type.y == 'away') %>%
      ungroup() %>%
        mutate(sodium = sodium_day1 - sodium_day2) %>%
          select(sodium)

notpremade_weekday_home1_home2 <- person_and_meals_notpremade_weekday %>%
  filter(location_type.x == 'home') %>%
    filter(location_type.y == 'home') %>%
      ungroup() %>%
        mutate(sodium = sodium_day2 - sodium_day1) %>%
          select(sodium)

notpremade_weekday_away1_home2 <- person_and_meals_notpremade_weekday %>%
  filter(location_type.x == 'away') %>%
    filter(location_type.y == 'home') %>%
      ungroup() %>%
        mutate(sodium = sodium_day1 - sodium_day2) %>%
          select(sodium)

notpremade_weekday_home1_away2 <- person_and_meals_notpremade_weekday %>%
  filter(location_type.x == 'home') %>%
    filter(location_type.y == 'away') %>%
      ungroup() %>%
        mutate(sodium = sodium_day2 - sodium_day1) %>%
  select(sodium)

notpremade_weekday_no_treatment <- rbind(notpremade_weekday_home1_home2[1:length(notpremade_weekday_home1_away2$sodium),], notpremade_weekday_away1_away2[1:length(notpremade_weekday_away1_home2$sodium),])

notpremade_weekday_treatment <- rbind(notpremade_weekday_home1_away2,notpremade_weekday_away1_home2)

t.test(notpremade_weekday_treatment$sodium,notpremade_weekday_no_treatment$sodium)$estimate[1] - t.test(notpremade_weekday_treatment$sodium,notpremade_weekday_no_treatment$sodium)$estimate[2]

t.test(notpremade_weekday_treatment$sodium,notpremade_weekday_no_treatment$sodium)$p.value

# Sodium at home vs away, premade on a weekend: -52.875, p-value = 0.8654871

person_and_meals_premade_weekend <- person_and_meals_weekend %>%
  filter(premade.x == 'premade') %>%
    filter(premade.y == 'premade')

premade_weekend_away1_away2 <- person_and_meals_premade_weekend %>%
  filter(location_type.x == 'away') %>%
    filter(location_type.y == 'away') %>%
      ungroup() %>%
        mutate(sodium = sodium_day1 - sodium_day2) %>%
          select(sodium)

premade_weekend_home1_home2 <- person_and_meals_premade_weekend %>%
  filter(location_type.x == 'home') %>%
    filter(location_type.y == 'home') %>%
      ungroup() %>%
        mutate(sodium = sodium_day2 - sodium_day1) %>%
          select(sodium)

premade_weekend_away1_home2 <- person_and_meals_premade_weekend %>%
  filter(location_type.x == 'away') %>%
    filter(location_type.y == 'home') %>%
      ungroup() %>%
        mutate(sodium = sodium_day1 - sodium_day2) %>%
          select(sodium)

premade_weekend_home1_away2 <- person_and_meals_premade_weekend %>%
  filter(location_type.x == 'home') %>%
    filter(location_type.y == 'away') %>%
      ungroup() %>%
        mutate(sodium = sodium_day2 - sodium_day1) %>%
          select(sodium)

premade_weekend_no_treatment <- rbind(premade_weekend_home1_home2[1:length(premade_weekend_home1_away2$sodium),], premade_weekend_away1_away2)

premade_weekend_treatment <- rbind(premade_weekend_home1_away2,premade_weekend_away1_home2[1:length(premade_weekend_away1_away2$sodium),])

t.test(premade_weekend_treatment$sodium,premade_weekend_no_treatment$sodium)$estimate[1] - t.test(premade_weekend_treatment$sodium,premade_weekend_no_treatment$sodium)$estimate[2]

t.test(premade_weekend_treatment$sodium,premade_weekend_no_treatment$sodium)$p.value

# Sodium at home vs away, not premade on a weekend: -644.0588, p-value = 0.05015625

person_and_meals_notpremade_weekend <- person_and_meals_weekend %>%
  filter(premade.x == 'not premade') %>%
    filter(premade.y == 'not premade')

notpremade_weekend_away1_away2 <- person_and_meals_notpremade_weekend %>%
  filter(location_type.x == 'away') %>%
    filter(location_type.y == 'away') %>%
      ungroup() %>%
        mutate(sodium = sodium_day1 - sodium_day2) %>%
          select(sodium)

notpremade_weekend_home1_home2 <- person_and_meals_notpremade_weekend %>%
  filter(location_type.x == 'home') %>%
    filter(location_type.y == 'home') %>%
      ungroup() %>%
        mutate(sodium = sodium_day2 - sodium_day1) %>%
          select(sodium)

notpremade_weekend_away1_home2 <- person_and_meals_notpremade_weekend %>%
  filter(location_type.x == 'away') %>%
    filter(location_type.y == 'home') %>%
      ungroup() %>%
        mutate(sodium = sodium_day1 - sodium_day2) %>%
          select(sodium)

notpremade_weekend_home1_away2 <- person_and_meals_notpremade_weekend %>%
  filter(location_type.x == 'home') %>%
    filter(location_type.y == 'away') %>%
      ungroup() %>%
        mutate(sodium = sodium_day2 - sodium_day1) %>%
          select(sodium)

notpremade_weekend_no_treatment <- rbind(notpremade_weekend_home1_home2[1:length(notpremade_weekend_home1_away2$sodium),], notpremade_weekend_away1_away2)

notpremade_weekend_treatment <- rbind(notpremade_weekend_home1_away2,notpremade_weekday_away1_home2[1:length(notpremade_weekend_away1_away2$sodium),])

t.test(notpremade_weekend_treatment$sodium,notpremade_weekend_no_treatment$sodium)$estimate[1] - t.test(notpremade_weekend_treatment$sodium,notpremade_weekend_no_treatment$sodium)$estimate[2]

t.test(notpremade_weekend_treatment$sodium,notpremade_weekend_no_treatment$sodium)$p.value
