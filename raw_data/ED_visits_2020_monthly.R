library(janitor)
library(tidyverse)
library(lubridate)
library(yaml)
library(dplyr)

## Import raw data from CM for 2020 ED visits
ed_2020 <- read_csv("raw_data/ED_visits_2020.csv") %>% 
  clean_names()

ed_2020 %>% 
  mutate(visit=1) %>% 
  mutate(admit_date = mdy_hm(admit_date)) %>% 
  mutate(month = month(admit_date, label = TRUE, abbr = FALSE)) %>% 
  group_by(month) %>% 
  count(visit) %>% 
  pull(month)

## Drop identifying information
ed_2020.hippa <- 
  ed_2020 %>% 
  select(-first_name, -last_name, -date_of_birth)



ed_2020_long <- ed_2020.hippa %>% 
  mutate(visit=1)



ed_2020_long_2 <- ed_2020_long %>% 
  separate(col=admit_date,
           into=c("month","day","year"),
           sep="/") 

ed_2020_long_2 %>% 
      group_by(month) %>% 
      count(visit)
      
## CREATE HIGH UTILIZATION VARIABLE
ed_visits_by_month_group <- ed_visits_by_month %>% 
  mutate(past_year_ED_five_plus = ifelse(x12_month_ed_visit_count>=5, 1,0)) %>% 
    tabyl(past_year_ED_five_plus)
  
   
  