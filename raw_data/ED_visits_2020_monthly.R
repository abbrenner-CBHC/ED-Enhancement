library(janitor)
library(tidyverse)
library(lubridate)
library(yaml)
library(dplyr)



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
  
   
  