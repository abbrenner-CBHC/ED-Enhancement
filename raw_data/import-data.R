
# Load packages -----------------------------------------------------------

library(tidyverse)
library(janitor)
library(readxl)
library(lubridate)



# Upload data -------------------------------------------------------------

ed_encounter <- read_xlsx("raw_data/YTD_ED_Visits_Monthly_Report.xlsx",
                         sheet="Data", col_names=TRUE) %>% 
               clean_names()

ed_cbhc_tyd <- read_xlsx("raw_data/credible_for_ed_ytd_04_28_2020.xlsx",
                         sheet="Data", col_names=TRUE) %>% 
              clean_names()


prescott_march_wide <- read_excel(path= "clean_data/ed-prescott-march-wide.xls",
                                  sheet="Sheet1",
                                  col_names = TRUE) %>% 
  clean_names()


# Basic data cleaning, emergency department encounters, YTD -----------------------------------------------------


ed_encounter %>% 
  select(bh_and_diabetes, oregon_ed_disparity_measure) %>% 
  mutate(bh_cohort = case_when(bh_and_diabetes == "BH and Diabetes" ~ TRUE,
                               TRUE ~ FALSE))


ed_encounter_clean <- ed_encounter %>% 
  select(-c(first_name, last_name, date_of_birth, discharge_date)) %>% 
  rename(id=member_id, past_month_visit_count=x1_month_ed_visit_count,
         past_3_mo_visit_count=x3_month_ed_visit_count,
         past_6_mo_visit_count=x6_month_ed_visit_count,
         past_year_visit_count=x12_month_ed_visit_count) %>% 
  mutate(bh_cohort=if_else(bh_and_diabetes=="BH and Diabetes", "1","0")) %>% 
  mutate(ed_disparity_cohort=if_else(oregon_ed_disparity_measure=="Oregon ED Disparity Measure", "1","0")) %>% 
 # as.numeric(c("bh_cohort","ed_disparity_cohort")) %>% 
  select(-c(oregon_ed_disparity_measure,bh_and_diabetes))
  
  


  # Note: I am having trouble changing the bh_cohort and ed_disparity_cohort variable to numeric type




# Basic data cleaning, CBHC client-level data, pulled April 2021 ----------
  
head(ed_cbhc_tyd)

ed_cbhc_tyd_clean <- ed_cbhc_tyd %>% 
  select(-c(last_name, other_admits_past_year, sex_assigned_at_birth,hepatitis_b_status, bmi:bp)) %>% 
  rename(id=client_id) 

  

# More complex Data management ---------------------------------------------------------


# Create a total count of daytime and after-hours ED admits.

ed_encounter_dates<- ed_encounter_clean %>% 
  mutate(admit_year =year(admit_date),
         admit_month = month(admit_date),
         admit_hour = hour(admit_date),
         day_admit=ifelse(admit_hour>=8 & admit_hour<=16, 1, 0), 
         after_hours_admit=ifelse(admit_hour>16, 1,0))


       
## NOTE: I can't get this count (creating a total per client) to work

# Create count of ED admits for each person
ed_encounter_dates %>% 
  select(id, day_admit) %>% 
  group_by(id) %>% 
  add_count(day_admit, name = "total_per_client")
  


# This was in your updated file, but client_ed_visit_total doesn't appear
# elsewhere in your script 
  # add_count(day_admit, name = client_ed_visit_total)

# Create high utilization categories
ed_encounter_dates <- ed_encounter_dates %>% 
  group_by(id) %>% 
  mutate(ed_past_yr_5_to_9 = ifelse(past_year_visit_count>4 & past_year_visit_count<10, 1, 0),
         ed_past_yr_10_plus = ifelse(past_year_visit_count>=10, 1, 0),
         ed_past_mo_2_plus = ifelse(past_month_visit_count>1, 1, 0)) %>% 
  #select(id,past_year_visit_count,past_month_visit_count,ed_past_yr_5_to_9,ed_past_yr_10_plus,ed_past_mo_2_plus) %>% 
  ungroup()

# Create more meaningful Dx variables

ed_encounter_dates <- ed_encounter_dates %>% 
  separate_rows(diagnoses, sep=";")
  

  
  
  ## Still need to create some variables: ph_dx, bh_dx, key_dx_codes, 


# Create a dataset for person-level ED summary data

ed_person_summary_data >- ed_encounter_clean %>% 
  select()


## PRESCOTT - PLOTTING

prescott_dx <- prescott_march_wide %>% 
  select(member_id,primary_dx_desc, past_12_mo_5_9, past_12_mo_10_plus, past_month_ed_2_plus, bh_dm) %>%
  filter(primary_dx_desc=="Reaction to severe stress, and adjustment disorders"|primary_dx_desc=="Schizoaffective disorders"|primary_dx_desc=="Unspecified psychosis not due to a substance or known physiological condition
"|primary_dx_desc=="Recurrent depressive disorder"|primary_dx_desc=="Schizophrenia"|primary_dx_desc=="Bipolar affective disorder") 


# I am trying to graph a count of clients by each of the three ed utilization groups (2+ times in past month,
# 5-9 times in past year, 10+ times in past year, those with behavioral health Dx and diabetes)
# I thought if I used pivot_longer to create a ed type variable, and then used facet wrap to show the count by diagnosis for each
# utilization category, it might give me a nice way to compare. But it's not working and I'm not sure where to
# go from here


prescott_dx_long <- prescott_dx %>% 
  pivot_longer(cols = !(c("member_id","primary_dx_desc")),
               names_to = "ed_visit_type", "percent_of_clients") %>% 
  rename("flag"="value") %>% 
  ggplot(mapping=aes(x="primary_dx_desc",
                     y=flag)) +
  geom_col()+
  labs(y="Number of Clients",
       x="Diagnosis")+
  facet_wrap(~vars(ed_visit_type))
