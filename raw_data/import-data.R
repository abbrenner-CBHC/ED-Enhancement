
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


