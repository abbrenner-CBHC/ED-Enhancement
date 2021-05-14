
# Load packages -----------------------------------------------------------

library(tidyverse)
library(janitor)
library(readxl)
library(lubridate)



# Upload data -------------------------------------------------------------

ed_encounter <- read_xlsx("raw_data/ytd-monthly-ed-05-01-2021.xlsx",
                         sheet="ytd_ed_visits", col_names=TRUE) %>% 
               clean_names()

ed_cbhc_tyd <- read_xlsx("raw_data/credible_for_ed_ytd_05_10_2021.xlsx",
                         sheet="Data", col_names=TRUE) %>% 
              clean_names()


prescott_march_wide <- read_excel(path= "clean_data/ed-prescott-march-wide.xls",
                                  sheet="Sheet1",
                                  col_names = TRUE) %>% 
  clean_names()


# Data cleaning, emergency department encounters, YTD -----------------------------------------------------


ed_encounter_clean <- ed_encounter %>% 
  select(-c(first_name, last_name, date_of_birth, discharge_date)) %>% 
      rename(id=member_id, past_month_visit_count=x1_month_ed_visit_count,
           past_3_mo_visit_count=x3_month_ed_visit_count,
           past_6_mo_visit_count=x6_month_ed_visit_count,
           past_year_visit_count=x12_month_ed_visit_count) %>% 
  mutate(bh_cohort = case_when(
          bh_and_diabetes == "BH and Diabetes" ~ 1,
          TRUE ~ 0)) %>% 
  mutate(ed_disparity_cohort = case_when(
          oregon_ed_disparity_measure == "Oregon ED Disparity Measure" ~ 1,
          TRUE ~ 0)) %>% 
  mutate(admit_year = year(admit_date),
         admit_month = month(admit_date),
         admit_hour = hour(admit_date)) %>% 
  mutate(day_admit = case_when(
        admit_hour >=7 & admit_hour <= 18 ~ 1,
        TRUE ~ 0),
        night_admit = case_when(
          admit_hour > 18 & admit_hour <= 24 ~ 1,
          TRUE ~ 0),
        early_morning_admit = case_when(
          admit_hour >= 0 & admit_hour < 7 ~ 1,
          TRUE ~ 0)
        ) %>% 
  # CREATE FLAGS FOR BH ED DISCHARGE DX AND PH ED DISCHARGE DX
  mutate(bh_ed_dx = str_detect(diagnoses, "F[0-9]")) %>% 
  mutate(bh_ed_dx = as.integer(bh_ed_dx)) %>% 
  mutate(bh_ed_dx=case_when(
    str_detect(diagnoses, c("[Ss]uicide","[Ss][Ii]")) ~ 1,
    TRUE ~ 0)) %>% 
  mutate(ph_ed_dx = case_when(
    bh_ed_dx==0 ~ 1,
    TRUE ~ 0
  )) %>% 
# Create high utilization categories
  group_by(id) %>% 
     mutate(ed_past_yr_5_to_9 = ifelse(past_year_visit_count>4 & past_year_visit_count<10, 1, 0),
         ed_past_yr_10_plus = ifelse(past_year_visit_count>=10, 1, 0),
         ed_past_mo_2_plus = ifelse(past_month_visit_count>1, 1, 0)) %>% 
  ungroup() 

visit_type <- ed_encounter_clean %>% 
  group_by(id) %>% 
  summarise(total_bh_visits = sum(bh_ed_dx),  
            total_ph_visits = sum(ph_ed_dx)) %>% 
  ungroup()

ed_encounter_clean_2 <- ed_encounter_clean %>% 
  left_join(visit_type, by="id") %>% 
  
ed_encounter_clean_2 %>% 
  arrange(id) %>% 
# create person-level variable to represent percent of their total visits ytd that were
# for a bh concern
   mutate(pct_bh_ed_visits = (total_bh_visits/(total_bh_visits + total_ph_visits))*100) %>% 
      select(c(id,bh_ed_dx,ph_ed_dx,total_bh_visits,total_ph_visits,pct_bh_ed_visits))


### Basic data cleaning, CBHC client-level data, pulled May 1, 2021

ed_cbhc_ytd_clean <- ed_cbhc_tyd %>% 
  select(-c(last_name, client_status_date, all_dx_icd, other_admits_past_year, sex_assigned_at_birth, est_mnthly_grs_hshld_incm, hepatitis_b_status, bmi:bp)) %>% 
  rename(id=client_id) 

ed_cbhc_ytd_clean <- ed_cbhc_ytd_clean %>% 
  arrange(primary_prog) %>% 
  filter(primary_prog !="Intake"|primary_prog !="Crisis"| primary_prog !="Gambling"|primary_prog!="NonMOTS"|primary_prog!="Respite"|primary_prog!="Turning Point"|!is.na(primary_prog))

# Output clean datasets ---------------------------------------------------


# CREATE FINAL ED ENCOUNTER DATASET
ed_ytd_05_01_2021_long <- ed_encounter_clean_2 %>% 
    select(-(c(oregon_ed_disparity_measure,bh_and_diabetes,admit_year)))

write_rds(ed_ytd_05_01_2021_long,
          file = "clean_data/ed_ytd_05_01_2021_long")
  
  



  


  
  


## PRESCOTT - PLOTTING

prescott_dx <- prescott_march_wide %>% 
  select(member_id,primary_dx_desc, past_12_mo_5_9, past_12_mo_10_plus, past_month_ed_2_plus, bh_dm) %>%
  filter(primary_dx_desc %in% c("Reaction to severe stress, and adjustment disorders","Schizoaffective disorders","Unspecified psychosis not due to a substance or known physiological condition
","Recurrent depressive disorder","Schizophrenia","Bipolar affective disorder"))



prescott_dx %>% 
  pivot_longer(cols = !(c("member_id","primary_dx_desc")),
               names_to = "ed_visit_type", "percent_of_clients") %>% 
  rename("flag"="value") %>% 
  ggplot(mapping=aes(x = primary_dx_desc,
                     y = flag,
                     fill=primary_dx_desc)) +
  geom_col() +
  coord_flip()
  #scale_color_brewer(palette = "Sequential") +
  labs(x="Number of Clients", 
       y="") +
  facet_wrap(~ed_visit_type)
