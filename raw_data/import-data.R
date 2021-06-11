
# Load packages -----------------------------------------------------------

library(tidyverse)
library(janitor)
library(readxl)
library(lubridate)
library(icd.data)
library(ICD10gm)



# Upload data -------------------------------------------------------------

ed_encounter <- read_xlsx("raw_data/ytd-monthly-ed-06-01-2021.xlsx",
                         sheet="ytd_ed_visits",
                         col_names=TRUE) %>% 
               clean_names()

ed_cbhc_tyd <- read_xlsx("raw_data/credible_for_ed_ytd_05_10_2021.xlsx",
                         sheet="Data", 
                         col_names=TRUE) %>% 
              clean_names()




# Data cleaning, emergency department encounters, YTD -----------------------------------------------------

ed_encounter_clean <- ed_encounter %>% 
  arrange(by_group = admit_date) %>% 
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


# Generate a total of behavioral health and physical health visits  per person
visit_type_by_id <- ed_encounter_clean %>% 
  group_by(id) %>% 
  summarise(total_bh_visits_per_person = sum(bh_ed_dx),  
            total_ph_visits_per_person = sum(ph_ed_dx)) %>% 
  ungroup()


ed_encounter_clean_2 <- ed_encounter_clean %>% 
  left_join(visit_type_by_id, by="id") %>%
  arrange(id) %>% 
# create person-level variable to represent percent of their total visits ytd that were
# for a bh concern
   mutate(pct_bh_ed_visits = (total_bh_visits_per_person/(total_bh_visits_per_person + total_ph_visits_per_person))*100) %>% 
      select(-c(oregon_ed_disparity_measure, bh_and_diabetes, admit_year))


# Generate a total of behavioral health and physical health visits  per month
visit_type_by_month <- ed_encounter_clean %>% 
  group_by(admit_month) %>% 
  summarise(total_bh_visits_per_month = sum(bh_ed_dx),  
            total_ph_visits_per_month = sum(ph_ed_dx)) %>% 
  ungroup()



# Merging with client-level data ------------------------------------------
ed_encounter_by_client <- ed_encounter_clean_2 %>% 
  full_join(ed_cbhc_tyd, by=c("id"="client_id"))  
# Perform a full join to keep all rows. Assume that clients in active
# client list from CBHC are those with zero past year ED visits in CM


# Data cleaning and management of full data file --------------------------

ed_encounter_by_client <- ed_encounter_by_client %>% 
  select(-c(last_name, client_status_date, all_dx_icd, other_admits_past_year, sex_assigned_at_birth, est_mnthly_grs_hshld_incm, hepatitis_b_status, bmi:bp)) %>% 
  mutate(
    past_year_visit_count = case_when(
    is.na(past_year_visit_count) ~ 0,
    TRUE ~ past_year_visit_count),
  past_month_visit_count = case_when(
    is.na(past_month_visit_count) ~ 0,
   TRUE ~ past_month_visit_count),
  past_6_mo_visit_count = case_when(
   is.na(past_6_mo_visit_count) ~ 0,
   TRUE ~ past_6_mo_visit_count),
  past_3_mo_visit_count = case_when(
    is.na(past_3_mo_visit_count) ~ 0,
    TRUE ~ past_3_mo_visit_count)) %>% 
  arrange(primary_prog) %>% 
  filter(primary_prog == "ACT"|primary_prog == "Mental Health Outpatient"| primary_prog== "Mental Health Community Solutions"| primary_prog== "Mental Health Residential Srv"|primary_prog== "PSRB Treatment"|primary_prog== "Residential Facility"|primary_prog== "SUD") %>% 
  mutate(primary_prog = case_when(
    primary_prog == "Mental Health Residential Srv" ~ "Residential",
    primary_prog == "Residential Facility" ~ "Residential",
    TRUE ~ primary_prog)) %>% 
  mutate(
    fact = case_when(
      primary_prog == "ACT" & str_detect(geo_desc,"Forensic") ~ 1,
      TRUE ~ 0),
    act_not_fact = case_when(
      primary_prog == "ACT" & fact!=1 ~ 1,
      TRUE ~ 0),
    icm = case_when(
      str_detect(geo_desc,"Intensive") ~ 1,
      TRUE ~ 0),
    comm_sol_hc = case_when(
      primary_prog == "Mental Health Community Solutions" & (str_detect(geo_desc, "Clinic")) ~ 1,
      TRUE ~ 0),
    comm_sol_comm = case_when(
      primary_prog == "Mental Health Community Solutions" & comm_sol_hc !=1 ~ 1,
      TRUE ~ 0),
    pc = case_when(
      !is.na(epic_id) ~ 1,
      TRUE ~ 0),
    older_adults = case_when(
      str_detect(geo_desc, "Older") ~ 1,
      TRUE ~ 0),
    child_fam = case_when(
      str_detect(geo_desc, "Child") ~ 1,
      TRUE ~ 0),
    prescott = case_when(
      str_detect(geo_desc, "Prescott") ~ 1,
      TRUE ~ 0),
    davids_harp = case_when(
      str_detect(geo_desc, "Harp") ~ 1,
      TRUE ~ 0),
    employment = case_when(
      str_detect(geo_desc, "Employment") ~ 1,
      TRUE ~ 0),
    hot = case_when(
      str_detect(geo_desc, "Housing|HUD|HOT|RADCREW|OTIS|Palm|JOIN|PSH") ~ 1,
      TRUE ~ 0),
    sot = case_when(
      str_detect(geo_desc, "Street|ISEP|PATH") ~ 1,
      TRUE ~ 0),
    justice = case_when(
      str_detect(geo_desc, "Justice|Second") ~ 1,
      TRUE ~ 0)) 


ed_encounter_by_client_recoded <- ed_encounter_by_client %>%
  mutate(
    homeless = case_when(
      living_arrangement == "Transient/Homeless" ~ 1,
      TRUE ~ 0),
    gender_simple = case_when(
      gender=="Male" ~ "male",
      gender=="Female" ~ "female",
      gender=="Transgender Female/Male-to-Female" ~ "trans_female",
      gender=="Transgender Male/Female-to-Male" ~ "trans_male",
      is.na(gender) ~ "missing",
      TRUE ~ "other_non_binary_gender"),
    sexual_orien_simple = case_when(
      sexual_orientation == "Straight"|sexual_orientation == "Straight/heterosexual" ~ "straight",
      sexual_orientation == "Chose not to disclose"| sexual_orientation == "Don''t know" ~ "missing_unknown",
      is.na(sexual_orientation) ~ "missing_unknown",
      TRUE ~ "not_straight"),
    eng_speaking = case_when(
      pref_language == "English" ~ "english",
      is.na(pref_language) ~ "missing",
      TRUE ~ "non_english"),
    mh_loc_simple = case_when(
            str_detect(mh_loc, "Level A") ~ "level_a",
            str_detect(mh_loc, "Level B Adult Outpatient|Level B Child and Family") ~ "level_b",
            str_detect(mh_loc, "Level B Adult SPMI") ~ "level_b_spmi",
            str_detect(mh_loc, "Level C Adult SPMI") ~ "level_c_spmi",
            str_detect(mh_loc, "Level C Adult Outpatient|Level C Child and Family") ~ "level_c",
            str_detect(mh_loc, "Level D") ~ "level_d",
            is.na(mh_loc) ~ "missing"),
    tobacco_user = case_when(
      tobacco == "YES" ~ "tobacco_user",
      is.na(tobacco) ~ "missing",
      TRUE ~ "never_former_tobacco_user")
    )
    
    
# GENERATE LIST OF TOP 10 PRIMARY DX
primary_dx <- ed_encounter_by_client_recoded %>% 
  select(id, primary_dx) %>%
  distinct() %>% 
  add_count(primary_dx) %>%
  distinct(primary_dx, .keep_all=TRUE) %>% 
  arrange(desc(by_group=n))

  
  
  
# Output clean datasets ---------------------------------------------------


# CREATE FINAL ED ENCOUNTER DATASET

write_rds(ed_encounter_by_client_recoded,
          file = "clean_data/ed_client_ytd_06_01_2021_long")
  
write_rds(primary_dx,
          file = "clean_data/primary_dx_counts")

write_rds(visit_type_by_month,
          file = "clean_data/visit_type_by_month")





