
library(tidyverse)
library(lubridate)
library(glue)

source("R/list_function.R")

# Load data

df_dfa_data <- readxl::read_excel("inputs/UGA2103_Financial_Service_Providers_Assessment_HH_Tool_June2021.xlsx") %>% 
  mutate(m.uuid = '_uuid',
         m.start_date = as_date(start),
         m.enumerator_id = enumerator_id,
         m.district_name = district_name,
         m.point_number = point_number,
         start = as_datetime(start),
         end = as_datetime(end)
         
  )

# Load survey questions sheet
df_dfa_survey <- readxl::read_excel("inputs/UGA2103_Digital_Finace_HH_Tool_June2021.xlsx", sheet = "survey")

# Load survey choices sheet
df_dfa_choices <-  readxl::read_excel("inputs/UGA2103_Digital_Finace_HH_Tool_June2021.xlsx", sheet = "choices")

# Logical flow

logical_output <- list()

# Data not meeting minimal requirements---------------------------------------

# no consent surveys
df_no_consent <- df_dfa_data %>% 
  filter(consent == "no") %>% 
  mutate(m.type = "remove_survey",
         m.name = "consent",
         m.current_value = consent,
         m.value = "",
         m.issue_id = "logic_m_requirement_no_consent",
         m.issue = "no_consent",
         m.other_text = "",
         m.checked_by = "",
         m.checked_date = as_date(today()),
         m.comment = "", 
         m.reviewed = "1",
         m.adjust_log = "",
         m.uuid_cl = "",
         m.so_sm_choices = "") %>% 
  dplyr::select(starts_with("m.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "m.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_no_consent")

# age below 18
df_repondent_below_age <-  df_dfa_data %>% 
  filter(respondent_age > 18) %>% 
  mutate(m.type = "remove_survey",
          m.name = "respondent_age",
          m.current_value = as.character(respondent_age),
          m.value = "",
          m.issue_id = "logic_m_requirement_respondent_below_age",
          m.issue = "below_age",
          m.other_text = "",
          m.checked_by = "",
          m.checked_date = as_date(today()),
          m.comment = "", 
          m.reviewed = "1",
          m.adjust_log = "",
          m.uuid_cl = "",
          m.so_sm_choices = "") %>% 
    dplyr::select(starts_with("m.")) %>% 
    rename_with(~str_replace(string = .x, pattern = "m.", replacement = ""))
         
    add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_repondent_below_age")
    

# data testing
df_data_tesing <- df_dfa_data %>% 
  filter(m.start_date < as_date("2021-08-28") | str_detect(string = point_number, pattern = fixed('test', ignore_case = TRUE))) %>% 
  mutate(m.type = "remove_survey",
         m.name = "",
         m.current_value = "",
         m.value = "",
         m.issue_id = "logic_m_ignoring_tesitng_data",
         m.issue = "testing_data",
         m.other_text = "",
         m.checked_by = "",
         m.checked_date = as_date(today()),
         m.comment = "", 
         m.reviewed = "1",
         m.adjust_log = "",
         m.uuid_cl = "",
         m.so_sm_choices = "") %>% 
  dplyr::select(starts_with("m.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "m.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_data_tesing")


# time checks -------------------------------------------------------------

# Time taken to complete survey


min_survey_time <- 30
Max_survey_time <- 120

df_child_survey_duration <- df_dfa_survey %>% 
  mutate(m.survey_duration = )




















