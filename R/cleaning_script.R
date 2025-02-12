# data cleaning script

# load libraries
library(tidyverse)
library(lubridate)

# blank variables to make respondents' identity anonymous 

variables_to_remove_from_data <- c("deviceid", "respondent_telephone", "geopoint", "_geopoint_latitude",  "_geopoint_longitude", "_geopoint_altitude",  "_geopoint_precision")

# read data
## read checking log

dfa_cleaning_log <- read_csv("inputs/combined_logic_spatial_and_others_checks.csv") %>% 
  mutate(adjust_log = ifelse(is.na(adjust_log), "apply_suggested_change", adjust_log)) %>% 
  filter(adjust_log != "delete_log", !is.na(value), !is.na(uuid)) %>%
  mutate(sheet = NA, index = NA, relevant = NA) %>% 
  select(uuid, type, name, value, issue_id, sheet, index, relevant)

dfa_data_nms <- names(readxl::read_excel(path = "inputs/UGA2103_Financial_Service_Providers_Assessment_HH_Tool_June2021.xlsx", n_max = 0))
c_types <- ifelse(str_detect(string = dfa_data_nms, pattern = "_other$"), "text", "guess")

dfa_raw_data <- readxl::read_excel(path = "inputs/UGA2103_Financial_Service_Providers_Assessment_HH_Tool_June2021.xlsx", col_types = c_types) %>% 
  filter(consent == "yes", respondent_age >=18,
         as_date(start) > as_date("2021-08-29"),
         point_number != "13m.",
         !as_date(start) %in% c(as_date("2021-09-08"), as_date("2021-09-09"), as_date("2021-09-21")),
         !str_detect(string = point_number, pattern = fixed('test', ignore_case = TRUE)),
         !`_uuid` %in% c(ng = point_number, pattern = fixed('test', ignore_case = TRUE)),
         !`_uuid` %in% c("27b0ffe2-8d47-4897-b402-1928fd23cfb3",
                         "40d216de-76db-42b7-9105-aea1ce234489",
                         "f2f648df-55d6-4b9d-93ca-aa87c3bc30c7",
                         "4167b891-b1ff-46b1-856b-532dd28e7a1e",
                         "d7bde578-cdc2-4d77-b31b-a15c4cec9d38",
                         "4f3afa4f-a065-4f6d-bd25-9919902f9ce0",
                         "a89d0010-d626-4a4f-8b8c-e83e8fade349",
                         "27ac9f75-3954-4c55-90a3-b5b09984fe7a",
                         "48ee8073-a0d7-460f-9867-304a47bdb1fc",
                         "b0a1c83dcdb24671b9eed78d7a77786f",
                         "c5c9b13aa6cd43338e113d8c647c04ed",
                         "d8936d35-7e1c-48d9-bafa-b25e758c05eb" )) %>% 
  mutate(current_receive_cash = reduce2(.x = c("\\bSCI\\b", "\\bWTI\\b"), .y = c("sci", "wti"), .init = current_receive_cash, .f = str_replace),
         `current_receive_cash` = ifelse(`current_receive_cash/SCI` == 1 & is.na(`current_receive_cash/sci`), `current_receive_cash/SCI`, `current_receive_cash/sci`),
         `current_receive_cash/wti` = ifelse(`current_receive_cash/WTI` == 1 & is.na(`current_receive_cash/wti`), `current_receive_cash/WTI`, `current_receive_cash/wti`),
         `current_receive_cash/wti` = ifelse(`current_receive_cash/WTI` == 1 & is.na(`current_receive_cash/wti`), `current_receive_cash/WTI`, `current_receive_cash/wti`),
         bank_acc_help_desk = ifelse(str_detect(string = bank_acc_help_desk, pattern = "bank_with_ac"), str_replace(string = bank_acc_help_desk, pattern = "bank_with_ac", replacement = "bank_with_acc"), bank_acc_help_desk),
         `bank_acc_help_desk/bank_with_acc` = ifelse(!is.na(`bank_acc_help_desk/bank_with_ac`) & is.na(`bank_acc_help_desk/bank_with_acc`), `bank_acc_help_desk/bank_with_ac`, `bank_acc_help_desk/bank_with_acc`)) %>% 
  select(-c(`id_type_refugee/school_ID`, `current_receive_cash/SCI`, `current_receive_cash/WTI`, `bank_acc_help_desk/bank_with_ac`), -c(`id_type`, `id_type_other`), -contains("id_type/")) %>% 
  mutate(across(.cols = everything(), .fns = ~ifelse(str_detect(string = ., pattern = fixed(pattern = "N/A", ignore_case = TRUE)), "NA", .)))

dfa_survey <- readxl::read_excel("inputs/UGA2103_Digital_Finace_HH_Tool_June2021.xlsx", sheet = "survey")
dfa_choices <- readxl::read_excel("inputs/UGA2103_Digital_Finace_HH_Tool_June2021.xlsx", sheet = "choices")


# find all new choices created to add to choices sheet --------------------

# gather choice options based on unique choices list

dfa_grouped_choices <- dfa_choices %>% 
  group_by(list_name) %>% 
  summarise(choice_options = paste(name, collapse = " : "))

# get new name and ad_option pairs to add to the choices sheet
new_vars <- dfa_cleaning_log %>% 
  filter(type %in% c("change_response", "add_option")) %>% 
  left_join(dfa_survey, by = "name") %>% 
  filter(str_detect(string = type.y, pattern = "select_one|select one|select_multiple|select multiple")) %>% 
  separate(col = type.y, into = c("select_type", "list_name"), sep = " ", remove = TRUE, extra = "drop") %>% 
  left_join(dfa_grouped_choices, by ="list_name") %>% 
  filter(!str_detect(string = choice_options, pattern = value)) %>% 
  rename(choice = value) %>% 
  select(name, choice) %>% 
  filter(!paste0(name, "/",choice) %in% c("bank_acc_help_desk/bank_agent", 
                                          "card_feedback/yes_agents",
                                          "cash_feedback/yes_agents",
                                          "mm_feedback/police",
                                          "mm_feedback/yes_agents",
                                          "mm_limitations/conmen",
                                          "reason_want_cash_aid/other_sys_unsafe",
                                          "use_mm_acc_for/keep_money_safest")) %>% 
  # ignore choice options removed from survey but still in the cleaning log
  distinct() %>% # to make sure there are no duplicates
  arrange(name)


# create kobold object ----------------------------------------------------

kbo_data <- kobold::kobold(survey = dfa_survey,
                                 choices = dfa_choices,
                                 data = dfa_raw_data,
                                 cleaning = dfa_cleaning_log)


# modified choices for the survey tool ------------------------------------

dfa_choices_modified <- butteR:::xlsform_add_choices(kobold = kbo_data, new_choices = new_vars)

# special treat for variables for select_multiple, we need to add the columns to the data itself
dfa_survey_sm <- dfa_survey %>% 
  mutate(qn_type = case_when(str_detect(string = type, pattern = "select_multiple|select multiple") ~ "sm",
                             str_detect(string = type, pattern = "select_one|select one") ~ "so",
                             TRUE ~ type)) %>% 
  select(name, qn_type)


# construct new columns for select multiple
new_vars_sm <- new_vars %>% 
  left_join(dfa_survey_sm, by = "name") %>% 
  filter(qn_type == "sm") %>% 
  mutate(new_cols = paste0(name, "/", choice))


# add new columns to raw data ---------------------------------------------
dfa_raw_data_modified <- dfa_raw_data %>% 
  butteR:::mutate_batch(nm = new_vars_sm$new_cols, value = F )


# make some cleanup -------------------------------------------------------
kbo_data_modified <- kobold::kobold(survey = dfa_survey %>% filter(name %in% colnames(dfa_raw_data_modified)),
                                    choices = dfa_choices_modified,
                                    data = dfa_raw_data_modified,
                                    cleaning = dfa_cleaning_log)
kbo_data_cleaned <- kobold::kobold_cleaner(kbo_data_modified)


# handling personally Identified Information(PII) -------------------------

dfa_handle_pii <- kbo_data_cleaned$data %>% 
  select(-c(`id_type_refugee/unhcr_refugee_id`, `id_type_refugee/opm_attestation_card`)) %>% 
  mutate(across(any_of(variables_to_remove_from_data), .fns = ~na_if(., .)))
  
# handling added responses after starting data collection and added responses in the cleaning process-----------------

sm_colnames <- dfa_handle_pii %>% 
  select(contains("/")) %>% 
  colnames() %>% 
  str_replace_all(pattern = "/.+", replacement = "") %>% 
  unique()

dfa_handle_sm_data <- dfa_handle_pii

for (cur_sm_col in sm_colnames) {
  dfa_updated_data <- dfa_handle_sm_data %>% 
    mutate(
      across(contains(paste0(cur_sm_col, "/")), .fns = ~ifelse(!is.na(!!sym(cur_sm_col)) & is.na(.) , FALSE, .)),
      across(contains(paste0(cur_sm_col, "/")), .fns = ~ifelse(is.na(!!sym(cur_sm_col)), NA, .))
    )
  dfa_handle_sm_data <- dfa_updated_data
}

dfa_final_cleaned_data <- dfa_handle_sm_data

# write final modified data -----------------------------------------------
write_csv(dfa_final_cleaned_data, file = paste0("outputs/", butteR::date_file_prefix(), "_clean_data.csv"))
