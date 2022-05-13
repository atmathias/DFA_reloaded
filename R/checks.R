
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

# load sample data
df_dfa_sample_data <- sf::st_read("inputs/dfa_settlement_host_samples.gpkg", quiet = TRUE)

# Logical flow

logical_output <- list()

# data not meeting minimal requirements---------------------------------------

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
df_respondent_age_out_of_range <-  df_dfa_data %>% 
  filter(respondent_age < 18 | respondent_age > 100) %>% 
  mutate(m.type = "remove_survey",
         m.name = "respondent_age",
         m.current_value = as.character(respondent_age),
         m.value = "",
         m.issue_id = "logic_m_requirement_respondent_age_out_of_range",
         m.issue = "respondent_age_out_of_range",
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

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_respondent_age_out_of_range")


# data testing
df_data_tesing <- df_dfa_data %>% 
  filter(m.start_date <= as_date("2021-08-29") | m.point_number == "13m." | 
           m.start_date %in% c(as_date("2021-09-08"), as_date("2021-09-09"), as_date("2021-09-21")) |
           str_detect(string = point_number, pattern = fixed('test', ignore_case = TRUE)) |
           m.uuid %in% c("27b0ffe2-8d47-4897-b402-1928fd23cfb3",
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
                         "d8936d35-7e1c-48d9-bafa-b25e758c05eb")) %>% 
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

df_survey_duration <- df_dfa_data %>% 
  mutate(interview.survey_duration = lubridate::time_length(end - start, unit = "min"),
         interview.survey_duration = ceiling(interview.survey_duration),
         m.type = "remove_survey",
         m.name = "point_number",
         m.current_value = "",
         m.value = "",
         m.issue_id = case_when(interview.survey_duration < min_survey_time ~ "less_survey_tme",
                                interview.survey_duration > Max_survey_time ~ "more_survey_time",
                                TRUE ~ "acceptable_survey_time"),
         m.issue = glue("{interview.survey_duration} minutes taken to complete survey"),
         m.other_text = "",
         m.checked_by = "",
         m.checked_date = as_date(today()),
         m.comment = "", 
         m.reviewed = "",
         m.adjust_log = "",
         m.uuid_cl = paste0(m.uuid, "_", m.type, "_", m.name),
         m.so_sm_choices = "") %>% 
  filter(m.issue_id %in% c("less_survey_tme", "more_survey_time")) %>% 
  dplyr::select(starts_with("m.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "m.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_survey_duration")        


# Time between surveys
min_time_btn_surveys <- 5

df_time_btn_surveys <- df_dfa_data %>% 
  group_by(m.start_date, m.enumerator_id) %>% 
  filter(n()>1) %>% 
  arrange(start, .by_group = TRUE) %>% 
  mutate(m.time_btn_surveys = lubridate::time_length(start - lag(end, default = first(start)), unit = "min"),
         m.time_btn_surveys = ceiling(m.time_btn_surveys)) %>% 
  filter(m.time_btn_surveys !=0 & m.time_btn_surveys < min_time_btn_surveys) %>% 
  mutate(m.type = "remove_survey",
         m.name = "point_number",
         m.current_value = "",
         m.value = "",
         m.issue.id = "less_time_btn_surveys",
         m.issue = glue("{m.time_btn_surveys} minutes taken between surveys"),
         m.other_text = "",
         m.checked_by = "",
         m.checked_date = as_date(today()),
         m.comment = "", 
         m.reviewed = "",
         m.adjust_log = "",
         m.uuid_cl = paste0(m.uuid, "_", m.type, "_", m.name),
         m.so_sm_choices = "") %>% 
  dplyr::select(starts_with("m.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "m.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_time_btn_surveys")


# logical checks -------------------------------------------------------------

# Anyone who selected "ugandan" and previously answered community_type = refugee, should be checked.

df_respondent_nationality <- df_dfa_data %>% 
  filter(status == "refugee", nationality == "uganda") %>% 
  mutate(m.type = "change_response",
         m.name = "nationality",
         m.current_value = nationality,
         m.value = "",
         m.issue_id = "logic_issue_nationality",
         m.issue = "nationality: ugandan but community_type: refugee",
         m.other_text = "",
         m.checked_by = "",
         m.checked_date = as_date(today()),
         m.comment = "", 
         m.reviewed = "1",
         m.adjust_log = "",
         m.uuid_cl = "",
         m.uuid_cl = paste0(m.uuid, "_", m.type, "_", m.name),
         m.so_sm_choices = "") %>%   
  dplyr::select(starts_with("m.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "m.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_respondent_nationality")

# Anyone who selected host for "type of community" and answers "refugee ID" or "beneficiary ID" should be checked.

df_id_type_selected <- df_dfa_data %>% 
  filter(status == "host_community", str_detect(string = id_type, pattern = "unhcr_refugee_id|ug_refugee_id|benef_id_not_unhcr")) %>% 
  mutate(m.type = "change_response",
         m.name = "id_type",
         m.current_value = id_type,
         m.value = "",
         m.issue_id = "logic_issue_status",
         m.issue = glue("status: host_community but refugee id_type: {id_type}"),
         m.other_text = "",
         m.checked_by = "",
         m.checked_date = as_date(today()),
         m.comment = "", 
         m.reviewed = "1",
         m.adjust_log = "",
         m.uuid_cl = "",
         m.uuid_cl = paste0(m.uuid, "_", m.type, "_", m.name),
         m.so_sm_choices = "") %>%   
  dplyr::select(starts_with("m.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "m.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_respondent_nationality")


# If respondents have selected a language but have NOT selected the same language that they previously selected for their main language, we need to check the survye.


df_language_selected <- df_dfa_data %>% 
  mutate(m.type = "change_response",
         m.name = "main_language",
         m.current_value = main_language,
         m.value = "",
         m.issue_id = ifelse(str_detect(string = language_understand, pattern = main_language, negate = TRUE) , 
                             "logic_issue_main_language", "main_language_also_understood"),
         m.issue = glue("main_language: {main_language} not in understood languages: {language_understand}"),
         m.other_text = "",
         m.checked_by = "",
         m.checked_date = as_date(today()),
         m.comment = "", 
         m.reviewed = "",
         m.adjust_log = "",
         m.uuid_cl = paste0(m.uuid, "_", m.type, "_", m.name),
         m.so_sm_choices = "") %>% 
  filter(m.issue_id == "logic_issue_main_language") %>% 
  dplyr::select(starts_with("m.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "m.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_language_selected")



# If respondent has selected "none" in addition to another option, the survey needs to be checked.
# type_phone_owned

df_type_phone_owned <- df_dfa_data %>% 
  rowwise() %>% 
  mutate(interview.type_phone_owned_count = sum(c_across(starts_with("type_phone_owned/")), na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(m.type = "remove_option",
         m.name = "type_phone_owned",
         m.current_value = "none",
         m.value = "none",
         m.issue_id = ifelse(interview.type_phone_owned_count > 1 & `type_phone_owned/none` == 1, "logic_issue_type_phone_owned", "expected_response"),
         m.issue = glue("none option selected with other options: {type_phone_owned}"),
         m.other_text = "",
         m.checked_by = "",
         m.checked_date = as_date(today()),
         m.comment = "", 
         m.reviewed = "",
         m.adjust_log = "",
         m.uuid_cl = paste0(m.uuid, "_", m.type, "_", m.name),
         m.so_sm_choices = "") %>% 
  filter(m.issue_id == "logic_issue_type_phone_owned") %>% 
  dplyr::select(starts_with("m.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "m.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_type_phone_owned")


# If previously selected "0" in response to "how many mobile phone numbers do you have" the survye needs to be checked.
# walk_top_upun_expected_response
# add this constraint to odk

df_walk_top_up <- df_dfa_data %>% 
  filter(walk_top_up %in% c("no_need_to_walk", "regularly_walk", "walk_specifically") , no_phones_hh_owns == 0) %>%
  mutate(m.type = NA,
         m.name = "type_phone_owned",
         m.current_value = "walk_top_up",
         m.value = "none",
         m.issue_id = "un_expected_response",
         m.issue = NA,
         m.other_text = "",
         m.checked_by = "",
         m.checked_date = as_date(today()),
         m.comment = "", 
         m.reviewed = "",
         m.adjust_log = "",
         m.uuid_cl = paste0(m.uuid, "_", m.type, "_", m.name),
         m.so_sm_choices = "") %>% 
  dplyr::select(starts_with("m.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "m.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_walk_top_up")


# If they previously selected "yes" to having mobile internet coverage (Q56) and now replied "no", the survey needs to be checked.
# mobile_internet == "yes" and internet_awareness == "no"

df_internet_awreness <- df_dfa_data %>% 
  filter(mobile_internet == "yes", internet_awareness == "no") %>%
  mutate(m.type = "change_response",
         m.name = "internet_awareness",
         m.current_value = internet_awareness,
         m.value = NA,
         m.issue_id = "logic_issue_internet_awareness",
         m.issue = "mobile_internet: yes but internet_awareness: no",
         m.other_text = "",
         m.checked_by = "",
         m.checked_date = as_date(today()),
         m.comment = "", 
         m.reviewed = "",
         m.adjust_log = "",
         m.uuid_cl = paste0(m.uuid, "_", m.type, "_", m.name),
         m.so_sm_choices = "") %>% 
  dplyr::select(starts_with("m.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "m.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_internet_awreness")


# If respondents who previously said they DO NOT have access to a feature phone or smart phone are now selecting uses for their phones that can only be done online (e.g. social media, access to information online etc.), survey needs to be checked
# # mobile_phone_use

df_mobile_phone_use <- df_dfa_data %>% 
  filter(str_detect(string = type_phone_owned, pattern = "none|basic_phone")) %>% 
  mutate(m.type = NA,
         m.name = "mobile_phone_use",
         m.current_value = NA,
         m.value = NA,
         m.issue_id = ifelse(str_detect(string = mobile_phone_use, 
                                        pattern = "social_media|online_inform_access|mobile_cash_voucher|mobile_banking|contactless_mobile_pay"), "un_expected_response", "expected_response"),
         m.issue = "mobile_internet: yes but internet_awareness: no",
         m.other_text = "",
         m.checked_by = "",
         m.checked_date = as_date(today()),
         m.comment = "", 
         m.reviewed = "",
         m.adjust_log = "",
         m.uuid_cl = paste0(m.uuid, "_", m.type, "_", m.name),
         m.so_sm_choices = "") %>% 
  filter(m.issue_id == "un_expected_response") %>% 
  dplyr::select(starts_with("m.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "m.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_mobile_phone_use")


# If in previous qn "why do you want to have  a mobile money account?" they answered "it is safer than keeping cash at home" and they now asnwered "the system is not safe i am concerned that my money will disappear", survey needs to be checked
# reason_want_mm_acc/safer_than_home == 1 and reason_not_open_mm_acc/unsafe_system

df_reason_not_open_bank_acc <- df_dfa_data %>% 
  filter(`reason_want_bank_acc/safe_storage` == 1, `reason_not_open_bank_acc/unsafe_system` == 1) %>% 
  mutate(m.type = "remove_option",
         m.name = "reason_not_open_bank_acc",
         m.current_value = "unsafe_system",
         m.value = "unsafe_system",
         m.issue_id = "logic_issue_reason_not_open_bank_acc",
         m.issue = "reason_want_bank_acc: safer_than_home but reason_not_open_bank_acc: unsafe_system",
         m.other_text = "",
         m.checked_by = "",
         m.checked_date = as_date(today()),
         m.comment = "", 
         m.reviewed = "",
         m.adjust_log = "",
         m.uuid_cl = paste0(m.uuid, "_", m.type, "_", m.name),
         m.so_sm_choices = "") %>% 
  dplyr::select(starts_with("m.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "m.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_reason_not_open_bank_acc")


# if in previous question 'Why do you want to have a pre-paid or smart card?' answered "it will allow me to securely store my money" and they now chose "the system is not safe i am concerned that my money will disappear", check survey
# reason_want_card/safe_storage and reason_not_want_card/unsafe_system

df_reason_not_want_card <- df_dfa_data %>% 
  filter(`reason_want_card/safe_storage` == 1, `reason_not_want_card/unsafe_system` == 1) %>% 
  mutate(m.type = "remove_option",
         m.name = "reason_not_want_card",
         m.current_value = "unsafe_system",
         m.value = "unsafe_system",
         m.issue_id = "logic_issue_reason_not_want_card",
         m.issue = "reason_want_card: safer_than_home but reason_not_want_card: unsafe_system",
         m.other_text = "",
         m.checked_by = "",
         m.checked_date = as_date(today()),
         m.comment = "", 
         m.reviewed = "",
         m.adjust_log = "",
         m.uuid_cl = paste0(m.uuid, "_", m.type, "_", m.name),
         m.so_sm_choices = "") %>% 
  dplyr::select(starts_with("m.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "m.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_reason_not_want_card")




# other_checks ------------------------------------------------------------

# add and rename some columns
df_dfa_tool_data <- df_dfa_data %>% 
  rename(uuid = `_uuid`) %>% 
  mutate(start_date = as_date(start))

# get questions with other
other_variable_names <-  df_dfa_data %>% 
  select(ends_with("other"), -contains("/")) %>% 
  colnames()


# data.frame for holding _other response data
dfa_other_response_data <- data.frame()

for (cln in other_variable_names) {
  
  current_parent_qn = str_replace_all(string = cln, pattern = "_other", replacement = "")
  
  dfa_filtered_data <- df_dfa_tool_data %>% 
    select(-contains("/")) %>% 
    select(uuid, start_date, enumerator_id, district_name, point_number, other_text = cln,
           current_value = current_parent_qn) %>% 
    filter(!is.na(other_text), !other_text %in% c(" ", "NA")) %>% 
    mutate(other_name = cln,
           m.my_current_value_extract = ifelse(str_detect(current_value, "other\\b"), str_extract_all(string = current_value, pattern = 
                                                                                                        "other\\b|[a-z]+._other\\b"), current_value ),
           value = "",
           parent_qn = current_parent_qn)       
  
  dfa_other_response_data <- rbind(dfa_other_response_data, dfa_filtered_data)
}

# arrange data
dfa_arranged_data <- dfa_other_response_data %>% 
  arrange(start_date, uuid)

# get choices to add to the _other responses extracted
dfa_grouped_choices <- df_dfa_choices %>% 
  group_by(list_name) %>% 
  summarize(choice_options = paste(name, collapse = " : ")) %>% 
  arrange(list_name)  

# extract parent question and join survey for existing list_name
dfa_data_parent_qns <- dfa_arranged_data %>% 
  left_join(df_dfa_survey %>% select(name, type), by = c("parent_qn"="name")) %>% 
  separate(col = type, into = c("select_type", "list_name"), sep = " ", remove = TRUE, extra = "drop") %>% 
  rename(name = parent_qn)

# join other responses with choice based on list_name
dfa_join_other_response_with_choices <- dfa_data_parent_qns %>% 
  left_join(dfa_grouped_choices, by = "list_name") %>% 
  mutate(issue_id = "other_checks",
         issue = "",
         checked_by = "",
         checked_date = as_date(today()),
         comment = "",
         reviewed = "",
         adjust_log = ""
  ) %>% 
  filter(str_detect(string = current_value, pattern = "other\\b|[a-z]+._other\\b"))

# take care of select_one and select_multiple (change response, add_option, remove_option) 
output <- list()

# select_one checks
output$select_one <- dfa_join_other_response_with_choices %>% 
  filter(str_detect(select_type, c("select_one|select one"))) %>% 
  mutate(type = "change_response")

# select_multiple checks
select_multiple_data <- dfa_join_other_response_with_choices %>% 
  filter(str_detect(select_type, c("select_multiple|select multiple")))

select_multiple_add_option <- select_multiple_data %>% 
  mutate(type = "add_option")
select_multiple_remove_option <- select_multiple_data %>% 
  mutate(type = "remove_option",
         value = as.character(m.my_current_value_extract))  

output$select_multiple <- bind_rows(select_multiple_add_option, select_multiple_remove_option) %>% 
  arrange(uuid, start_date, enumerator_id, name)

# merge other checks
merged_other_checks <- bind_rows(output) %>% 
  mutate(uuid_cl = paste0(uuid, "_", type, "_", name),
         so_sm_choices = choice_options) %>% 
  select(uuid,
         start_date,
         enumerator_id,
         district_name,
         point_number,
         type,
         name,
         current_value,
         value,
         issue_id,
         issue,
         other_text,
         checked_by,
         checked_date,
         comment,
         reviewed,
         adjust_log,
         uuid_cl,
         so_sm_choices)



# spatial checks ----------------------------------------------------------

sample_pt_nos <- df_dfa_sample_data %>% 
  mutate(unique_pt_number = paste0(status, "_", Name)) %>% 
  pull(unique_pt_number) %>% 
  unique()

# duplicate point numbers

df_k_duplicate_pt_nos <- df_dfa_data %>% 
  mutate(unique_pt_number = paste0(status, "_", point_number )) %>% 
  group_by(m.district_name, status, m.point_number) %>% 
  filter(n() > 1, unique_pt_number %in% sample_pt_nos) %>% 
  mutate(m.type = "change_response",
         m.name = "point_number",
         m.current_value = point_number,
         m.value = NA,
         m.issue_id = "spatial_c_duplicate_pt_no",
         m.issue = glue("point_number: {point_number} is duplicated: check that its not a repeated survey"),
         m.other_text = "",
         m.checked_by = "",
         m.checked_date = as_date(today()),
         m.comment = "", 
         m.reviewed = "",
         m.adjust_log = "",
         m.uuid_cl = paste0(m.uuid, "_", m.type, "_", m.name),
         m.so_sm_choices = "") %>% 
  dplyr::select(starts_with("m.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "m.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_k_duplicate_pt_nos")

# point id does not exist in sample

df_k_pt_not_in_sample <- df_dfa_data %>% 
  mutate(unique_pt_number = paste0(status, "_", point_number)) %>% 
  filter((!unique_pt_number %in% sample_pt_nos)) %>% 
  mutate(m.type = "change_response",
         m.name = "point_number",
         m.current_value = point_number,
         m.value = NA,
         m.issue_id = "spatial_k_pt_no_not_in_sample",
         m.issue = glue("point_number: {point_number} not in samples"),
         m.other_text = "",
         m.checked_by = "",
         m.checked_date = as_date(today()),
         m.comment = "", 
         m.reviewed = "",
         m.adjust_log = "",
         m.uuid_cl = paste0(m.uuid, "_", m.type, "_", m.name),
         m.so_sm_choices = "") %>% 
  dplyr::select(starts_with("m.")) %>% 
  rename_with(~str_replace(string = .x, pattern = "m.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_k_pt_not_in_sample")


# distance exceeds threshold

threshold_dist <- 150

dfa_sample_data_threshold <- df_dfa_sample_data %>% 
  mutate(unique_pt_number = paste0(status, "_",Name)) %>% 
  sf::st_transform(4326)
  
  
 dfa_tool_data_threshold <- df_dfa_data %>% 
   mutate(unique_pt_number = paste0(status, "_", point_number)) %>% 
   sf::st_as_sf(coords = c("_geopoint_longitude","_geopoint_latitude"), crs = 4326) 
 
 # sample_data_unique_pts
 sample_data_unique_pts <- dfa_sample_data_threshold %>%  
 pull(unique_pt_number) %>% 
 unique()
 
 # tool_data_unique_pts
 tool_data_unique_pts <- dfa_tool_data_threshold %>% 
   pull(unique_pt_number) %>% 
   unique()
  
 sample_pt_nos_thresh <- sample_data_unique_pts[sample_data_unique_pts %in% tool_data_unique_pts]
 
 if(length(sample_pt_nos_thresh) > 0) {
   
   # tibble to hold data
   dfa_data_with_distance <- tibble()
   
     for (pt_number in sample_pt_nos_thresh){
    current_sample <- dfa_sample_data_threshold %>% 
    filter(unique_pt_number == pt_number)
    current_tool_data <- dfa_tool_data_threshold %>% 
    filter(unique_pt_number == pt_number) 
  
    if(nrow(current_tool_data) > 0){
      current_sample_target_dist <- sf::st_distance(x = current_sample, y = current_tool_data, by_element = TRUE)  
      
      current_data_with_dist <- current_tool_data %>% 
        sf::st_drop_geometry() %>% 
        mutate(distance = round(x = current_sample_target_dist, digits = 0))
      
      dfa_data_with_distance <- bind_rows(dfa_data_with_distance, current_data_with_dist)    
   }
   
 }
 
 }
 
 
 
 
 
 
 
 
 
 
 
 
 
  
  
  
  
  
  
  


