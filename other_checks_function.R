

# Extract other specify function ------------------------------------------

other_data_extract <- function(input_dfa_tool_data, input_survey, input_choices) {
  
  # add and rename some columns
  dfa_data <- input_dfa_tool_data %>% 
    rename(uuid = '_uuid') %>% 
    mutate(start_date = as_date(start))
  
  # get questions with other
  other_variable_names <-  dfa_data %>% 
    select(ends_with("other"), -contains("/")) %>% 
    colnames()
  
  # data.frame for holding _other response data
  dfa_other_response_data <- data.frame()
  
  for (cln in other_variable_names) {
    
  current_parent_qn = str_replace_all(string = cln, pattern = "_other", replacement = "")
    
  dfa_filtered_data <- dfa_data %>% 
    select(-contains("/")) %>% 
    select(uuid, start_date, enumerator_id, district_name, point_number, other_text = cln,
           current_value = current_parent_qn) %>% 
    filter(!is.na(other_text), !other_text %in% c(" ", "NA")) %>% 
    mutate(other_name = cln,
           m.my_current_value_extract = ifelse(str_detect(current_value, "other\\b"), str_extract_all(string = current_value, pattern = 
                                                                                       "other\\b|[a-z]+._other\\b"), current_value ),
           value = "",
           parent_qn <- current_parent_qn)
    
    dfa_other_response_data <- rbind(dfa_other_response_data, dfa_filtered_data)
  }
  
  # arrange data
  dfa_arranged_data <- dfa_other_response_data %>% 
    arrange(start_date, uuid)
    
  # get choices to add to the _other responses extracted
  dfa_grouped_choices <- input_choices %>% 
    group_by(list_name)
    summarize(choice_options = paste(name, collapse = " : ")) %>% 
    arrange(list_name)  
    
    
  # extract parent question and join survey for existing list_name
    dfa_data_parent_qns <- dfa_arranged_data %>% 
    left_join(input_survey %>% select(name, type), by = c("parent_qn"="name")) %>% 
    separate(col = type, into = c(""))
      
  
    
    
    
    
    
    
    

 }


