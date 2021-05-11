
read_file_raw <- function(){
  
  
  data_to_use <-  input$data_to_use
  preloaded_data_options <-  preloaded_data_options ## defined in global file
  preloaded_data_choice <-  input$preloaded_data_choice
  uploaded_data_contacts_list_path <-  input$uploaded_data_contacts_list$datapath
  uploaded_data_follow_up_list_path <-  input$uploaded_data_follow_up_list$datapath
  
  
  if (data_to_use == "Use preloaded data") {
    
    contacts_list <- 
      preloaded_data_options[[preloaded_data_choice]]$contacts_list %>% 
      clean_names() %>%
      type_convert() %>% 
      mutate(region_de_residence = str_to_sentence(region_de_residence)) # important for picking admin_1 in data subset
    
    
    
    follow_up_list <-
      preloaded_data_options[[preloaded_data_choice]]$follow_up_list %>% 
      clean_names() %>%
      type_convert()
    
    
  } else if (data_to_use == "Use uploaded data") {
    contacts_list <-
      uploaded_data_contacts_list_path %>% 
      rio::import()  %>%
      clean_names() %>%
      type_convert() %>% 
      mutate(region_de_residence = str_to_sentence(region_de_residence)) # important for picking admin_1 in data subset
    
    follow_up_list <-
      rio::import(uploaded_data_follow_up_list_path)  %>%
      clean_names() %>%
      type_convert() %>% 
      rename(code_unique_du_contact = quel_est_le_code_du_contact) ## need to rename now to permit join
    
  }
  
  
  tracing_data_raw  <- list(contacts_list = contacts_list, 
                            follow_up_list = follow_up_list)
  
  return(tracing_data_raw)
}


read_file_transformed <- function(tracing_data_raw){
  
  
  contacts_df_long_transformed <-
    tracing_data_raw %>%
    .$contacts_list %>% 
    ## for speeding up testing
    {if (PARAMS$testing_mode) slice_sample(., n = 10) else .} %>% 
    mutate(counter = 1) %>%
    # row numbers to match Excel spreadsheet
    mutate(row_id = row_number() + 1) %>%
    # clean admin levels
    mutate(across(c(region_de_residence, district_de_residence),  # EDIT 2021-03-04 I changed it.  don't change region spelling as we used the raw spellings to populate the dropdown select on the admin_1 tab
                  ~ .x %>%
                    str_to_lower() %>%
                    str_to_title() %>%
                    replace_na("NA") %>%
                    str_trim() %>% 
                    str_replace_all("  ", " "))) %>% 
    left_join(tracing_data_raw$follow_up_list, 
              by = "code_unique_du_contact") %>% 
    ## rename to match columns for which scripts were originally written
    rename_with(~
                  case_when(.x == "code_unique_du_contact" ~ "contact_id",
                            .x == 'code_du_cas_index' ~ "linked_case_id",
                            .x == "sexe" ~ "sex",
                            .x == 'quel_est_le_nom_du_contact' ~ "last_name",
                            .x == 'quel_est_le_prenom_du_contact' ~ 'first_name',
                            .x == 'quel_est_l_age_du_contact' ~ 'age',
                            .x == 'quelle_est_l_unite_de_l_age' ~ 'age_unit',
                            .x == 'region_de_residence' ~ 'admin_1',
                            .x == 'district_de_residence' ~ 'admin_2',
                            .x == 'quel_est_le_lien_du_contact_avec_le_cas' ~ 'link_with_the_case',
                            .x == 'quel_type_de_contact' ~ 'type_of_contact' ,
                            .x == 'date_du_dernier_contact_avec_le_cas' ~ 'date_of_last_contact',
                            .x == 'date_du_suivi' ~ 'follow_up_date',
                            .x == 'date_de_suivi' ~ 'follow_up_date',
                            .x == 'jour_du_suivi' ~ 'follow_up_day',
                            .x == 'jour_de_suivi' ~ 'follow_up_day',
                            .x == 'issue_du_suivi' ~ 'follow_up_status',
                            .x == 'issue_de_suivi' ~ 'follow_up_status',
                            .x == 'etat_du_suivi' ~ 'follow_up_status_simple',
                            .x == 'etat_de_suivi' ~ 'follow_up_status_simple',
                            TRUE ~ .x)) %>% 
    mutate(across(.cols = any_of(c("admin_1", "admin_2",
                                   "linked_case_id","link_with_the_case",
                                   "follow_up_status","follow_up_status_simple")), 
                  .fns = ~ replace_na(.x, "Manquant")
    )) %>% 
    ## possibly temporary. replace all nas
    # mutate(across(.cols = where(~ is.character(.x) | is.factor(.x)),
    #                 .fns = ~ replace_na(.x, "Manquant")
    # )) %>% 
    # if follow-up lasted the full 21 days, change last follow_up state to "Fin de suivi"
    mutate(follow_up_status = if_else(follow_up_day == 10 & follow_up_status == "vu ou contacte",
                                      "Fin de suivi",
                                      follow_up_status)) %>% 
    mutate(follow_up_status = str_to_sentence(follow_up_status)) %>% 
    ## shorten
    mutate(follow_up_status = recode(follow_up_status, 
                                     "Devenu symptomatique et resultats tests attendus" = "Symptomatique, resultats attendus"
    )) %>% 
    ## shorten
    mutate(follow_up_status_simple = recode(follow_up_status_simple, 
                                            "vu ou contacte" = "Seen",
                                            "non vu ou contacte" = "Not seen"
    )) %>% 
    ## what exactly does poursuite du suivi mean?
    ## I am not sure. But in the meantime we replace it where possible
    mutate(follow_up_status = ifelse(follow_up_status_simple == "Not seen",
                                     "Manquant",
                                     follow_up_status)) %>%
    # convert dates to dates
    mutate(across(.cols = matches("date|Date"),
                  .fns = 
                    ~ .x %>% 
                    str_replace_all(" UTC", "") %>% 
                    as.Date())) %>% 
    ## start and end date
    mutate(follow_up_start_date = if_else(follow_up_day == 1, follow_up_date, NA_Date_)) %>% 
    mutate(follow_up_end_date = if_else(follow_up_day == 14, follow_up_date, NA_Date_)) %>% 
    ## complete data
    group_by(row_id) %>% 
    complete(row_id, follow_up_date = seq.Date(unique(date_of_last_contact) + days(1), 
                                               unique(date_of_last_contact) + days(10),
                                               by = "1 days")) %>% 
    ## what does this do? I can't remember (I guess it removes NAs. Why?)
    mutate(across(.cols = -tidyr::one_of("follow_up_date", "follow_up_day", 
                                         "follow_up_status", "follow_up_status_simple"),
                  .fns = ~ first(na.omit(.x))) ) %>% 
    ungroup() %>% 
    # for the simple version of follow up state, assume that manquant means no follow-up
    mutate(follow_up_status_simple = ifelse(follow_up_status_simple == "Manquant",
                                            "Not seen",
                                            follow_up_status_simple)) %>%
    ## row number for easy tracking
    mutate(row_number = row_number()) %>% 
    mutate(follow_up_day = as.numeric(follow_up_date - date_of_last_contact)) %>% 
    distinct(row_id, follow_up_date, .keep_all = TRUE) # not sure why there are duplicates but there are
  
  return(contacts_df_long_transformed)
  
}


read_file_filtered <- function(){
  ## takes no inputs for now
  
  
  contacts_df_long_transformed <-  read_file_transformed_reactive()
  
  
  all_cols <- 
    contacts_df_long_transformed %>% 
    names()
  
  cols_to_filter <- 
    contacts_df_long_transformed %>% 
    ## remember to use this in the ui generating code as well
    select(-any_of(c("follow_up_date", 
                     "first_name", 
                     "last_name", 
                     "row_id", 
                     "row_number"))) %>% 
    janitor::remove_constant() %>% 
    names()
  
  
  filter_or_not <-  input$filter_or_not
  todays_date <-  todays_date_reactive()
  
  if ((!is.null(input$filter_or_not)) && input$filter_or_not == "Yes"){
    
    temp <- 
      contacts_df_long_transformed %>% 
      as.data.frame()
    
    for(j in 1:length(all_cols)) {
      
      col <- temp[ ,all_cols[j]]
      
      if (all_cols[[j]] %in% cols_to_filter){
        
        
        ## factor
        if (is.factor(col)) {
          
          ## if na_input ui element exists and is TRUE, include NAs
          if (  (!is.null(input[[ paste0("na_", all_cols[j]  )  ]])) &&
                input[[ paste0("na_", all_cols[j]  )  ]] == TRUE ){
            
            temp <- temp[ temp[,all_cols[j]] %in% input[[all_cols[j]]] |
                            is.na(temp[all_cols[j]]  ), ]
            ## otherwise, exclude NAs
          } else {
            temp <- temp[ temp[,all_cols[j]] %in% input[[all_cols[j]]], ]
          }
          
        } else if (is.character(col)) {
          
          ## if na_input ui element exists and is TRUE, include NAs
          if (  (!is.null(input[[ paste0("na_", all_cols[j]  )  ]])) &&
                input[[ paste0("na_", all_cols[j]  )  ]] == TRUE ){
            
            temp <- temp[ temp[,all_cols[j]] %in% input[[all_cols[j]]] |
                            is.na(temp[all_cols[j]]  ), ]
            ## otherwise, exclude NAs
          } else {
            temp <- temp[ temp[,all_cols[j]] %in% input[[all_cols[j]]], ]
          }
          
        } else if (is.numeric(col)) {
          
          
          ## if na_input ui element exists and is TRUE, include NAs
          if (  (!is.null(input[[ paste0("na_", all_cols[j]  )  ]])) &&
                input[[ paste0("na_", all_cols[j]  )  ]] == TRUE ){
            
            
            temp <- temp[temp[,all_cols[j]] >= input[[all_cols[j]]][1] | 
                           is.na(temp[all_cols[j]]) ,  ]
            temp <- temp[temp[,all_cols[j]] <= input[[all_cols[j]]][2] | 
                           is.na(temp[all_cols[j]]), ]
            
            ## otherwise, exclude NAs
          } else {
            
            temp <- temp[temp[,all_cols[j]] >= input[[all_cols[j]]][1], ]
            temp <- temp[temp[,all_cols[j]] <= input[[all_cols[j]]][2], ]
          }
          
          
        } else if(lubridate::is.Date(col)) {
          ## if na_input ui element exists and is TRUE, include NAs
          if (  (!is.null(input[[ paste0("na_", all_cols[j]  )  ]])) &&
                input[[ paste0("na_", all_cols[j]  )  ]] == TRUE ){
            
            
            temp <- temp[temp[,all_cols[j]] >= input[[all_cols[j]]][1] | 
                           is.na(temp[all_cols[j]]) ,  ]
            
            temp <- temp[temp[,all_cols[j]] <= input[[all_cols[j]]][2] | 
                           is.na(temp[all_cols[j]]), ]
            
            ## otherwise, exclude NAs
          } else {
            
            temp <- temp[temp[,all_cols[j]] >= input[[all_cols[j]]][1], ]
            temp <- temp[temp[,all_cols[j]] <= input[[all_cols[j]]][2], ]
          }
        }
      }
      
    }
    
    contacts_df_long_transformed <- 
      temp %>% 
      as_tibble() %>% 
      ## not sure if necessary again
      # convert dates to dates
      mutate(across(.cols = matches("date|Date"),
                    .fns = 
                      ~ .x %>% 
                      str_replace_all(" UTC", "") %>% 
                      as.Date()))
  }
  
  
  ## return
  contacts_df_long_transformed %>%
    ## add future follow-up. 
    mutate(follow_up_status = if_else(follow_up_date > todays_date, 
                                      "Suivi futur",
                                      follow_up_status)) %>% 
    mutate(follow_up_status_simple = if_else(follow_up_date > todays_date, 
                                             "Suivi futur",
                                             follow_up_status_simple)) %>% 
    # keep only those for whom follow-up had begun by the date of review
    group_by(row_id) %>% 
    filter(min(follow_up_date) <= todays_date) %>%
    ungroup() %>% 
    # add legend colors
    # legend_df is defined in global.R 
    left_join(legend_df, by = c("follow_up_status" = "breaks"))
}