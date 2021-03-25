
read_file <- reactive({
  
  req(input$Analyze)
  
  if (input$data_to_use_id == "Use preloaded data") {
    
    contacts_df_raw <- preloaded_data_options[[input$preloaded_data]]
    
  } else if (input$data_to_use_id == "Use uploaded data") {
    
    contacts_df_raw <- read_csv(input$uploaded_data$datapath) %>% janitor::clean_names()
    
  } else {
    contacts_df_raw <- NULL
    
  }
  #prep data
  
  contacts_df <- 
    contacts_df_raw %>%         
    # add counter column. 1 for all records
    mutate(counter = 1) %>%
    # row numbers to match Excel spreadsheet
    mutate(row_id = row_number() + 3) %>%
    # convert dates to dates
    mutate(across(
      .cols = starts_with("date_"),
      .fns = ~ as.Date(anytime(as.character(.x)))
    )) %>%
    # drop records past faux date
    filter(date_last_interaction < todays_date | is.na(date_last_interaction)) %>%
    # convert dates to day of the year
    mutate(across(
      .cols = starts_with("date_"),
      .fns = ~ as.Date(anytime(as.character(.x)))
    )) %>%
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # - add a date of first follow-up.
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # - This really should have been included in the original tool.
    # - Without it, I don't think we can work out what each follow_up day refers to.
    # - below I impute these dates using the protocol I can think of.
    # - if there is a date of exit, assume that the last recorded '1' in the follow-up columns pertains to that date
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # first we duplicate but rename the follow_up_day_columns
    mutate(across(
      .cols = starts_with("follow_up_day_"),
      .fns = ~.,
      .names = "follow_up_date_{.col}"
    )) %>%
    rename_with(
      .cols = starts_with("follow_up_date_"),
      .fn = ~ str_remove_all(.x, "follow_up_day_")
    ) %>%
    # now, we convert the numbers in the follow up columns
    # to reflect the nth day of follow up
    mutate(across(
      .cols = starts_with("follow_up_date_"),
      .fn = ~
        # start from 14 otherwise you'll remove the tens unit before you get to the teens
        str_extract(cur_column(), paste(14:1, collapse = "|")) %>% 
        as.numeric()
    )) %>%
    # - clean the 'reasons for follow_up end' column. Forms the basis of much of later analysis
    mutate(across(
      reasons_end_follow_up,
      ~ .x %>%
        stri_trans_general("Latin-ASCII") %>%
        str_to_lower() %>%
        str_to_sentence() %>%
        recode(
          "Deplace" = "Lost to follow-up",
          "Evade" = "Lost to follow-up",
          "Negatif" = "Has tested negative",
          "Positif" = "Has tested positive"
        )
    )
    ) %>% 
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ~~ clean admin levels ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    mutate(across(
    starts_with("admin"),
    ~ .x %>%
      stri_trans_general("Latin-ASCII") %>%
      str_to_lower() %>%
      str_to_sentence() %>%
      recode(
        "Deplace" = "Lost to follow-up",
        "Evade" = "Lost to follow-up",
        "Negatif" = "Has tested negative",
        "Positif" = "Has tested positive"
      )
  )
  )
  
  
  pivot_day <- 
    contacts_df %>% 
    select(row_id, paste0("follow_up_day_", 1:14)) %>% 
    pivot_longer(cols = c(paste0("follow_up_day_", 1:14)), 
                 names_to = "follow_up_day", 
                 values_to = "follow_up_state_imputed")
  
  contacts_df_long <- 
    contacts_df %>%
    # slice_sample(n = 50) %>%
    left_join(pivot_day, by = "row_id") %>%
    select(-c(starts_with("follow_up_date_"), starts_with("follow_up_day_"))) %>%
    mutate(follow_up_day = str_extract(follow_up_day, paste(14:1, collapse = "|"))) %>%
    mutate(follow_up_day = as.numeric(follow_up_day)) %>%
    # assume that follow up begins from date of last interactions
    mutate(follow_up_date = follow_up_day + date_last_interaction) %>%
    # select(row_id, follow_up_day, follow_up_state_imputed, follow_up_date) %>%
    mutate(follow_up_state_imputed = as.character(follow_up_state_imputed)) %>%
    mutate(follow_up_state_imputed = if_else(is.na(follow_up_state_imputed),
                                             reasons_end_follow_up,
                                             follow_up_state_imputed
    )) %>%
    # - remove status from records that should be still active
    mutate(follow_up_state_imputed = if_else(follow_up_date > todays_date,
                                             "Upcoming follow-up",
                                             follow_up_state_imputed
    )) %>%
    # recode
    mutate(follow_up_state_imputed = recode(follow_up_state_imputed, 
                                            "0" = "Not followed",
                                            "1" = "Followed",
                                            "2" = "Developed symptoms",
                                            "3" = "Lost to follow-up",
                                            "4" = "Missing status",
                                            "5" = "Missing status", 
                                            "11" = "Missing status"
    )) %>% 
    mutate(follow_up_state_imputed = replace_na(follow_up_state_imputed, "Missing status")) %>% 
    # - if follow-up lasted the full 14 days,
    # - change last follow_up state to "Fin de suivi"
    mutate(follow_up_state_imputed = if_else(follow_up_day == 14 & follow_up_state_imputed == "Followed",
                                             "Fin de suivi",
                                             follow_up_state_imputed)) %>% 
    mutate(follow_up_state_imputed = recode(follow_up_state_imputed, 
                                            "Fin de suivi" = "Completed")) %>% 
    select(row_id, follow_up_day, follow_up_date, follow_up_state_imputed)

  # send function of output to global environment.
  # All data-plotting reactives wait for this object to appear in the global environment before executing
  read_file_out <<- list(contacts_df_raw = contacts_df_raw, 
                        contacts_df = contacts_df, 
                        contacts_df_long = contacts_df_long)
  

  return(NULL)
})