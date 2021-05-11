

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~  UI Outputs ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

output$data_to_use_picker <- renderUI({
  radioButtons(inputId = "data_to_use", 
               label = "Input Data", 
               choices = c("Connect to Go.Data"))
})

output$input_data_preloaded_or_uploaded <- 


request_access_reactive <- reactive({
  
  req(input$go_data_url)
  req(input$go_data_username)
  req(input$go_data_password)
  req(input$go_data_outbreak_id)
  req(input$go_data_request_access_button)
  
  url <- input$go_data_url
  username <- input$go_data_username
  password <- input$go_data_password
  outbreak_id <- input$go_data_outbreak_id
  
  access_token <- 
    paste0(url,"api/oauth/token?access_token=123") %>% 
    POST(body = list(username = username,
                     password = password),
         encode = "json") %>% 
    content(as = "text") %>%
    fromJSON(flatten = TRUE) %>%
    .$access_token
  
  ## return
  access_token
  
})


read_file_raw <- function(){
  
    
    url <- input$go_data_url
    username <- input$go_data_username
    password <- input$go_data_password
    outbreak_id <- input$go_data_outbreak_id
    # 
    # ## here for testing
    # url <- "https://godata-r13.who.int/"
    # username <- "godata_api@who.int"
    # password <- "godata_api@who"
    # password <- "bad_password"
    # outbreak_id <- "3b5554d7-2c19-41d0-b9af-475ad25a382b"


    # ~~~~ get access token for API calls ----    

    access_token <- 
    paste0(url,"api/oauth/token?access_token=123") %>% 
      POST(body = list(username = username,
                            password = password),
                 encode = "json") %>% 
      content(as = "text") %>%
      fromJSON(flatten = TRUE) %>%
      .$access_token
    
    # ~~~~ import relevant api collections----    
    
    # import contact follow-ups
    follow_up_list <- 
      GET(paste0(url,"api/outbreaks/",outbreak_id,"/follow-ups"),
                add_headers(Authorization = paste("Bearer", access_token, sep = " "))) %>% 
      content(as="text") %>% 
      fromJSON(flatten = TRUE) %>% 
      as_tibble()
    
    
    # import oubtreak Contacts 
    ## may not be needed. redundant with follow_up_list
    contacts_list <- 
      GET(paste0(url,"api/outbreaks/",outbreak_id,"/contacts"), 
          add_headers(Authorization = paste("Bearer", access_token, sep = " "))) %>% 
      content(as = "text") %>% 
      fromJSON(flatten = TRUE) %>%  
      as_tibble()
    
  
  
  tracing_data_raw  <- list(follow_up_list = follow_up_list, 
                            contacts_list = contacts_list)
  
  return(tracing_data_raw)
}


read_file_transformed <- function(tracing_data_raw){

  
  needed_cols <- c(admin_1 = NA_character_, admin_2 = NA_character_)
  
  contacts_df_long_transformed <-
    tracing_data_raw %>%
    .$contacts_list %>% 
    ## for speeding up testing
    {if (PARAMS$testing_mode) slice_sample(., n = 10) else .} %>% 
    mutate(counter = 1) %>%
    # row numbers to match Excel spreadsheet
    mutate(row_id = row_number() + 1) %>%
    left_join(tracing_data_raw$follow_up_list, 
              by = c("id" = "contact.id")) %>% 
    ## drop inds with missing ids. Will diagnose properly later
    filter(across(.cols =  any_of(c("id.y", "id") ), 
                  .fns = ~ !is.na(.x)
                  )) %>% 
    ## keep important columns
    select(any_of(c("date",
                    "statusId",
                    "contact.type", 
                    "id", 
                    "visualId", 
                    "contact.firstName",
                    "contact.lastName",
                    "contact.gender", 
                    "contact.dateOfReporting", 
                    "contact.dateOfLastContact", 
                    "contact.occupation",
                    "contact.age.years", 
                    "contact.followUp.startDate", 
                    "contact.followUp.endDate", 
                    "address.city", 
                    "row_id", 
                    "counter"))) %>% 
    # clean admin levels
    mutate(across(any_of("address.city"),
                  ~ .x %>%
                    str_to_lower() %>%
                    str_to_title() %>%
                    replace_na("NA") %>%
                    str_trim() %>% 
                    str_replace_all("  ", " "))) %>% 
    ## rename to match columns for which scripts were originally written
    rename_with(~
                  case_when(.x == "visualId" ~ "contact_id",
                            .x == "contact.gender" ~ "sex",
                            .x == 'contact.lastName' ~ "last_name",
                            .x == 'contact.firstName' ~ 'first_name',
                            .x == 'contact.age.years' ~ 'age',
                            .x == 'address.city' ~ 'admin_1',
                            .x == 'contact.type' ~ 'type_of_contact' ,
                            .x == 'contact.dateOfLastContact' ~ 'date_of_last_contact',
                            .x == 'contact.followUp.startDate' ~ 'follow_up_start_date',
                            .x == 'contact.followUp.endDate' ~ 'follow_up_end_date',
                            .x == 'date' ~ 'follow_up_date',
                            .x == 'statusId' ~ 'follow_up_status',
                            .x == 'contact.occupation' ~ 'occupation',
                            TRUE ~ .x)) %>% 
    ## force in cols that the analysis functions require
    force_col_to_exist(c("admin_1", "admin_2", 
                         "sex", "linked_case_id",
                         "link_with_the_case")) %>% 
    ## replace NA with "missing"
    mutate(across(.cols = any_of(c("admin_1", "admin_2",
                                   "linked_case_id",
                                   "follow_up_status","follow_up_status_simple")), 
                  .fns = ~ replace_na(.x, "Missing"))) %>% 
    ## convert dates to date
    mutate(across(matches("date"),
                  ~ anytime::anydate(.x))) %>% 
    ## complete followup
    group_split(contact_id) %>%   ## for each group
    map(.f = 
          ~ .x %>% 
          ## add sequence from day after last contact to 14 days after
          complete(follow_up_date = seq.Date(follow_up_start_date[1], 
                                             follow_up_end_date[1], 
                                             by = '1 days'),
                   fill = list(follow_up_status = "Not generated")) %>% 
          ## remove NA followups. Artifact of completion
          filter(!is.na(follow_up_date))  %>% 
          ## remove old out-of-range follow-ups. Assume mistaken
          filter(follow_up_date <= (date_of_last_contact + 14) & 
                   follow_up_date > date_of_last_contact )) %>% 
    ## recombine
    bind_rows() %>% 
    ## follow up day from follow up date
    mutate(follow_up_day = as.numeric(follow_up_date - date_of_last_contact)) %>% 
    ## cascade down constant values 
    group_by(contact_id) %>% 
    mutate(across(.cols = 
                    !matches("follow_up_date|follow_up_status|follow_up_day"),
                  .fns = ~ first(na.omit(.x)))) %>% 
    ungroup() %>% 
    ## for sample df only. need cities and towns
    { if(PARAMS$fake_data == TRUE){
      group_split(., contact_id) %>%
        map(.f = 
              ~ .x %>%  
              mutate(admin_1 = sample( paste0("CITY_", LETTERS[1:10]), size = 1)) %>% 
              mutate(admin_2 = sample( paste0("TOWN_", LETTERS[1:10]), size = 1)) %>% 
              mutate(admin_2 = paste(admin_1, admin_2))) %>% 
        bind_rows()
    }  else {.} 
    } %>% 
    ## remove prepended text from status
    mutate(follow_up_status = sub('.*TYPE_' , '', follow_up_status)) %>%
    mutate(follow_up_status = str_to_sentence(follow_up_status)) %>% 
    ## other modifications for status
    mutate(follow_up_status = recode(follow_up_status, 
                                     "Seen_ok" = "Seen, Ok",
                                     "Seen_not_ok" = "Seen, Not Ok",
                                     "Not_attempted" = "Not attempted", 
                                     "Not_generated" = "Not generated", 
                                     "Not_performed" = "Not performed")) %>% 
    ## shorten
    mutate(follow_up_status_simple = recode(follow_up_status, 
                                            "Seen, Ok" = "Seen",
                                            "Seen, Not Ok" = "Seen", 
                                            "Not attempted" = "Not seen", 
                                            "Missed" = "Not seen", 
                                            "Missing" = "Not seen",
                                            "Not performed" = "Not seen",
                                            ## just for easy reference
                                            "Not generated" = "Not generated")) %>% 
    ## row number for easy tracking
    mutate(row_number = row_number()) %>% 
    ## remove duplicates
    group_by(contact_id, follow_up_date) %>% 
    slice_max(order_by = follow_up_date, n = 1) %>% 
    ungroup()
      
  
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
  
  
  contacts_df_long <- 
    contacts_df_long_transformed %>%
    ## add future follow-up. 
    mutate(follow_up_status = if_else(follow_up_date > todays_date, 
                                      "Future follow-up",
                                      follow_up_status)) %>% 
    mutate(follow_up_status_simple = if_else(follow_up_date > todays_date, 
                                             "Future follow-up",
                                             follow_up_status_simple)) %>% 
    # keep only those for whom follow-up had begun by the date of review
    group_by(row_id) %>% 
    filter(min(follow_up_date) <= todays_date) %>%
    ungroup() %>% 
    # add legend colors
    # legend_df is defined in global.R 
    left_join(legend_df, by = c("follow_up_status" = "breaks"))
  
  ## return
  contacts_df_long
}










