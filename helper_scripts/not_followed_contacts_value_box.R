
not_followed_contacts_value_box <- function(contacts_df, contacts_df_long){
  
  data_to_plot <- 
    contacts_df_long %>%
    # filter(max(follow_up_date, na.rm = T) > todays_date) %>%
    inner_join(contacts_df, 
               by = "row_id"
    ) %>%
    select(
      row_id,
      name_last_contact,
      name_first_contact,
      date_last_interaction,
      follow_up_day,
      follow_up_date,
      follow_up_state_imputed, 
      admin1_contact
    ) %>% 
    mutate(follow_up_state_imputed = recode(follow_up_state_imputed, 
                                            "Completed" = "Followed",
                                            "Developed symptoms" = "Followed",
                                            "Has tested negative" = "Followed",
                                            "Has tested positive" = "Followed",
                                            "Lost to follow-up" = "Not followed",
    )) %>% 
    filter(follow_up_state_imputed == "Not followed") %>% 
    # last 28 days
    filter(follow_up_date < todays_date & 
             follow_up_date+0 > todays_date-28
    ) %>%
    group_by(follow_up_date) %>% 
    count() %>% 
    ungroup() %>% 
    arrange(follow_up_date) 
  
  
  highchart_to_plot <- 
    data_to_plot %>% 
    hchart("area", hcaes(x = follow_up_date, y = n), name = "Contacts not followed") %>% 
    hc_size(height = 100) %>% 
    hc_credits(enabled = FALSE) %>% 
    hc_add_theme(hc_theme_sparkline_vb()) 
  
  value_box_out <- valueBoxSpark(
    value = sum(data_to_plot$n),
    title = toupper("Contacts not followed, last 4 weeks"),
    sparkobj = highchart_to_plot,
    subtitle = tagList(HTML("&uarr;"), "XX% last 2 weeks"),
    info = "The number of active contacts who were not followed in the last 28 days",
    icon = icon("times"),
    width = 4,
    color = "yellow",
    href = NULL
  )
  
  return(value_box_out)
  
  
}