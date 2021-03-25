

active_contacts_per_district_stack_relative_highcharter <- 
  function(contacts_df, contacts_df_long){
    

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
    admin1_contact, 
    admin2_contact
  ) %>% 
  filter(follow_up_day == 1) %>% 
  group_by(follow_up_date, admin1_contact) %>% 
  count(admin1_contact, admin2_contact) %>% 
  ungroup() %>% 
  bind_rows(tibble(follow_up_date =  seq.Date(from = min(.$follow_up_date), 
                                              to = max(.$follow_up_date), 
                                              by = "day"), 
                   admin1_contact = "temporary",
                   admin2_contact = "temporary"
  )) %>% 
  complete(follow_up_date, admin1_contact, fill = list(n = 0)) %>% 
  filter(admin1_contact != "temporary" & !is.na(admin1_contact)) %>% 
  group_by(admin1_contact) %>% 
  complete(follow_up_date, admin2_contact, fill = list(n = 0)) %>%
  filter(admin2_contact != "temporary" & !is.na(admin2_contact)) %>% 
  group_by(admin1_contact, follow_up_date) %>% 
  mutate(prop = n/sum(n)) %>% 
  ungroup() %>% 
  arrange(admin1_contact, admin2_contact, follow_up_date)





map(unique(data_to_plot$admin1_contact), function(x){
  
  
  data_to_plot %>% 
    filter(admin1_contact == x) %>% 
    # region with the most cases should be at the top
    group_by(admin2_contact) %>% 
    mutate(admin2_contact_total = sum(n)) %>% 
    ungroup() %>% 
    mutate(admin2_contact = fct_rev(fct_reorder(admin2_contact, admin2_contact_total))) %>% 
    hchart("column", hcaes(x = follow_up_date, y = prop, group = admin2_contact)) %>% 
    hc_title(text = x) %>% 
    hc_yAxis(visible = TRUE) %>% 
    hc_plotOptions(column = list(stacking = "normal", 
                                 pointPadding = 0, 
                                 groupPadding = 0, 
                                 borderWidth= 0.05,
                                 stickyTracking = T
    )) %>% 
    hc_plotOptions(column = list(states = list(inactive = list(opacity = 0.7)))) %>% 
    hc_xAxis(title = list(text = "Date")) %>% 
    hc_yAxis(title = list(text = "Daily Number of active contacts")) %>% 
    hc_exporting(enabled = TRUE) %>% 
    hc_xAxis(title = list(text = "Date"),
             min = min(data_to_plot$follow_up_date, na.rm = T)  %>% datetime_to_timestamp(), 
             max = max(data_to_plot$follow_up_date, na.rm = T) %>% datetime_to_timestamp()
    ) %>% 
    hc_yAxis(title = list(text = "n"),
             min = min(data_to_plot$prop, na.rm = T), 
             max = max(data_to_plot$prop, na.rm = T)) 
}) %>% 
  hw_grid(rowheight = 300, ncol = 2, browsable = T)

}
