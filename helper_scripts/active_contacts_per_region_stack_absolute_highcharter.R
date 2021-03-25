
active_contacts_per_region_stack_absolute_highcharter <- 
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
    admin1_contact
  ) %>% 
  group_by(follow_up_date, admin1_contact) %>% 
  count(admin1_contact) %>% 
  ungroup() %>% 
  arrange(admin1_contact, follow_up_date) %>% 
  bind_rows(tibble(follow_up_date =  seq.Date(from = min(.$follow_up_date), 
                                              to = max(.$follow_up_date), 
                                              by = "day"), 
                   admin1_contact = "temporary"
  )) %>% 
  complete(follow_up_date, admin1_contact, fill = list(n = 0)) %>% 
  filter(admin1_contact != "temporary") %>% 
  ungroup() %>% 
  # region with the most cases should be at the top
  group_by(admin1_contact) %>% 
  mutate(total = sum(n)) %>% 
  ungroup() %>% 
  mutate(admin1_contact = fct_rev(fct_reorder(admin1_contact, total)))



data_to_plot %>% 
  hchart("column", hcaes(x = follow_up_date, y = n, group = admin1_contact)) %>% 
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
  hc_exporting(enabled = TRUE)

}