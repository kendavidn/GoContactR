individual_follow_up_timeline <- function(contacts_df, contacts_df_long){


data_to_plot <- 
  contacts_df_long %>%
  # filter(max(follow_up_date, na.rm = T) > todays_date) %>%
  right_join(contacts_df 
             %>% slice_sample(n = 200),
             by = "row_id"
  ) %>%
  inner_join(legend_df, by = c("follow_up_state_imputed" = "breaks")) %>% 
  select(
    row_id,
    name_last_contact,
    name_first_contact,
    date_last_interaction,
    follow_up_day,
    follow_up_date,
    follow_up_state_imputed, 
    colors
  ) %>% 
  group_by(row_id) %>% 
  mutate(hc_ttip = glue("<b>{name_first_contact} {name_last_contact}</> <br>
                         {format.Date(follow_up_date, format = '%b %d')} (follow up day {follow_up_day}) <br>
                         <b>Status:</b> {follow_up_state_imputed}
                         ")) %>% 
  ungroup() %>% 
  arrange(follow_up_state_imputed) %>%   # arranging is necessary so that that colors are pulled in the right order for highcharter
  mutate(follow_up_date_timestamp = datetime_to_timestamp(follow_up_date))




data_to_plot %>% 
  hchart("scatter", 
         hcaes(x = follow_up_date, y = row_id, 
               color = colors, group = follow_up_state_imputed)) %>% 
  hc_colors(unique(data_to_plot$colors)) %>% 
  hc_plotOptions(series = list(marker = list(radius = 3, 
                                             symbol = "circle"))) %>% 
  hc_xAxis(title = list(text = "Date"),
           min = min(data_to_plot$follow_up_date, na.rm = T)  %>% datetime_to_timestamp(), 
           max = max(data_to_plot$follow_up_date, na.rm = T) %>% datetime_to_timestamp()
  ) %>% 
  hc_yAxis(title = list(text = "Contact ID"),
           min = min(data_to_plot$n, na.rm = T), 
           max = max(data_to_plot$n, na.rm = T)) %>% 
  hc_tooltip(formatter = JS("function(){return(this.point.hc_ttip)}")) %>% 
  hc_plotOptions(scatter = list(stickyTracking = F)) %>% 
  hc_exporting(enabled = TRUE)

}