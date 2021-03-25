followed_vs_non_followed_highcharter_line <- function(contacts_df, contacts_df_long){
  

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
  filter(follow_up_state_imputed != "Missing status") %>% 
  group_by(follow_up_date) %>% 
  count(follow_up_state_imputed) %>% 
  #filter(follow_up_state_imputed %in% c("Followed", "Not followed")) %>% 
  ungroup() %>% 
  complete(follow_up_date, follow_up_state_imputed, fill = list(n = 0)) %>% 
  # replace 0s with NA since for Upcoming follow ups before today's date
  # and replace 0s with NA for past follow ups after today's date
  mutate(n = ifelse(follow_up_state_imputed == "Upcoming follow-up" & 
                      follow_up_date <= todays_date, 
                    NA_integer_, 
                    n)) %>% 
  mutate(n = ifelse(follow_up_state_imputed %in% c("Followed", "Not followed") & 
                      follow_up_date > todays_date, 
                    NA_integer_, 
                    n)) %>% 
  mutate(prop = n/sum(n)) %>% 
  mutate(hc_ttip = glue("{format.Date(follow_up_date,  format = '%a %b %d, \\'%y')}<br>
                        <b>Contacts {str_to_lower(follow_up_state_imputed)}:</b> {n}
                        ")) %>% 
  inner_join(legend_df, by = c("follow_up_state_imputed" = "breaks")) %>% 
  arrange(follow_up_state_imputed)   # arranging is necessary so that that colors are pulled in the right order for highcharter



data_to_plot %>% 
  hchart("areaspline", hcaes(x = follow_up_date, y = n, 
                             #color = follow_up_state_imputed, 
                             group = follow_up_state_imputed), 
         fillOpacity = 0.35) %>% 
  hc_colors(unique(data_to_plot$colors)) %>% 
  hc_plotOptions(areaspline = list(series = list(fillOpacity = 0.1),
                                   marker = list(enabled = TRUE, 
                                                 radius = 3, 
                                                 symbol = "circle"),
                                   states = list(inactive = list(opacity = 0.4)))) %>% 
  hc_tooltip(formatter = JS("function(){return(this.point.hc_ttip)}")) %>% 
  hc_xAxis(title = list(text = "Date")) %>% 
  hc_yAxis(title = list(text = "Number of contacts"))

}