
active_contacts_value_box <- function(contacts_df, contacts_df_long){
  
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
    ) %>% 
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
    hchart("area", hcaes(x = follow_up_date, y = n), name = "Active contacts") %>% 
    hc_size(height = 100) %>% 
    hc_credits(enabled = FALSE) %>% 
    hc_add_theme(hc_theme_sparkline_vb()) 
  
  value_box_out <- valueBoxSpark(
    value = sum(data_to_plot$n),
    title = toupper("Active contacts, last 4 weeks"),
    sparkobj = highchart_to_plot,
    subtitle = tagList(HTML("&uarr;"), "XX% last 2 weeks"),
    info = "The number of active contacts reported in the last 28 days, whether or not they were followed",
    icon = icon("project-diagram"),
    width = 4,
    color = "purple",
    href = NULL
  )
  
  return(value_box_out)
  

}

# 
# 
# df <- cars %>% 
#   mutate(x = speed, y  = dist)
# 
# hc <- hchart(df, "area", hcaes(x, y), name = "lines of code")  %>%
#   hc_size(height = 100) %>%
#   hc_credits(enabled = FALSE) %>%
#   hc_add_theme(hc_theme_sparkline_vb())
# 
# hc2 <- hchart(df, "line", hcaes(x, y), name = "Distance")  %>%
#   hc_size(height = 100) %>%
#   hc_credits(enabled = FALSE) %>%
#   hc_add_theme(hc_theme_sparkline_vb())
# 
# hc3 <- hchart(df, "column", hcaes(x, y), name = "Daily amount")  %>%
#   hc_size(height = 100) %>%
#   hc_credits(enabled = FALSE) %>%
#   hc_add_theme(hc_theme_sparkline_vb())
# 
# vb <- valueBoxSpark(
#   value = "1,345",
#   title = toupper("Lines of code written"),
#   sparkobj = hc,
#   subtitle = tagList(HTML("&uarr;"), "25% Since last day"),
#   info = "This is the lines of code I've written in the past 20 days! That's a lot, right?",
#   width = 4,
#   color = "teal",
#   href = NULL
# )
# 
# vb2 <- valueBoxSpark(
#   value = "1,345 KM",
#   title = toupper("Distance Traveled"),
#   sparkobj = hc2,
#   subtitle = tagList(HTML("&uarr;"), "25% Since last month"),
#   info = "This is the lines of code I've written in the past 20 days! That's a lot, right?",
#   icon = icon("plane"),
#   width = 4,
#   color = "red",
#   href = NULL
# )
# 
# vb3 <- valueBoxSpark(
#   value = "1,3 Hrs.",
#   title = toupper("Thinking time"),
#   sparkobj = hc3,
#   subtitle = tagList(HTML("&uarr;"), "5% Since last year"),
#   info = "This is the lines of code I've written in the past 20 days! That's a lot, right?",
#   icon = icon("hourglass-half"),
#   width = 4,
#   color = "yellow",
#   href = NULL
# )

