new_contacts_per_day_value_box <-
function(contacts_df_long, todays_date, report_format = "shiny"){
    
      data_to_plot <- 
        contacts_df_long %>%
        filter(follow_up_day == 1) %>% 
        count(follow_up_start_date) %>% 
        complete(follow_up_start_date = seq.Date(from = min(.$follow_up_start_date), 
                                       to = todays_date, 
                                       by = "day"), 
                 fill = list(n = 0))
      
      ## for labels
      cases_last_day <- 
        data_to_plot %>% 
        filter(follow_up_start_date == todays_date) %>% 
        .$n
      
      date_last_day <- 
        todays_date %>% 
        format.Date("%b %d")
      
      ## different output format for shiny vs others
      if (report_format == "shiny"){
        
        highchart_to_plot <- 
          data_to_plot %>% 
          hchart("column", hcaes(x = follow_up_start_date, y = n), name = "No. contacts") %>% 
          hc_size(height = 85) %>% 
          hc_credits(enabled = FALSE) %>% 
          hc_add_theme(hc_theme_sparkline_vb()) 
      
        output_valuebox <- 
          ## valueboxspark is a custom function defined in misc_functions
          valueBoxSpark(
          value = HTML(glue("{cases_last_day} <font size='1'> in past day ({date_last_day}) </font>")),
          title = toupper(glue("New contacts")),
          sparkobj = highchart_to_plot,
          info = "Bars show the no. of new contacts per day (based on first date of follow-up)",
          subtitle = HTML("<font size='1'> </font>"),
          #icon = icon("calendar-day"),
          width = 2,
          color = "aqua",
          href = NULL)
      
      } else {
        
        output_valuebox <- 
          data_to_plot %>% 
          ggplot() + 
          geom_col(aes(x= follow_up_start_date, y = n), fill = "white") + 
          labs(title = "**NEW CONTACTS**", 
               subtitle = glue::glue("**{cases_last_day}** in past day ({date_last_day})"), 
               x = "",
               y = "") + 
          scale_x_date(breaks = c(min(data_to_plot$follow_up_start_date),  
                                  max(data_to_plot$follow_up_start_date)), 
                       labels = function(.x) format.Date(.x, format = "%b %d, '%y")) +
          theme_classic() +
          theme(panel.background = element_rect("#00BAEA"), 
                plot.background = element_rect("#00BAEA"), 
                plot.title = ggtext::element_textbox(color = "white", size = 15),
                plot.subtitle = ggtext::element_textbox(color = "white", size = 13), 
                panel.grid.major = element_blank(), 
                axis.line = element_blank(), 
                axis.text = element_text(size = 10, color = "white", face = "bold"), 
                axis.text.x = element_text(hjust = .8),
                axis.ticks.length = unit(.2, "cm"), 
                axis.ticks = element_line(color = "white", size = 1)) 
      }
      
      
      return(output_valuebox)
      
      
    
  }
