
region_count_sunburst_plot <- function(contacts_df){
  
  
  x <- c("Sex: ", "Count: ")
  y <- c("{point.sex_contact}", "{point.n}")
  tltip <- tooltip_table(x, y)
  
  click_capture_function <- JS("function(event) 
                               { Shiny.onInputChange('click_capture', event.point.name);}")
  
  
  dd <- contacts_df %>% 
    slice_sample(n = 1000) %>% 
    select(admin1_contact, admin2_contact, admin3_contact) %>% 
    mutate(across(.fns = 
                    ~.x %>% 
                    str_to_lower() %>% 
                    str_to_title() %>% 
                    replace_na("NA") %>% 
                    str_trim())) %>% 
    group_by(admin1_contact, admin2_contact) %>% 
    count(admin3_contact) %>% 
    ungroup() %>% 
    data_to_hierarchical(group_vars = c(admin1_contact, admin2_contact, admin3_contact), size_var = n)
 
highchart() %>% 
  hc_chart(type = "sunburst") %>% 
  hc_title(text = "Breakdown of contacts by location") %>% 
  #hc_subtitle(text = subtitle, useHTML = TRUE) %>% 
  hc_add_series(data = dd,
                allowDrillToNode = TRUE,
                levelIsConstant = FALSE,
                #textOverflow = "clip",
                levels = list(list(level = 1,
                                   borderWidth = 1, 
                                   dataLabels = list(enabled = TRUE,
                                                     #color = "#000000",
                                                     style = list(fontSize = "12px",
                                                                  fontWeight = "bold",
                                                                  textOutline = "white", 
                                                                  opacity = 0.8))), 
                              list(level = 2, 
                                   borderWidth = 0,
                                   dataLabels = list(enabled = TRUE, 
                                                     color = "#FFFFFF",
                                                     style = list(fontSize = "10px", 
                                                                  textOutline = FALSE, 
                                                                  opacity = 0.8))), 
                              list(level = 3,
                                   borderWidth = 0,
                                   dataLabels = list(enabled = TRUE, 
                                                     color = "#FFFFFF",
                                                     style = list(fontSize = "8px", 
                                                                  textOutline = FALSE, 
                                                                  opacity = 0.8))))) %>% 
  hc_size(600, 600) %>% 
  hc_plotOptions(sunburst = list(events = list(click = click_capture_function)))



  
}


