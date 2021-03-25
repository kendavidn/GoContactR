
region_count_sunburst_plot <- function(contacts_df){
  
req(input$Analyze)

contact_regions <-
  contacts_df %>%
  select(admin1_contact, admin2_contact, admin3_contact) %>%
  mutate(across(
    .fns =
      ~ .x %>%
        str_to_lower() %>%
        str_to_title() %>%
        replace_na("NA") %>%
        str_trim()
  )) %>%
  add_count(admin1_contact, name = "n_admin1_contact") %>%
  mutate(pct_admin1_contact = round(100 * n_admin1_contact / nrow(.), 
                                    digits = 2)) %>%
  mutate(admin1_contact = paste0(admin1_contact, 
                                 " (", pct_admin1_contact, "%", ")")) %>%
  group_by(admin1_contact) %>%
  mutate(admin2_contact = fct_lump(other_level = "Autres", 
                                   admin2_contact, prop = 0.01)) %>%
  add_count(admin2_contact, name = "n_admin2_contact") %>%
  mutate(pct_admin2_contact = round(100 * n_admin2_contact / nrow(.), 
                                    digits = 2)) %>%
  mutate(admin2_contact = paste0(admin2_contact, " (", pct_admin2_contact, "%", ")")) %>%
  group_by(admin1_contact, admin2_contact) %>%
  mutate(admin3_contact = fct_lump(other_level = "Autres", admin3_contact, prop = 0.02)) %>%
  add_count(admin3_contact, name = "n_admin3_contact") %>%
  mutate(pct_admin3_contact = round(100 * n_admin3_contact / nrow(.), digits = 2)) %>%
  mutate(admin3_contact = paste0(admin3_contact, " (", pct_admin3_contact, "%", ")")) %>%
  group_by(admin1_contact, admin2_contact, admin3_contact) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  arrange(-n_admin1_contact)
  
  full_palette <- 
    c(russian_violet, cinnabar,
      "#56bfa3", "#f79f57" ,"#7570B3","#56B4E9", #greenblue #lightorange #purplepastel #lightblue
      "#3758a6" , "#CC79A7" , "#91142c", "#63a88a", #darkblue #pinkpurple #wine #pastelgreen
      "#a3b0c4", "#870476", "#479444", "#3cd6d6" ) # grey, # royalpurple #darkgreen # cyan 

  color_df <- 
    data.frame(admin1_contact = unique(contact_regions$admin1_contact)) %>% 
    add_column(color_lvl_1 = full_palette[1:nrow(.)])
  
  contact_regions_list <- 
    contact_regions %>% 
    data_to_hierarchical(group_vars = c(admin1_contact, 
                                        admin2_contact, 
                                        admin3_contact
    ), 
    size_var = n_admin3_contact, 
    colors= color_df$color_lvl_1)
  
  
  x <- c("Region: ", "n = ")
  y <- c("{point.name}", "{point.value}")
  tltip <- tooltip_table(x, y)
  
  
  highchart() %>% 
    hc_chart(type = "sunburst") %>% 
    hc_title(text = "Breakdown of contacts by Region ") %>% 
    #hc_subtitle(text = subtitle, useHTML = TRUE) %>% 
    hc_add_series(data = contact_regions_list,
                  allowDrillToNode = TRUE,
                  levelIsConstant = FALSE,
                  #textOverflow = "clip",
                  levels = list(list(level = 1,
                                     borderWidth = 1, 
                                     dataLabels = list(enabled = TRUE,
                                                       color = "#FFFFFF",
                                                       style = list(fontSize = "14px",
                                                                    #fontWeight = "bold",
                                                                    #textOutline = "white", 
                                                                    opacity = 0.8))), 
                                list(level = 2, 
                                     borderWidth = 0,
                                     dataLabels = list(enabled = TRUE, 
                                                       color = "#FFFFFF",
                                                       style = list(fontSize = "12px", 
                                                                    textOutline = FALSE, 
                                                                    opacity = 0.8))), 
                                list(level = 3,
                                     borderWidth = 0,
                                     dataLabels = list(enabled = TRUE, 
                                                       color = "#FFFFFF",
                                                       style = list(fontSize = "8px", 
                                                                    textOutline = FALSE, 
                                                                    opacity = 0.8))))) %>% 
    hc_plotOptions(sunburst = list(dataLabels = list(enabled = TRUE) )) %>% 
    hc_size(600, 600) %>% 
    hc_tooltip(useHTML = TRUE, 
               headerFormat = "", pointFormat = tltip) 
  
}


