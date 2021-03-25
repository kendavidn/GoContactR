
contact_type_count_sunburst_plot <- function(contacts_df){
  
  req(input$Analyze)
  
  
  contact_types <- 
    contacts_df %>% 
    select(type_contact, relation_contact_case) %>% 
    mutate(across(.fns = 
                    ~.x %>% 
                    str_to_lower() %>% 
                    str_to_title() %>% 
                    replace_na("NA") %>% 
                    str_replace_all(" \\s*\\([^\\)]+\\)", "") %>% 
                    str_trim())) %>% 
    add_count(type_contact, name = "n_type_contact") %>% 
    mutate(pct_type_contact = round( 100 * n_type_contact/nrow(.), digits = 1 )) %>% 
    group_by(type_contact) %>% 
    mutate(relation_contact_case = fct_lump(other_level = "Autres", relation_contact_case, prop = 0.01)) %>% 
    add_count(relation_contact_case, name = "n_relation_contact_case") %>% 
    mutate(pct_relation_contact_case = round( 100 * n_relation_contact_case/nrow(.), digits = 1 )) %>% 
    group_by(type_contact, relation_contact_case) %>% 
    slice_head(n = 1) %>% 
    ungroup() %>% 
    mutate(type_contact = paste0(type_contact, " (", pct_type_contact, "%", ")")) %>% 
    mutate(relation_contact_case = paste0(relation_contact_case, " (", pct_relation_contact_case, "%", ")")) %>% 
    ungroup() 
  
  
  full_palette <- c("#332859",(paletteer_d("awtools::a_palette") %>% 
                                 as.character() %>%
                                 str_sub(1,7)
  )
  )
  
  color_df <- 
    data.frame(type_contact = levels(as.factor(contact_types$type_contact))) %>% 
    add_column(color_lvl_1 = c(space_cadet, "#459ac4"))
  
  
  contact_types_list <- 
    contact_types %>% 
    data_to_hierarchical(group_vars = c(type_contact, relation_contact_case), size_var = n_relation_contact_case, 
                         colors = c(space_cadet, "#459ac4"))
  
  
  x <- c("Type: ", "n = ")
  y <- c("{point.name}", "{point.value}")
  tltip <- tooltip_table(x, y)
  
  
  highchart() %>% 
    hc_chart(type = "sunburst") %>% 
    hc_title(text = "Breakdown of contacts by contact type") %>% 
    #hc_subtitle(text = subtitle, useHTML = TRUE) %>% 
    hc_add_series(data = contact_types_list,
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
    hc_size(400, 400) %>% 
    hc_tooltip(useHTML = TRUE,
               headerFormat = "", 
               pointFormat = tltip) 
  
}