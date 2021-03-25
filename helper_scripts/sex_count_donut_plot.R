
sex_count_donut_plot <- function(contacts_df){
  
  req(input$Analyze)
  
  x <- c("Sex: ", "Count: ")
  y <- c("{point.sex_contact}", "{point.n}")
  
  tltip <- tooltip_table(x, y)
  
  contacts_df %>% 
    mutate(sex_contact = str_to_upper(sex_contact)) %>% 
    mutate(sex_contact = ifelse(sex_contact %in% c("F", "M"), sex_contact, "Missing" )) %>% 
    count(sex_contact) %>% 
    hchart("pie", hcaes(name = sex_contact, y = n ), 
           innerSize = "40%", 
           dataLabels = list(enabled = TRUE)) %>% 
    hc_title(text= 'Breakdown of contacts by sex') %>% 
    hc_tooltip(useHTML = TRUE,
               headerFormat = "", 
               pointFormat = tltip) 

  }
