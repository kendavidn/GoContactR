
data_cardinality_plot <- function(contacts_df){

  contacts_df %>% 
    {if (nrow(.) > 1000) sample_n(., 1000) else .} %>% # sample if too large
    inspectdf::inspect_cat() %>% 
    show_plot(col_palette = 4)+ 
    labs(title = "Frequency of categorical levels in loaded dataset")
  
}