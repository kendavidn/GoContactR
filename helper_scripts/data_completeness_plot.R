
data_completeness_plot <- function(contacts_df){

  contacts_df %>% 
  mutate(data_completeness_plot_row_id = row_number()) %>%  # long name so it does not conflict
    {if (nrow(.) > 1000) sample_n(., 1000) else .} %>% # sample if too large
  arrange(-data_completeness_plot_row_id) %>% 
  visdat::vis_dat() +
  scale_fill_paletteer_d(palette = "NineteenEightyR::sonny") +
  #my_theme+
  theme(axis.text.x = element_text(angle = 60, hjust = 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) + 
  labs(title = "Summary of uploaded dataframe", 
       subtitle = "Data types and missingness are shown.")+ 
    theme(title = element_text(face = "bold"))
  

}