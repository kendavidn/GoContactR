reactable_table <- 
  function(contacts_df){
    
    contacts_df %>% 
      reactable(searchable = TRUE,
                striped = TRUE,
                highlight = TRUE,
                filterable = TRUE,
                theme = reactableTheme(stripedColor = "#f0f1fc",
                                       highlightColor = "#DADEFB",
                                       cellPadding = "8px 12px",
                                       style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif")
                                       )
      )
                                                    

  }    
