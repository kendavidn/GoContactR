input <- list()
input$snake_plot_slider <- c(4,53)


todays_date_reactive <- 
  function()return(todays_date)


read_file <- 
  function(){
    
    return(list(contacts_df_raw =contacts_df_raw,
                contacts_df = contacts_df,
                contacts_df_long = contacts_df_long))
    
    
  }

todays_date <- as.Date("2021-03-01")
