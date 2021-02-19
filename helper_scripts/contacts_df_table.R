contacts_df_table <- 
  function(df){
    df %>% 
      DT::datatable(filter = "top", 
                    extensions = c("Buttons", "FixedHeader", "Scroller"),
                    options = list(searchHighlight = TRUE, 
                                   pageLength = 15, 
                                   scroller = TRUE, 
                                   deferRender = TRUE,
                                   scrollX='600px',
                                   scrollY='300px', 
                                   buttons = list(c("excel", "print"), 
                                                  list(extend = "colvis", target = 0, visible = FALSE)), 
                                   dom = "Bfrtip",
                                   fixedHeader = TRUE), 
                    rownames = FALSE
              )
  }    
