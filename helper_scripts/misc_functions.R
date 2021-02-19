readFile <- reactive({
  if(input$data_to_use_id == "Use examples") {
    
    description <- input$example_data
    
    x <- valid_sets()
    
    y <- get(x[x$Title == description, ] %>% 
               unique() %>% 
               .[,"Item"] %>% 
               na.omit %>% 
               as.character())
    
    y <- as.data.frame(y)
    
    
  } else {
    
    input_file <- input$file1
    
    input_file_path <- input_file$datapath
    
    x <- read.csv(input_file_path, header = TRUE)
    
    y <- as.data.frame(x)
    
  }
  
  return(y)
  
})
