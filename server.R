
## read in clean names and replace
raw_and_clean_names <- readxl::read_excel(here("data/Niger_Contacts_COVID-19_review_27_01_2021.xlsx"), sheet = 3) 
raw_names <- raw_and_clean_names$raw_names
clean_names <- raw_and_clean_names$clean_names

# data in 
contacts_df <- 
  readxl::read_excel(here("data/Niger_Contacts_COVID-19_review_27_01_2021.xlsx"), sheet = 2, skip = 2) %>% 
  ## add counter column. 1 for all records. Useful for counting later (where we use 0 for fake records)
  mutate(counter = 1) %>% 
  mutate(row_id = row_number()) %>% 
  ## rename to new names
  rename_with(.cols = any_of(raw_names), .fn = ~ clean_names[which(raw_names == .x)]) 


contacts_df <- 
  readxl::read_excel(here("data/Niger_Contacts_COVID-19_review_27_01_2021.xlsx"), sheet = 2, skip = 2) %>% 
  ## add counter column. 1 for all records. Useful for counting later (where we use 0 for fake records)
  mutate(counter = 1) %>% 
  mutate(row_id = row_number()) %>% 
  ## rename to new names
  rename_with(.cols = any_of(raw_names), .fn = ~ clean_names[which(raw_names == .x)]) 

         
sample_contacts_df <- read_rds(here("data/sample_contacts_df.RDS"))       
  
preloaded_data <- 
  list(`Niger contacts list` = contacts_df, 
        `Sample contacts list`= sample_contacts_df)


server <- function(input, output) {  
  
  #~~ Data plots ---------------------------

  #~~ Donut plots ---------------------------
  source(here("helper_scripts/sex_count_donut_plot.R"))
  output$sex_count_donut_plot1 <- renderHighchart(contacts_df %>% sex_count_donut_plot())
  output$sex_count_donut_plot2 <- renderHighchart(contacts_df %>% sex_count_donut_plot())
  output$sex_count_donut_plot3 <- renderHighchart(contacts_df %>% sex_count_donut_plot())
  
  #~~ Sunburst and click capture ---------------------------
  source(here("helper_scripts/region_count_sunburst_plot.R"))
  output$region_count_sunburst_plot <- renderHighchart(contacts_df %>% region_count_sunburst_plot())
  output$click_capture <- renderText(input$click_capture)
  
  #~~ DT Table ---------------------------
  source(here("helper_scripts/contacts_df_table.R"))
  output$contacts_df_table <- renderDataTable(contacts_df %>% contacts_df_table())
  
  #~~ Data completeness ---------------------------
  output$data_completeness_plot <- 
    renderPlot(
      filtered_data_dyn() %>% 
        #slice_sample(n = 2000) %>% 
        visdat::vis_dat() + 
        scale_fill_paletteer_d(palette = "NineteenEightyR::sonny", drop = T) +
        labs(title = "Column types and missingness for your dataset.",
             subtitle = "Please ensure that your data has the needed variables (see manual)") +
        my_theme+
        theme(axis.text.x = element_text(angle = 60, hjust = 0)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, NA))
    )
  
  
  output$data_completeness_plot2 <- 
    renderPlot(
      filtered_data_dyn() %>% 
        #slice_sample(n = 2000) %>% 
        visdat::vis_dat() + 
        scale_fill_paletteer_d(palette = "NineteenEightyR::sonny", drop = T) +
        labs(title = "Column types and missingness for your dataset.",
             subtitle = "Please ensure that your data has the needed variables (see manual)") +
        my_theme+
        theme(axis.text.x = element_text(angle = 60, hjust = 0)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, NA))
    )
  
  
  #~~ Reactives ---------------------------
  
  
  readFile <- reactive({
    if(input$data_to_use_id == "Use pre-loaded data") {
      
      df_name <- input$example_data
      
      y <- preloaded_data[[df_name]]
      
      y <- as.data.frame(y)
      
      
    } else {
      
      input_file <- input$file1
      
      input_file_path <- input_file$datapath
      
      x <- read.csv(input_file_path, header = TRUE)
      
      y <- as.data.frame(x)
      
    }
    
    return(y)
    
  })
  
  filtered_data <- reactive({
    
    req(input$Analyze)
    
    my_data <- readFile()
    
    my_inputs <- input$cols_to_analyze
    
    all_possible_inputs <- names(my_data)
    
    matching <- all_possible_inputs %in% my_inputs
    
    cols_to_filter <- all_possible_inputs[matching]
    
    temp <- my_data
    
    for(j in 1:length(cols_to_filter)) {
      
      col <- my_data[ ,my_inputs[j]]
      
      if(is.character(col)) {
        temp <- temp[temp[,cols_to_filter[j]] %in% input[[cols_to_filter[j]]], ]
      } else if(is.numeric(col)) {
        temp <- temp[temp[,cols_to_filter[j]] >= input[[cols_to_filter[j]]][1], ]
        temp <- temp[temp[,cols_to_filter[j]] <= input[[cols_to_filter[j]]][2], ]
      }
    }
    
    return(temp)
  })
  
  filtered_data_dyn <- reactive({
    filtered_data()
  })
  
  
  initial_analysis <- reactive({
    temp <- readFile()
    getDataInsight(temp)
  })
  
  observeEvent(input$Analyze,{
    readFile()
  })
  
  # Switch controlbar menu based on sidebar item value. Moreover
  # if the sidebar menu item is 2, the controlbar opens
  observeEvent(input$sidebarMenu, {
    idx <- strsplit(input$sidebarMenu, "_")[[1]][2]
    if (idx == 2) {
      updateControlbarMenu("controlbarMenu", selected = idx)
    }
  })
  
  
  #~~ Right sidebar controls ---------------------------
  
  output$data_to_use <- renderUI({
    radioButtons(inputId = "data_to_use_id", inline = T,
                 label = "Linelist to analyse", 
                 choices = c("Use pre-loaded data", 
                             "Upload"))
  })
  
  output$input_data <- renderUI({
    
    if(input$data_to_use_id == "Upload") {
      fileInput(inputId = "file1",
                label = "Upload a csv file", 
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain", 
                           ".csv"))
    } else {
      
      selectInput("example_data", 
                  label = "Use pre-loaded dataset", 
                  choices = c("Niger contacts list", "Sample contacts list"),
                  selected = "Niger contacts list",
                  multiple = FALSE)
    }
    
  })
  
  output$columns_to_analyze <- renderUI({
    
    if(input$data_to_use_id == "Upload" & 
       is.null(input$file1)) {
      return(NULL)
    }
    
    my_data <- readFile()
    
    pickerInput(inputId = "cols_to_analyze",
                label = "Columns to create filter",
                choices = names(my_data),
                multiple = TRUE,
                selected = names(my_data),
                
    )
    # }
    
  })
  
  output$analyze_action_bttn <- renderUI({
    # req(input$file1 | input$example_data)
    actionBttn(inputId = "Analyze", 
               #style = "stretch",
               #style = "minimal", 
               label = "Create filters")
  })
  
  output$filters <- renderUI({
    
    req(input$Analyze)
    my_data <- readFile() %>% type_convert()
    selected_cols <- input$cols_to_analyze
    
    labels <- lapply(1:length(selected_cols), FUN = function(x){
      selected_cols[x]
    })
    
    ids <- lapply(1:length(selected_cols), FUN = function(x){
      paste0(selected_cols[x])
    })
    
    choices <- lapply(1:length(selected_cols), FUN = function(x){
      unique(my_data[ ,selected_cols[x]]) 
    })
    
    lapply(1:length(labels), function(i) {
      output[[labels[[i]]]] <- renderUI({
        col <- my_data[ ,selected_cols[i]]
        if(is.character(col) | is.factor(col)) {
          if(is.factor(col)){
            ch <- choices[[i]] %>% levels()
          } else {
            ch <- choices[[i]]
          }
          pickerInput(ids[[i]],
                      label = labels[[i]],
                      choices = ch,
                      selected = ch,
                      multiple = TRUE)
        } else if(is.numeric(col)) {
          sliderInput(ids[[i]],
                      label = labels[[i]],
                      min = min(col, na.rm = TRUE),
                      max = max(col, na.rm = TRUE),
                      value = c(min(col, na.rm = TRUE), max(col, na.rm = TRUE)),
                      ticks = FALSE
                      )
        }
        
      })
      
    })
    
    lapply(1:length(labels), function(i) {
      uiOutput(labels[[i]])
    })
    
  })
  
  
  






  
  # output$avg_msg_length_barplot <- renderHighchart(messages_df() %>% avg_msg_length_barplot())
  # 
  # output$msg_freq_over_time_lineplot <- renderHighchart(messages_df() %>% msg_freq_over_time_lineplot())
  # 
  # output$msg_freq_weekly_radialplot <- renderHighchart(messages_df() %>% msg_freq_weekly_radialplot())
  # 
  # output$top_words_barplot <- renderHighchart(messages_df() %>% top_words_barplot(max_nb = 30)) 
  # 
  # output$top_ngrams_barplot <- renderHighchart(messages_df() %>% top_ngrams_barplot(max_nb = 30)) 
  # 
  # 
  # 
}
