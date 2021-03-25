
# data in 
contacts_df <- 
    readxl::read_excel(here("data/Niger_Contacts_COVID-19_review_27_01_2021.xlsx"), sheet = 2, skip = 2) %>% 
    ## add counter column. 1 for all records. Useful for counting later (where we use 0 for fake records)
    mutate(counter = 1) %>% 
    mutate(row_id = row_number()) %>% 
    arrange(-row_id) %>% 
    ## rename to new names
    rename_with(.cols = any_of(raw_names), .fn = ~ clean_names[which(raw_names == .x)]) 


#write.csv(contacts_df, file = here("data/contacts_df.csv"))


sample_contacts_df <- read_rds(here("data/sample_contacts_df.RDS"))       

preloaded_data_options <- 
    list(`Niger contacts list` = contacts_df, 
         `Sample contacts list`= sample_contacts_df)




library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    uiOutput("data_to_use"),
    
    uiOutput("input_data"), 
    
    uiOutput("columns_to_analyze"), 
    
    uiOutput("analyze_action_bttn"), 
    
    uiOutput("filters"), 
    
    reactableOutput("reactable_table")

   
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    read_file <- reactive({
        
        if (input$data_to_use_id == "Use preloaded data") {
            
            y <- preloaded_data_options[[input$preloaded_data]] %>% as.data.frame()
            
            
        } else if (input$data_to_use_id == "Use uploaded data") {
            
            y <- 
                read_csv(input$uploaded_data$datapath) %>% janitor::clean_names() %>% as.data.frame()
            
        }
        
        return(y)
    })
    
    output$data_to_use <- renderUI({
        radioButtons(inputId = "data_to_use_id", 
                     label = "Input Data", 
                     choices = c("Use uploaded data", 
                                 "Use preloaded data"))
    })
    
    output$input_data <- renderUI({
        
        req(input$data_to_use_id)
        
        if(input$data_to_use_id == "Use uploaded data") {
            fileInput(inputId = "uploaded_data",
                      label = "Upload a csv file", 
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain", 
                                 ".csv"))
        } else if (input$data_to_use_id == "Use preloaded data") {
            
            selectInput("preloaded_data", 
                        label = "Use preloaded data", 
                        choices = c("Niger contacts list", "Sample contacts list"),
                        selected = NULL,
                        multiple = FALSE)
        }
        
    })
    
    output$columns_to_analyze <- renderUI({
        
        if(input$data_to_use_id == "Use uploaded data" & is.null(input$uploaded_data)) {return(NULL )}
        if(input$data_to_use_id == "Use preloaded data" & is.null(input$preloaded_data)) {return(NULL )}
        
        my_data <- read_file()
        
        pickerInput(inputId = "cols_to_analyze",
                    label = "Columns to create filter",
                    choices = names(my_data),
                    multiple = TRUE,
                    selected = names(my_data))
        # }
        
    })
    
    output$analyze_action_bttn <- renderUI({
        
        if(input$data_to_use_id == "Use uploaded data" & is.null(input$uploaded_data)) {return(NULL )}
        if(input$data_to_use_id == "Use preloaded data" & is.null(input$preloaded_data)) {return(NULL )}
        
        actionBttn(inputId = "Analyze", label =  "Analyze")
    })
    
    output$filters <- renderUI({
        
        req(input$Analyze)
        
        my_data <- 
            read_file() %>% 
            # whoever coded this (I took it from shinyr, requires a dataframe, not tibble)
            as.data.frame()
        
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
                                value = c(min(col, na.rm = TRUE), max(col, na.rm = TRUE)))
                }
                
            })
            
        })
        
        lapply(1:length(labels), function(i) {
            uiOutput(labels[[i]])
        })
        
    })
    
    filtered_data <- reactive({
        
        req(input$Analyze)
        req(input$row_id)
        
        
        my_data <- read_file()
        
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
    
    # # rebuilds output$columns_to_analyze, output$filters and filtered_data_dyn() each time "Analyze" button is clicked
    # observeEvent(input$Analyze,{
    #     read_file()
    #     })

    #~~ Data completeness ---------------------------
    source(here("helper_scripts/data_completeness_plot.R"), local = T, encoding = "UTF-8")
    
    output$reactable_table <- 
        renderReactable(
            filtered_data_dyn() %>% 
                reactable()
        )


}

# Run the application 
shinyApp(ui = ui, server = server)

