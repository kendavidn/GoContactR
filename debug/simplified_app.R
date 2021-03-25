
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
  
  uiOutput("analyze_action_bttn"), 
  
  reactableOutput("reactable_table")
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  read_file <- reactive({
    
    req(input$Analyze)
    
    if (input$data_to_use_id == "Use preloaded data") {
      
      y <- preloaded_data_options[[input$preloaded_data]]
      
      
    } else if (input$data_to_use_id == "Use uploaded data") {
      
      y <- read_csv(input$uploaded_data$datapath) %>% janitor::clean_names()
      
    } else {
      y <- NULL
      
    }
    
    return(y)
  })
  
  output$data_to_use <- renderUI({
    radioButtons(inputId = "data_to_use_id", 
                 label = "Input Data", 
                 choices = c("Use preloaded data", 
                             "Use uploaded data"))
  })
  
  output$input_data <- renderUI({

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
  
  output$analyze_action_bttn <- renderUI({
    actionBttn(inputId = "Analyze", label =  "Analyze")
  })
  
  output$reactable_table <- renderReactable({
      read_file() %>% 
        reactable()})
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

