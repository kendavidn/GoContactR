options(shiny.maxRequestSize=30*1024^2)

server <- function(input, output) {  
  
  
  source(here("helper_scripts/server_functions.R"), local = T)
  
  source(here(paste0("helper_scripts/server_functions_for_",PARAMS$country_code), local = T))
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #   load_data_tab ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # ~~ data_to_use_picker ---------------------------
  if (PARAMS$country_code == "CIV") {

  output$data_to_use_picker <- renderUI({
    radioButtons(inputId = "data_to_use", 
                 label = "Input Data", 
                 choices = c("Use preloaded data", 
                             "Use uploaded data"))
  })
  
  } 
  # else if (PARAMS$country_code == "UGA"){
  #   
  #   output$data_to_use_picker <- renderUI({
  #     radioButtons(inputId = "data_to_use", 
  #                  label = "Input Data", 
  #                  choices = c("Connect to Go.Data"))
  #   })
  #   
  #   
  # }
  
  # ~~ data_to_use_input ---------------------------
  
  output$data_to_use_input <- renderUI({
    
    req(input$data_to_use)
    
    if (input$data_to_use == "Use preloaded data") {
      
      selectInput("preloaded_data_choice", 
                  label = "Use preloaded data", 
                  choices = c("Sample tracing data"
                              #, 
                              #"Sample contacts list"
                  ),
                  selected = NULL,
                  multiple = FALSE)
      
    } else if (input$data_to_use == "Use uploaded data") {
      
      tagList(fileInput(inputId = "uploaded_data_contacts_list",
                label = "Upload the list of contacts", 
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain", 
                           ".csv", 
                           ".xlsx",
                           ".xls")), 
              fileInput(inputId = "uploaded_data_follow_up_list",
                        label = "Upload the follow-up list", 
                        multiple = FALSE,
                        accept = c("text/csv",
                                   "text/comma-separated-values,text/plain", 
                                   ".csv", 
                                   ".xlsx",
                                   ".xls")))
    } else if (input$data_to_use == "Connect to Go.Data"){
      
      tagList(textInput("go_data_url",
                        "URL for your instance:",
                        value = "https://godata-r13.who.int/"), 
              textInput("go_data_username", 
                        "Username:",
                        value = "godata_api@who.int"),
              passwordInput("go_data_password", 
                            "Password:"), 
              textInput("go_data_outbreak_id", 
                        "Outbreak ID:", 
                        value = "3b5554d7-2c19-41d0-b9af-475ad25a382b"),
              actionBttn("go_data_request_access_button",
                         "Request access", 
                         style = "jelly", 
                         color = "primary"
                         ), 
              uiOutput("access_granted")
              )
      
      }  
  })
  
  output$access_granted <- renderUI({
    
    req(input$go_data_url)
    req(input$go_data_username)
    req(input$go_data_password)
    req(input$go_data_outbreak_id)
    req(input$go_data_request_access_button)
    
    if(is.character(request_access_reactive())){
      c("Successful!")
    } else {
      c("Access not permitted. Try again or contact developers.")
    }
    
    
  })
    
  
  
  
  
  # ~~~~ read_file_raw_reactive ----
  read_file_raw_reactive <- reactive({
    
    req(input$data_to_use)
    req(input$analyze_action_bttn)

    read_file_raw()
    
  })
  
  # ~~~~ read_file_transformed_reactive ----
  

  read_file_transformed_reactive <- reactive({
    
    req(input$data_to_use)
    req(input$analyze_action_bttn)
    
    read_file_transformed(tracing_data_raw = read_file_raw_reactive())
    
    
  })
  
  
  
  # ~~~~ read_file_filtered_reactive ----
  
  
  ## can't be inside of a function because the input[[cols_to_filter]] elements would not work
  read_file_filtered_reactive <- reactive({

    req(input$analyze_action_bttn)
    req(input$filter_or_not)
    
    read_file_filtered()
      

  })
  


  # ~~ analyze_action_bttn ---------------------------
  
  output$analyze_action_bttn <- renderUI({
    
    req(input$data_to_use)
    
    if(input$data_to_use == "Use uploaded data") {
      req(input$uploaded_data_contacts_list)
      req(input$uploaded_data_follow_up_list)
      
    }
    
    if(input$data_to_use == "Use preloaded data") {
      req(input$preloaded_data_choice)
    }
    
    if(input$data_to_use == "Connect to Go.Data") {
      req(input$go_data_url)
      req(input$go_data_username)
      req(input$go_data_password)
      req(input$go_data_outbreak_id)
      req(input$go_data_request_access_button)
      #shiny::validate(need(is.character(request_access_reactive())), message = FALSE, label = "Proceed to analysis" )
  
    }
    
    tagList(HTML("<p style='font-size:4px'>  <br><br>  </p>"),
            
            actionBttn(inputId = "analyze_action_bttn", label = "Analyze", 
               style = "jelly", color = "primary"), 
            
            HTML("<br>
            <span style='color: rgb(97, 189, 109);'>ℹ:</span>
            <font size='1'>
            After analyses have been triggered once, 
            the app must be reloaded before triggering again on a new dataset.
            </font>")
    )
  })
  
  # ~~ action button observer ---------------------------
  
  observeEvent(input$analyze_action_bttn, {
    read_file_transformed_reactive()
  })
  
  
  
   #~~ data_completeness_plot ---------------------------
  output$data_completeness_plot <- 
    renderPlot({
      req(read_file_transformed_reactive())
      
      if(input$data_to_use == "Use uploaded data") {
        req(input$uploaded_data_contacts_list)
        req(input$uploaded_data_follow_up_list)
        
      }
      
      if(input$data_to_use == "Use preloaded data") {
        req(input$preloaded_data_choice)
      }
      
      
      read_file_transformed_reactive() %>% 
      data_completeness_plot()
    }) 

  #~~ data_cardinality_plot ---------------------------
  output$data_cardinality_plot <- 
    renderPlot({
      req(read_file_transformed_reactive())
      
      if(input$data_to_use == "Use uploaded data") {
        req(input$uploaded_data_contacts_list)
        req(input$uploaded_data_follow_up_list)
        
      }
      
      if(input$data_to_use == "Use preloaded data") {
        req(input$preloaded_data_choice)
      }
      
      read_file_transformed_reactive() %>% 
      data_cardinality_plot()})
  
  #~~ reactable_table ---------------------------
  output$reactable_table <- 
    renderReactable({
      req(read_file_transformed_reactive())
      
      if(input$data_to_use == "Use uploaded data") {
        req(input$uploaded_data_contacts_list)
        req(input$uploaded_data_follow_up_list)
        
      }
      
      if(input$data_to_use == "Use preloaded data") {
        req(input$preloaded_data_choice)
      }
      
      read_file_transformed_reactive() %>% 
        reactable_table()})
  

  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #   main_tab ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # ~~~~ ouput$filters----
  
  output$filters <- renderUI({
    
    req(input$analyze_action_bttn)
    req(input$select_date_of_review)
    req(input$filter_or_not)
    
    if ((!is.null(input$filter_or_not)) && input$filter_or_not == "Yes"){
      
    my_data <- 
      read_file_transformed_reactive() %>% 
      as.data.frame() %>%  ## not sure why but tibble doesnt work
      select(-any_of(c("follow_up_date", 
                       "first_name", 
                       "last_name", 
                       "row_id", 
                       "row_number", 
                       "sort_number"))) %>% 
      janitor::remove_constant() 
      
    
    selected_cols <- names(my_data)
    
    na_cols <- 
      my_data %>% 
      select(where(~ any(is.na(.x))  )) %>% 
      ## there are some columns which, although may come in as NA in the read_file_transformed_reactive(),
      ## will be filled before being passed along to read_file_filtered(). 
      ## therefore, we do not need to show NAs for these columns
      select(-(any_of(c("follow_up_day", 
                        "follow_up_status",
                        "follow_up_status_simple",
                        "follow_up_start_date"
                        ) ))) %>% 
      names()
    
    #browser()
    
    labels <- lapply(1:length(selected_cols), FUN = function(x){
      selected_cols[x]
    })
    
    choices <- lapply(1:length(selected_cols), FUN = function(x){
      unique(my_data[ ,selected_cols[x]]) 
    })
    
    
    ## first we render each ui in the list
    lapply(1:length(labels), function(i) {
      
      output[[labels[[i]]]] <- renderUI({
        col <- my_data[ ,selected_cols[i]]
        
        
        ## factor columns
       if (is.factor(col)) {
          
         if (any(is.na(col)) & labels[[i]] %in% na_cols){
         tagList(
             
          pickerInput(labels[[i]],
                      label = labels[[i]],
                      choices = na.omit(levels(choices[[i]])),
                      selected = na.omit(levels(choices[[i]])),
                      options = list(`actions-box` = TRUE),
                      multiple = TRUE), 
          
          checkboxInput(paste0("na_", labels[[i]]),
                        label = paste0("Include contacts w. missing values for ", labels[[i]], "?"), 
                        value = TRUE)
          )
         } else {
           pickerInput(labels[[i]],
                       label = labels[[i]],
                       choices = na.omit(levels(choices[[i]])),
                       selected = na.omit(levels(choices[[i]])),
                       options = list(`actions-box` = TRUE),
                       multiple = TRUE)
         }
          
         ## character columns
        } else if (is.character(col)) {
          
          
          if (any(is.na(col)) & labels[[i]] %in% na_cols){
            tagList(pickerInput(labels[[i]],
                      label = labels[[i]],
                      choices = na.omit(choices[[i]]),
                      selected = na.omit(choices[[i]]),
                      options = list(`actions-box` = TRUE),
                      multiple = TRUE), 
                    checkboxInput(paste0("na_", labels[[i]]),
                                  label = paste0("Include contacts w. missing values for ", labels[[i]], "?"), 
                                  value = TRUE)
            )
          } else {
            pickerInput(labels[[i]],
                                label = labels[[i]],
                                choices = na.omit(choices[[i]]),
                                selected = na.omit(choices[[i]]),
                                options = list(`actions-box` = TRUE),
                                multiple = TRUE)
          }
          
          ## numeric columns 
        } else if (is.numeric(col)) {
         if (any(is.na(col)) & labels[[i]] %in% na_cols){
            tagList(sliderInput(labels[[i]],
                      label = labels[[i]],
                      min = min(col, na.rm = TRUE),
                      max = max(col, na.rm = TRUE),
                      value = c(min(col, na.rm = TRUE), max(col, na.rm = TRUE))), 
                    
                    checkboxInput(paste0("na_", labels[[i]]),
                                  label = paste0("Include contacts w. missing values for ", labels[[i]], "?"), 
                                  value = TRUE)
                    
            )
          } else {
            sliderInput(labels[[i]],
                        label = labels[[i]],
                        min = min(col, na.rm = TRUE),
                        max = max(col, na.rm = TRUE),
                        value = c(min(col, na.rm = TRUE), max(col, na.rm = TRUE)))
            
            }
          
          ## date columns 
        } else if (lubridate::is.Date(col)) {
          
        if (any(is.na(col)) & labels[[i]] %in% na_cols){
            tagList(
          dateRangeInput(labels[[i]],
                         label = labels[[i]],
                         min = min(col, na.rm = TRUE),
                         max = max(col, na.rm = TRUE), 
                         start = min(col, na.rm = TRUE), 
                         end = max(col, na.rm = TRUE)), 
          
          checkboxInput(paste0("na_", labels[[i]]),
                        label = paste0("Include contacts w. missing values for ", labels[[i]], "?"), 
                        value = TRUE)
            )
          } else {
              
            dateRangeInput(labels[[i]],
                           label = labels[[i]],
                           min = min(col, na.rm = TRUE),
                           max = max(col, na.rm = TRUE), 
                           start = min(col, na.rm = TRUE), 
                           end = max(col, na.rm = TRUE))
            
            }
        }
        
        
        
      })
      
    })
    
    lapply(1:length(labels), function(i) {
      uiOutput(labels[[i]])
    })
    
    }
    
    
  })
  
  output$additional_filters_text <- renderUI({
    
    req(input$filter_or_not)
    
    if ((!is.null(input$filter_or_not)) && input$filter_or_not == "Yes"){
    
    tagList(
    h6("Use the input pickers and sliders to filter your data"),
    HTML( "<font size='1'>
            Note that the following are not shown: <br>
          • Empty or constant columns; <br>
          • Name columns; and <br>
          • the date of follow-up column <br>
          Also note that the options for each filter do not react to selections on other filters.
                               </font>")
    )
    } else { 
      HTML(c(" ")) 
      }
    
  })
  

  # ~~ select_date_of_review---------------------------
  
  output$select_date_of_review <- renderUI({
    
    ## date selection needs to use the unfiltered data frame
    ## because the selelction from this input feeds the filtering function
    
    req(input$data_to_use)
    req(read_file_transformed_reactive())
    
    
    flattened_dates <- 
      read_file_transformed_reactive() %>% 
      select(follow_up_date) %>%
      pull(1)
    
    min_date <- min(flattened_dates, na.rm = T) 
    max_date <- max(flattened_dates, na.rm = T) 
    
    # get the last date for which follow-up status was not "missing" or "future"
    # assume that that is the date as on which the data is being analyzed
    todays_date_imputed_from_data <- 
      read_file_transformed_reactive() %>% 
      filter(follow_up_status != "Suivi futur" & 
               follow_up_status != "Manquant" &
               !is.na(follow_up_status) ) %>% 
      select(follow_up_date) %>%
      pull(1) %>% 
      max(na.rm = T)
    
    dateInput("select_date_of_review", 
              label = "Select date of review", 
              #value = todays_date, 
              value = todays_date_imputed_from_data,
              min = min_date, 
              max = max_date)
    
  })
  
  
  # ~~~~ todays_date_reactive ----
  
  ## does not need to be inside of a reactive atm. But it was put there initially so I can trigger it with ObserveEvent
  todays_date_reactive <- reactive({
    input$select_date_of_review
  })
  
  
  # ~~ download_report---------------------------
  
  
  
  output$select_format <- renderUI({
    
    req(input$select_date_of_review)
    
    selectInput("report_format", 
                label = "Select format", 
                choices = c(
                  "pptx",
                  "docx", 
                  "pdf", 
                  "html (page)",
                  "html (slides)"
                  ))
    
  })
  
  output$download_report_button <- renderUI({
    
    req(input$select_date_of_review)
    
    tagList(HTML("<p style='font-size:4px'>  <br><br>  </p>"),
            downloadBttn("report",
                         label =  "Download report", 
                         style = "jelly", 
                         color = "primary", size = "md"
            ))
    
  })
  

  
  output$report <- download_report_function()


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~ main_tab_row_0 ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  output$contacts_per_day_value_box <-
    renderValueBox({
      req(input$select_date_of_review)
      req(input$analyze_action_bttn)
      shiny::validate(need(nrow(read_file_filtered_reactive()) > 0, message = FALSE))
      
      contacts_per_day_value_box(contacts_df_long = read_file_filtered_reactive(),
                                 todays_date = todays_date_reactive()) %>% 
        .$shiny_valuebox
      
    })
  
  output$cumulative_contacts_value_box <-
    renderValueBox({
      req(input$select_date_of_review)
      req(input$analyze_action_bttn)
      shiny::validate(need(nrow(read_file_filtered_reactive()) > 0, message = FALSE))
      
      cumulative_contacts_value_box(contacts_df_long = read_file_filtered_reactive(),
                                    todays_date = todays_date_reactive()) %>% 
        .$shiny_valuebox
      
    })
  
  output$contacts_under_surveillance_value_box <-
    renderValueBox({
      req(input$select_date_of_review)
      req(input$analyze_action_bttn)
      shiny::validate(need(nrow(read_file_filtered_reactive()) > 0, message = FALSE))
      
      contacts_under_surveillance_value_box(contacts_df_long = read_file_filtered_reactive(),
                                            todays_date = todays_date_reactive()) %>% 
        .$shiny_valuebox
      
    })
  
  output$pct_contacts_followed_value_box <-
    renderValueBox({
      req(input$select_date_of_review)
      req(input$analyze_action_bttn)
      shiny::validate(need(nrow(read_file_filtered_reactive()) > 0, message = FALSE))
      
      pct_contacts_followed_value_box(contacts_df_long = read_file_filtered_reactive(),
                                      todays_date = todays_date_reactive())%>% 
        .$shiny_valuebox
      
    })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~ main_tab_row_1 ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  output$all_contacts_per_admin_1_table <- 
    renderReactable({
      req(input$select_date_of_review)
      
        all_contacts_per_admin_1_table(contacts_df_long = read_file_filtered_reactive(),
                                      todays_date = todays_date_reactive()) 
      })
  
  
  output$all_contacts_per_admin_1_sunburst_plot <- 
    renderHighchart({
      req(input$select_date_of_review)
      
        all_contacts_per_admin_1_sunburst_plot(contacts_df_long = read_file_filtered_reactive(),
                                              todays_date = todays_date_reactive())
      })
  
  output$all_contacts_per_admin_1_bar_chart <- 
    renderHighchart({
      req(input$select_date_of_review)
      
      all_contacts_per_admin_1_bar_chart(contacts_df_long = read_file_filtered_reactive(),
                                            todays_date = todays_date_reactive())
    })
  
  
  output$all_contacts_per_admin_1_text <- 
    renderUI({
      req(input$select_date_of_review)
      
        all_contacts_per_admin_1_text(contacts_df_long = read_file_filtered_reactive(),
                                     todays_date = todays_date_reactive())
      })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~ main_tab_row_2 ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  output$contacts_under_surveillance_per_admin_1_over_time_bar_chart <- 
    renderHighchart({
      req(input$select_date_of_review)
      
        contacts_under_surveillance_per_admin_1_over_time_bar_chart(contacts_df_long = read_file_filtered_reactive(),
                                                                   todays_date = todays_date_reactive())
    })
  
  output$contacts_under_surveillance_per_admin_1_over_time_bar_chart_relative <- 
    renderHighchart({
      req(input$select_date_of_review)
      
        contacts_under_surveillance_per_admin_1_over_time_bar_chart_relative(contacts_df_long = read_file_filtered_reactive(),
                                                                            todays_date = todays_date_reactive())
    })
  
  output$contacts_under_surveillance_per_admin_1_over_time_text <- 
    renderUI({
      req(input$select_date_of_review)
      
        contacts_under_surveillance_per_admin_1_over_time_text(contacts_df_long = read_file_filtered_reactive(),
                                                              todays_date = todays_date_reactive())
    })
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~ main_tab_row_3 ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$total_contacts_per_case_donut_plot <- 
    renderHighchart({
      req(input$select_date_of_review)

        total_contacts_per_case_donut_plot(contacts_df_long = read_file_filtered_reactive(),
                                           todays_date = todays_date_reactive())
    })
  
  
  output$total_contacts_per_case_bar_chart <- 
    renderHighchart({
      req(input$select_date_of_review)
      
      total_contacts_per_case_bar_chart(contacts_df_long = read_file_filtered_reactive(),
                                         todays_date = todays_date_reactive())
    })
  
  
  output$total_contacts_per_case_text <- 
    renderUI({
      req(input$select_date_of_review)
      
        total_contacts_per_case_text(contacts_df_long = read_file_filtered_reactive(),
                                     todays_date = todays_date_reactive())
    })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~ main_tab_row_4 ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$total_contacts_per_link_type_donut_plot <- 
    renderHighchart({
      req(input$select_date_of_review)
      
        total_contacts_per_link_type_donut_plot(contacts_df_long = read_file_filtered_reactive(),
                                                todays_date = todays_date_reactive())
    })
  
  output$total_contacts_per_link_type_bar_chart <- 
    renderHighchart({
      req(input$select_date_of_review)
      
      total_contacts_per_link_type_bar_chart(contacts_df_long = read_file_filtered_reactive(),
                                    todays_date = todays_date_reactive())
    })
    
  
  output$total_contacts_per_link_type_text <- 
    renderUI({
      req(input$select_date_of_review)
      
        total_contacts_per_link_type_text(contacts_df_long = read_file_filtered_reactive(),
                                          todays_date = todays_date_reactive())
    })
  
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~ main_tab_row_6 ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # output$active_contacts_timeline_snake_plot <-
  #   renderEcharts4r({
  #     req(input$select_date_of_review)
  #     
  #       active_contacts_timeline_snake_plot(contacts_df_long = read_file_filtered_reactive(),
  #                                    todays_date = todays_date_reactive(), 
  #                                    legend_df = legend_df)
  #   })
  # 
  output$active_contacts_timeline_snake_plot <-
    renderPlotly({
      req(input$select_date_of_review)
      
      active_contacts_timeline_snake_plot(contacts_df_long = read_file_filtered_reactive(),
                                          todays_date = todays_date_reactive(), 
                                          legend_df = legend_df)
    })
  
  
  output$active_contacts_snake_plot_selected_table <-
    renderReactable({
      req(input$select_date_of_review)
      
      active_contacts_snake_plot_selected_table(contacts_df_long = read_file_filtered_reactive() ,
                                     event_data("plotly_selecting")$customdata)
      
    })  
  
  output$active_contacts_snake_plot_selected_table_download <- 
    active_contacts_snake_plot_selected_table_download()
    
  
  output$active_contacts_timeline_table <-
    renderReactable({
      req(input$select_date_of_review)
      
      active_contacts_timeline_table(contacts_df_long = read_file_filtered_reactive() ,
                                      todays_date = todays_date_reactive())
    })  
  
  output$active_contacts_timeline_table_download <- active_contacts_timeline_table_download()

  
  output$active_contacts_breakdown_bar_chart <-
    renderEcharts4r({
      req(input$select_date_of_review)
      
        active_contacts_breakdown_bar_chart(contacts_df_long = read_file_filtered_reactive() ,
                                            todays_date = todays_date_reactive(), 
                                            legend_df = legend_df)
    })
  
  
  output$active_contacts_breakdown_table <-
    renderReactable({
      req(input$select_date_of_review)
      
        active_contacts_breakdown_table(contacts_df_long = read_file_filtered_reactive() ,
                                        todays_date = todays_date_reactive())
    })
  
  output$active_contacts_breakdown_table_download <-active_contacts_breakdown_table_download()
  
  
  output$active_contacts_timeline_text <- renderUI({
    req(input$select_date_of_review)
    
    active_contacts_timeline_text(contacts_df_long = read_file_filtered_reactive() ,
                                        todays_date = todays_date_reactive())
    
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~ main_tab_row_7 ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  output$contacts_lost_24_to_72_hours_table <- 
    render_gt({
      req(input$select_date_of_review)
      
        contacts_lost_24_to_72_hours_table(contacts_df_long = read_file_filtered_reactive() ,
                                     todays_date = todays_date_reactive())
    })
  
  output$contacts_lost_24_to_72_hours_table_download <-
    contacts_lost_24_to_72_hours_table_download()
  
  output$lost_contacts_linelist_table <- 
    renderReactable({
      req(input$select_date_of_review)
      
        lost_contacts_linelist_table(contacts_df_long = read_file_filtered_reactive() ,
                                todays_date = todays_date_reactive()) %>% 
        .$output_table
    })
  
  output$lost_contacts_linelist_table_download <-
    lost_contacts_linelist_table_download()
  
  
  output$lost_contacts_linelist_table_title <- 
    renderUI({
      req(input$select_date_of_review)
      
        lost_contacts_linelist_table(contacts_df_long = read_file_filtered_reactive() ,
                               todays_date = todays_date_reactive()) %>% 
        .$table_title
    })
  
  output$lost_contacts_linelist_text <- renderUI({
    req(input$select_date_of_review)
    
    lost_contacts_linelist_text(contacts_df_long = read_file_filtered_reactive() ,
                                  todays_date = todays_date_reactive())
    
    
  })
  
  
  
  

  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #   main_tab_admin_1 ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  
  output$select_admin_1 <- renderUI({
    
    req(read_file_transformed_reactive())
    
    admin_1_choices <- 
      unique(read_file_transformed_reactive()$admin_1) %>% 
      .[.!="NA"] %>% 
      .[!is.na(.)]
      
  selectInput("select_admin_1", 
              label = "Choose admin level 1",
              choices = admin_1_choices,
              multiple = FALSE)

    })
  
  
  # ~~~ select_date_of_review_admin_1----
  output$select_date_of_review_admin_1 <- renderUI({
    
    req(read_file_transformed_admin_1_reactive())
    
      ## date selection needs to use the unfiltered data frame
      ## because the selection from this input feeds the filtering function
      
      flattened_dates <- 
        read_file_transformed_admin_1_reactive() %>% 
        select(follow_up_date) %>%
        pull(1)
      
      min_date <- min(flattened_dates, na.rm = T) 
      max_date <- max(flattened_dates, na.rm = T) 
      
      # get the last date for which follow-up status was not "missing"
      # assume that that is the date as at which the data is being analyzed
      todays_date_imputed_from_data <- 
        read_file_transformed_admin_1_reactive() %>% 
        filter(follow_up_status != "Suivi futur" & 
                 follow_up_status != "Manquant" &
                 !is.na(follow_up_status) ) %>% 
        select(follow_up_date) %>%
        pull(1) %>% 
        max(na.rm = T)
      

    dateInput("select_date_of_review_admin_1", 
              label = HTML("Select date of review 
                           <br>
                         <font size='1'>
                         (Date range is from the date of first follow-up in the division to the date of final follow-up in the division)
                         </font>"
                           ),
              value = todays_date_imputed_from_data, 
              min = min_date, 
              max = max_date)
    
  })
  
  
  # ~~~ todays_date_admin_1_reactive ----
  
  ## does not need to be inside of a reactive atm. But it was put there initially so I can trigger it with ObserveEvent
  todays_date_admin_1_reactive <- reactive({
    req(input$select_admin_1)
    input$select_date_of_review_admin_1
  })
  
  
  
  
  # ~~~ main_tab_row_0_admin_1 ----
  
  
  output$contacts_per_day_value_box_admin_1 <-
    renderValueBox({
      req(input$select_admin_1)
      req(input$select_date_of_review_admin_1)
      
      contacts_per_day_value_box(read_file_filtered_admin_1_reactive(), 
                                   todays_date_admin_1_reactive()) %>% 
        .$shiny_valuebox
      
    })
  
  
  output$cumulative_contacts_value_box_admin_1 <-
    renderValueBox({
      req(input$select_admin_1)
      req(input$select_date_of_review_admin_1)
      cumulative_contacts_value_box(read_file_filtered_admin_1_reactive(), 
                                    todays_date_admin_1_reactive()) %>% 
        .$shiny_valuebox
      
    })
  
  
  output$contacts_under_surveillance_value_box_admin_1 <-
    renderValueBox({
      req(input$select_admin_1)
      req(input$select_date_of_review_admin_1)
      contacts_under_surveillance_value_box(read_file_filtered_admin_1_reactive(), 
                                            todays_date_admin_1_reactive()) %>% 
        .$shiny_valuebox
      
    })
  
  output$pct_contacts_followed_value_box_admin_1 <-
    renderValueBox({
      req(input$select_admin_1)
      req(input$select_date_of_review_admin_1)
      pct_contacts_followed_value_box(read_file_filtered_admin_1_reactive(), 
                                      todays_date_admin_1_reactive()) %>% 
        .$shiny_valuebox
      
    })
  
  # ~~~ main_tab_row_1_admin_1 ----
  
  
  output$all_contacts_per_admin_1_table_admin_1 <- 
    renderReactable({
      req(input$select_date_of_review_admin_1)
      
        all_contacts_per_admin_1_table_admin_1(contacts_df_long = read_file_filtered_admin_1_reactive(),
                                               todays_date = todays_date_admin_1_reactive())   
    })
  
  
  output$all_contacts_per_admin_1_sunburst_plot_admin_1 <- 
    renderHighchart({
      req(input$select_date_of_review_admin_1)
      
        all_contacts_per_admin_1_sunburst_plot_admin_1(contacts_df_long = read_file_filtered_admin_1_reactive(),
                                                       todays_date = todays_date_admin_1_reactive())   
    })
  
  output$all_contacts_per_admin_1_text_admin_1 <- 
    renderUI({
      req(input$select_date_of_review_admin_1)
      
        all_contacts_per_admin_1_text_admin_1(contacts_df_long = read_file_filtered_admin_1_reactive(),
                                              todays_date = todays_date_admin_1_reactive())   
    })
  
  
  
  
  # ~~~ main_tab_row_2_admin_1 ----
  
  
  output$contacts_under_surveillance_per_admin_1_over_time_bar_chart_admin_1 <- 
    renderHighchart({
      req(input$select_date_of_review_admin_1)
      
        contacts_under_surveillance_per_admin_1_over_time_bar_chart_admin_1(contacts_df_long = read_file_filtered_admin_1_reactive(),
                                                                            todays_date = todays_date_admin_1_reactive())   
    })
  
  output$contacts_under_surveillance_per_admin_1_over_time_bar_chart_relative_admin_1 <- 
    renderHighchart({
      req(input$select_date_of_review_admin_1)
      
        contacts_under_surveillance_per_admin_1_over_time_bar_chart_relative_admin_1(contacts_df_long = read_file_filtered_admin_1_reactive(),
                                                                                     todays_date = todays_date_admin_1_reactive())   
    })
  
  output$contacts_under_surveillance_per_admin_1_over_time_text_admin_1 <- 
    renderUI({
      req(input$select_date_of_review_admin_1)
      
        contacts_under_surveillance_per_admin_1_over_time_text_admin_1(contacts_df_long = read_file_filtered_admin_1_reactive(),
                                                                       todays_date = todays_date_admin_1_reactive())   
    })
  
  
  
  # ~~~ main_tab_row_3_admin_1 ----
  
  output$total_contacts_per_case_donut_plot_admin_1 <- 
    renderHighchart({
      req(input$select_date_of_review_admin_1)
      
        total_contacts_per_case_donut_plot(contacts_df_long = read_file_filtered_admin_1_reactive(),
                                           todays_date = todays_date_admin_1_reactive())   
    })
  
  output$total_contacts_per_case_table_admin_1 <- 
    renderReactable({
      req(input$select_date_of_review_admin_1)
      
        total_contacts_per_case_table(contacts_df_long = read_file_filtered_admin_1_reactive(),
                                      todays_date = todays_date_admin_1_reactive())   
    })
  
  
  output$total_contacts_per_case_text_admin_1 <- 
    renderUI({
      req(input$select_date_of_review_admin_1)
      
        total_contacts_per_case_text(contacts_df_long = read_file_filtered_admin_1_reactive(),
                                     todays_date = todays_date_admin_1_reactive())    
        })
  
  
  # ~~~ main_tab_row_4_admin_1 ----
  
  output$total_contacts_per_link_type_donut_plot_admin_1 <- 
    renderHighchart({
      req(input$select_date_of_review_admin_1)
      
        total_contacts_per_link_type_donut_plot(contacts_df_long = read_file_filtered_admin_1_reactive(),
                                                todays_date = todays_date_admin_1_reactive())   
    })

  
  output$total_contacts_per_link_type_text_admin_1 <- 
    renderUI({
      req(input$select_date_of_review_admin_1)
      
        total_contacts_per_link_type_text(contacts_df_long = read_file_filtered_admin_1_reactive(),
                                          todays_date = todays_date_admin_1_reactive())   
    })

  
  # ~~~ main_tab_row_6_admin_1 ----
  
  
  output$active_contacts_timeline_snake_plot_admin_1 <-
    renderEcharts4r({
      req(input$select_date_of_review_admin_1)
      
      active_contacts_timeline_snake_plot(contacts_df_long = read_file_filtered_admin_1_reactive(),
                                   todays_date = todays_date_admin_1_reactive(), 
                                   legend_df = legend_df)
    })
  
  
  output$active_contacts_timeline_text_admin_1 <- renderUI({
    req(input$select_date_of_review_admin_1)
    
    active_contacts_timeline_text(contacts_df_long = read_file_filtered_admin_1_reactive() ,
                                 todays_date = todays_date_admin_1_reactive())
    
    
  })
  
  output$active_contacts_breakdown_bar_chart_admin_1 <-
    renderEcharts4r({
      req(input$select_date_of_review_admin_1)
      
      active_contacts_breakdown_bar_chart(contacts_df_long = read_file_filtered_admin_1_reactive() ,
                                          todays_date = todays_date_admin_1_reactive(), 
                                          legend_df = legend_df)
    })
  
  
  output$active_contacts_breakdown_table_admin_1 <-
    renderReactable({
      req(input$select_date_of_review_admin_1)
      
      active_contacts_breakdown_table(contacts_df_long = read_file_filtered_admin_1_reactive() ,
                                      todays_date = todays_date_admin_1_reactive())
    })
  
  
  

  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~~~ main_tab_row_7_admin_1 ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$contacts_lost_24_to_72_hours_table_admin_1 <- 
    render_gt({
      req(input$select_date_of_review_admin_1)
      
      contacts_lost_24_to_72_hours_table(contacts_df_long = read_file_filtered_admin_1_reactive() ,
                                   todays_date = todays_date_admin_1_reactive())
    })
  
  
  output$lost_contacts_linelist_table_admin_1 <- 
    renderReactable({
      req(input$select_date_of_review_admin_1)
      
      lost_contacts_linelist_table(contacts_df_long = read_file_filtered_admin_1_reactive() ,
                             todays_date = todays_date_admin_1_reactive()) %>%
        .$output_table
    })
  
  output$lost_contacts_linelist_table_title_admin_1 <- 
    renderUI({
      req(input$select_date_of_review_admin_1)
      
      lost_contacts_linelist_table(contacts_df_long = read_file_filtered_admin_1_reactive() ,
                             todays_date = todays_date_admin_1_reactive()) %>%
        .$table_title
    })
  
  
  
  
}

#polished::secure_server(server)



