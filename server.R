options(shiny.maxRequestSize=30*1024^2)

server <- function(input, output) {  
  
  
  source(here("helper_scripts/server_functions.R"), local = T)
  

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #   load_data_tab ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  

  # ~~ data_to_use_picker ---------------------------
  output$data_to_use_picker <- renderUI({
    radioButtons(inputId = "data_to_use", 
                 label = "Input Data", 
                 choices = c("Use preloaded data", 
                             "Use uploaded data"))
  })
  
  # ~~ input_data_preloaded_or_uploaded ---------------------------
  
  output$input_data_preloaded_or_uploaded <- renderUI({
    
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
      }  
  })
  
  
  # ~~~~ read_file_raw_reactive ----
  read_file_raw_reactive <- reactive({
    
    req(input$data_to_use)
    req(input$analyze_action_bttn)

    read_file_raw(
      data_to_use = input$data_to_use,
      preloaded_data_options = preloaded_data_options,
      preloaded_data_choice = input$preloaded_data_choice,
      uploaded_data_contacts_list_path = input$uploaded_data_contacts_list$datapath,
      uploaded_data_follow_up_list_path = input$uploaded_data_follow_up_list$datapath
      )
    
  })
  
  # ~~~~ read_file_transformed_reactive ----
  

  read_file_transformed_reactive <- reactive({
    
    req(input$data_to_use)
    req(input$analyze_action_bttn)
    
    read_file_transformed(tracing_data_raw = read_file_raw_reactive())
    
    
  })
  
  #~~~~ read_file_transformed_regional_reactive ----

  read_file_transformed_regional_reactive <- reactive({
    
    req(input$select_region)

    read_file_transformed_regional(
      contacts_df_long_transformed = read_file_transformed_reactive(),
      select_region = input$select_region
    )

  })
  
  
  # ~~~~ read_file_filtered_reactive ----
  
  read_file_filtered_reactive <- reactive({
    
    read_file_filtered(
      contacts_df_long_transformed = read_file_transformed_reactive(),
      todays_date = todays_date_reactive(),
      legend_df = legend_df # defined in global.R
    )
    
    
  })
  
  
  # ~~~~ read_file_filtered_regional_reactive ----
  
  read_file_filtered_regional_reactive <- reactive({
    
    req(input$select_region)

    read_file_filtered_regional(
      contacts_df_long_transformed_regional = read_file_transformed_regional_reactive(),
      todays_date = todays_date_regional_reactive(),
      legend_df = legend_df # defined in global.R
    )
    
    
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
  #   app_tab ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  
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
      filter(etat_suivi != "Suivi futur" & 
               etat_suivi != "Manquant" &
               !is.na(etat_suivi) ) %>% 
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
                  "html",
                  "pdf", 
                  "docx", 
                  "pptx"
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
  # ~ app_tab_row_0 ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  output$contacts_per_day_value_box <-
    renderValueBox({
      req(input$select_date_of_review)
      req(input$analyze_action_bttn)
      
      contacts_per_day_value_box(contacts_df_long = read_file_filtered_reactive(),
                                 todays_date = todays_date_reactive()) %>% 
        .$shiny_valuebox
      
    })
  
  output$cumulative_contacts_value_box <-
    renderValueBox({
      req(input$select_date_of_review)
      req(input$analyze_action_bttn)
      
      cumulative_contacts_value_box(contacts_df_long = read_file_filtered_reactive(),
                                    todays_date = todays_date_reactive()) %>% 
        .$shiny_valuebox
      
    })
  
  output$contacts_under_surveillance_value_box <-
    renderValueBox({
      req(input$select_date_of_review)
      req(input$analyze_action_bttn)
      
      contacts_under_surveillance_value_box(contacts_df_long = read_file_filtered_reactive(),
                                            todays_date = todays_date_reactive()) %>% 
        .$shiny_valuebox
      
    })
  
  output$pct_contacts_followed_value_box <-
    renderValueBox({
      req(input$select_date_of_review)
      req(input$analyze_action_bttn)
      
      pct_contacts_followed_value_box(contacts_df_long = read_file_filtered_reactive(),
                                      todays_date = todays_date_reactive())%>% 
        .$shiny_valuebox
      
    })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~ app_tab_row_1 ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  output$all_contacts_per_region_table <- 
    renderReactable({
      req(input$select_date_of_review)
      
        all_contacts_per_region_table(contacts_df_long = read_file_filtered_reactive(),
                                      todays_date = todays_date_reactive()) 
      })
  
  
  output$all_contacts_per_region_sunburst_plot <- 
    renderHighchart({
      req(input$select_date_of_review)
      
        all_contacts_per_region_sunburst_plot(contacts_df_long = read_file_filtered_reactive(),
                                              todays_date = todays_date_reactive())
      })
  
  output$all_contacts_per_region_bar_chart <- 
    renderHighchart({
      req(input$select_date_of_review)
      
      all_contacts_per_region_bar_chart(contacts_df_long = read_file_filtered_reactive(),
                                            todays_date = todays_date_reactive())
    })
  
  
  output$all_contacts_per_region_text <- 
    renderUI({
      req(input$select_date_of_review)
      
        all_contacts_per_region_text(contacts_df_long = read_file_filtered_reactive(),
                                     todays_date = todays_date_reactive())
      })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~ app_tab_row_2 ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  output$contacts_under_surveillance_per_region_over_time_bar_chart <- 
    renderHighchart({
      req(input$select_date_of_review)
      
        contacts_under_surveillance_per_region_over_time_bar_chart(contacts_df_long = read_file_filtered_reactive(),
                                                                   todays_date = todays_date_reactive())
    })
  
  output$contacts_under_surveillance_per_region_over_time_bar_chart_relative <- 
    renderHighchart({
      req(input$select_date_of_review)
      
        contacts_under_surveillance_per_region_over_time_bar_chart_relative(contacts_df_long = read_file_filtered_reactive(),
                                                                            todays_date = todays_date_reactive())
    })
  
  output$contacts_under_surveillance_per_region_over_time_text <- 
    renderUI({
      req(input$select_date_of_review)
      
        contacts_under_surveillance_per_region_over_time_text(contacts_df_long = read_file_filtered_reactive(),
                                                              todays_date = todays_date_reactive())
    })
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~ app_tab_row_3 ----
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
  # ~ app_tab_row_4 ----
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
  # ~ app_tab_row_6 ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$active_contacts_timeline_snake_plot <-
    renderEcharts4r({
      req(input$select_date_of_review)
      
        active_contacts_timeline_snake_plot(contacts_df_long = read_file_filtered_reactive(),
                                     todays_date = todays_date_reactive())
    })
  
  output$active_contacts_timeline_table <-
    renderReactable({
      req(input$select_date_of_review)
      
      active_contacts_timeline_table(contacts_df_long = read_file_filtered_reactive() ,
                                      todays_date = todays_date_reactive())
    })  
  
  output$active_contacts_timeline_table_download <-
    active_contacts_timeline_table_download(contacts_df_long = read_file_filtered_reactive(),
                                             todays_date = todays_date_reactive())

  
  output$active_contacts_breakdown_bar_chart <-
    renderEcharts4r({
      req(input$select_date_of_review)
      
        active_contacts_breakdown_bar_chart(contacts_df_long = read_file_filtered_reactive() ,
                                            todays_date = todays_date_reactive())
    })
  
  
  output$active_contacts_breakdown_table <-
    renderReactable({
      req(input$select_date_of_review)
      
        active_contacts_breakdown_table(contacts_df_long = read_file_filtered_reactive() ,
                                        todays_date = todays_date_reactive())
    })
  
  output$active_contacts_breakdown_table_download <-
    active_contacts_breakdown_table_download(contacts_df_long = read_file_filtered_reactive(),
                                             todays_date = todays_date_reactive())
  
  
  output$active_contacts_timeline_text <- renderUI({
    req(input$select_date_of_review)
    
    active_contacts_timeline_text(contacts_df_long = read_file_filtered_reactive() ,
                                        todays_date = todays_date_reactive())
    
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~ app_tab_row_7 ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  output$contacts_lost_24_to_72_hours_table <- 
    render_gt({
      req(input$select_date_of_review)
      
        contacts_lost_24_to_72_hours_table(contacts_df_long = read_file_filtered_reactive() ,
                                     todays_date = todays_date_reactive())
    })
  
  output$contacts_lost_24_to_72_hours_table_download <-
    contacts_lost_24_to_72_hours_table_download(contacts_df_long = read_file_filtered_reactive(),
                                             todays_date = todays_date_reactive())
  
  output$lost_contacts_linelist_table <- 
    renderReactable({
      req(input$select_date_of_review)
      
        lost_contacts_linelist_table(contacts_df_long = read_file_filtered_reactive() ,
                                todays_date = todays_date_reactive()) %>% 
        .$output_table
    })
  
  output$lost_contacts_linelist_table_download <-
    lost_contacts_linelist_table_download(contacts_df_long = read_file_filtered_reactive(),
                                                todays_date = todays_date_reactive())
  
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
  # ~~  app_tab_regional ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  output$select_region <- renderUI({
    
    req(read_file_transformed_reactive())
    
    region_choices <- 
      unique(read_file_transformed_reactive()$region) %>% 
      .[.!="NA"] %>% 
      .[!is.na(.)]
      
  selectInput("select_region", 
              label = "Choose region",
              choices = region_choices,
              multiple = FALSE)

    })
  
  
  # ~~~ select_date_of_review_regional----
  output$select_date_of_review_regional <- renderUI({
    
    req(read_file_transformed_regional_reactive())
    
      ## date selection needs to use the unfiltered data frame
      ## because the selection from this input feeds the filtering function
      
      flattened_dates <- 
        read_file_transformed_regional_reactive() %>% 
        select(follow_up_date) %>%
        pull(1)
      
      min_date <- min(flattened_dates, na.rm = T) 
      max_date <- max(flattened_dates, na.rm = T) 
      
      # get the last date for which follow-up status was not "missing"
      # assume that that is the date as at which the data is being analyzed
      todays_date_imputed_from_data <- 
        read_file_transformed_regional_reactive() %>% 
        filter(etat_suivi != "Suivi futur" & 
                 etat_suivi != "Manquant" &
                 !is.na(etat_suivi) ) %>% 
        select(follow_up_date) %>%
        pull(1) %>% 
        max(na.rm = T)
      

    dateInput("select_date_of_review_regional", 
              label = HTML("Select date of review 
                           <br>
                         <font size='1'>
                         (Date range is from the date of first follow-up in region to the date of final follow-up in the region)
                         </font>"
                           ),
              value = todays_date_imputed_from_data, 
              min = min_date, 
              max = max_date)
    
  })
  
  
  # ~~~ todays_date_regional_reactive ----
  
  ## does not need to be inside of a reactive atm. But it was put there initially so I can trigger it with ObserveEvent
  todays_date_regional_reactive <- reactive({
    req(input$select_region)
    input$select_date_of_review_regional
  })
  
  
  
  
  # ~~~ app_tab_row_0_regional ----
  
  
  output$contacts_per_day_value_box_regional <-
    renderValueBox({
      req(input$select_region)
      req(input$select_date_of_review_regional)
      
      contacts_per_day_value_box(read_file_filtered_regional_reactive(), 
                                   todays_date_regional_reactive()) %>% 
        .$shiny_valuebox
      
    })
  
  
  output$cumulative_contacts_value_box_regional <-
    renderValueBox({
      req(input$select_region)
      req(input$select_date_of_review_regional)
      cumulative_contacts_value_box(read_file_filtered_regional_reactive(), 
                                    todays_date_regional_reactive()) %>% 
        .$shiny_valuebox
      
    })
  
  
  output$contacts_under_surveillance_value_box_regional <-
    renderValueBox({
      req(input$select_region)
      req(input$select_date_of_review_regional)
      contacts_under_surveillance_value_box(read_file_filtered_regional_reactive(), 
                                            todays_date_regional_reactive()) %>% 
        .$shiny_valuebox
      
    })
  
  output$pct_contacts_followed_value_box_regional <-
    renderValueBox({
      req(input$select_region)
      req(input$select_date_of_review_regional)
      pct_contacts_followed_value_box(read_file_filtered_regional_reactive(), 
                                      todays_date_regional_reactive()) %>% 
        .$shiny_valuebox
      
    })
  
  # ~~~ app_tab_row_1_regional ----
  
  
  output$all_contacts_per_region_table_regional <- 
    renderReactable({
      req(input$select_date_of_review_regional)
      
        all_contacts_per_region_table_regional(contacts_df_long = read_file_filtered_regional_reactive(),
                                               todays_date = todays_date_regional_reactive())   
    })
  
  
  output$all_contacts_per_region_sunburst_plot_regional <- 
    renderHighchart({
      req(input$select_date_of_review_regional)
      
        all_contacts_per_region_sunburst_plot_regional(contacts_df_long = read_file_filtered_regional_reactive(),
                                                       todays_date = todays_date_regional_reactive())   
    })
  
  output$all_contacts_per_region_text_regional <- 
    renderUI({
      req(input$select_date_of_review_regional)
      
        all_contacts_per_region_text_regional(contacts_df_long = read_file_filtered_regional_reactive(),
                                              todays_date = todays_date_regional_reactive())   
    })
  
  
  
  
  # ~~~ app_tab_row_2_regional ----
  
  
  output$contacts_under_surveillance_per_region_over_time_bar_chart_regional <- 
    renderHighchart({
      req(input$select_date_of_review_regional)
      
        contacts_under_surveillance_per_region_over_time_bar_chart_regional(contacts_df_long = read_file_filtered_regional_reactive(),
                                                                            todays_date = todays_date_regional_reactive())   
    })
  
  output$contacts_under_surveillance_per_region_over_time_bar_chart_relative_regional <- 
    renderHighchart({
      req(input$select_date_of_review_regional)
      
        contacts_under_surveillance_per_region_over_time_bar_chart_relative_regional(contacts_df_long = read_file_filtered_regional_reactive(),
                                                                                     todays_date = todays_date_regional_reactive())   
    })
  
  output$contacts_under_surveillance_per_region_over_time_text_regional <- 
    renderUI({
      req(input$select_date_of_review_regional)
      
        contacts_under_surveillance_per_region_over_time_text_regional(contacts_df_long = read_file_filtered_regional_reactive(),
                                                                       todays_date = todays_date_regional_reactive())   
    })
  
  
  
  # ~~~ app_tab_row_3_regional ----
  
  output$total_contacts_per_case_donut_plot_regional <- 
    renderHighchart({
      req(input$select_date_of_review_regional)
      
        total_contacts_per_case_donut_plot(contacts_df_long = read_file_filtered_regional_reactive(),
                                           todays_date = todays_date_regional_reactive())   
    })
  
  output$total_contacts_per_case_table_regional <- 
    renderReactable({
      req(input$select_date_of_review_regional)
      
        total_contacts_per_case_table(contacts_df_long = read_file_filtered_regional_reactive(),
                                      todays_date = todays_date_regional_reactive())   
    })
  
  
  output$total_contacts_per_case_text_regional <- 
    renderUI({
      req(input$select_date_of_review_regional)
      
        total_contacts_per_case_text(contacts_df_long = read_file_filtered_regional_reactive(),
                                     todays_date = todays_date_regional_reactive())    
        })
  
  
  # ~~~ app_tab_row_4_regional ----
  
  output$total_contacts_per_link_type_donut_plot_regional <- 
    renderHighchart({
      req(input$select_date_of_review_regional)
      
        total_contacts_per_link_type_donut_plot(contacts_df_long = read_file_filtered_regional_reactive(),
                                                todays_date = todays_date_regional_reactive())   
    })

  
  output$total_contacts_per_link_type_text_regional <- 
    renderUI({
      req(input$select_date_of_review_regional)
      
        total_contacts_per_link_type_text(contacts_df_long = read_file_filtered_regional_reactive(),
                                          todays_date = todays_date_regional_reactive())   
    })

  
  # ~~~ app_tab_row_6_regional ----
  
  
  output$active_contacts_timeline_snake_plot_regional <-
    renderEcharts4r({
      req(input$select_date_of_review_regional)
      
      active_contacts_timeline_snake_plot(contacts_df_long = read_file_filtered_regional_reactive(),
                                   todays_date = todays_date_regional_reactive())
    })
  
  
  output$active_contacts_timeline_text_regional <- renderUI({
    req(input$select_date_of_review_regional)
    
    active_contacts_timeline_text(contacts_df_long = read_file_filtered_regional_reactive() ,
                                 todays_date = todays_date_regional_reactive())
    
    
  })
  
  output$active_contacts_breakdown_bar_chart_regional <-
    renderEcharts4r({
      req(input$select_date_of_review_regional)
      
      active_contacts_breakdown_bar_chart(contacts_df_long = read_file_filtered_regional_reactive() ,
                                          todays_date = todays_date_regional_reactive())
    })
  
  
  output$active_contacts_breakdown_table_regional <-
    renderReactable({
      req(input$select_date_of_review_regional)
      
      active_contacts_breakdown_table(contacts_df_long = read_file_filtered_regional_reactive() ,
                                      todays_date = todays_date_regional_reactive())
    })
  
  
  

  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~~~ app_tab_row_7_regional ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$contacts_lost_24_to_72_hours_table_regional <- 
    render_gt({
      req(input$select_date_of_review_regional)
      
      contacts_lost_24_to_72_hours_table(contacts_df_long = read_file_filtered_regional_reactive() ,
                                   todays_date = todays_date_regional_reactive())
    })
  
  
  output$lost_contacts_linelist_table_regional <- 
    renderReactable({
      req(input$select_date_of_review_regional)
      
      lost_contacts_linelist_table(contacts_df_long = read_file_filtered_regional_reactive() ,
                             todays_date = todays_date_regional_reactive()) %>%
        .$output_table
    })
  
  output$lost_contacts_linelist_table_title_regional <- 
    renderUI({
      req(input$select_date_of_review_regional)
      
      lost_contacts_linelist_table(contacts_df_long = read_file_filtered_regional_reactive() ,
                             todays_date = todays_date_regional_reactive()) %>%
        .$table_title
    })
  
  
  
  
}

#polished::secure_server(server)



