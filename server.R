options(shiny.maxRequestSize=30*1024^2)

server <- function(input, output) {  
  
  
  source(here("helper_scripts/server_functions.R"), local = T)
  

  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #   load_data_tab ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # ~~ data_to_use ---------------------------
  output$data_to_use <- renderUI({
    radioButtons(inputId = "data_to_use_id", 
                 label = "Input Data", 
                 choices = c("Use preloaded data", 
                             "Use uploaded data"))
  })
  
  # ~~ input_data ---------------------------
  output$input_data <- renderUI({
    
    if(input$data_to_use_id == "Use uploaded data") {
      fileInput(inputId = "uploaded_data",
                label = "Upload an xlsx or csv file", 
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain", 
                           ".csv", 
                           ".xlsx",
                           ".xls"))
    } else if (input$data_to_use_id == "Use preloaded data") {
      
      selectInput("preloaded_data", 
                  label = "Use preloaded data", 
                  choices = c("Guinea list 03_14"
                              #, 
                              #"Sample contacts list"
                              ),
                  selected = NULL,
                  multiple = FALSE)
    }
    
  })
  
  
   #~~ data_completeness_plot ---------------------------
  output$data_completeness_plot <- 
    renderPlot({
      read_file_raw() %>% 
      data_completeness_plot()
    }) 

  #~~ data_cardinality_plot ---------------------------
  output$data_cardinality_plot <- 
    renderPlot({
      read_file_raw() %>% 
      data_cardinality_plot()})
  
  #~~ reactable_table ---------------------------
  output$reactable_table <- 
    renderReactable({
      read_file_raw() %>% 
        reactable_table()})
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #   all_contacts_tab ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  
  # ~~ select_date_of_review---------------------------
  
  output$select_date_of_review <- renderUI({
    
    ## date selection needs to use the unfiltered data frame
    ## because the selelction from this input feeds the filtering function
    
    flattened_dates <- 
      read_file_unfiltered()$contacts_df_long_unfiltered%>% 
      select(follow_up_date) %>%
      pull(1)
    
    min_date <- min(flattened_dates, na.rm = T) 
    max_date <- max(flattened_dates, na.rm = T) 
    
    cat(file=stderr(), "dates have been flattened")
    
    # get the last date for which follow-up status was not "missing"
    # assume that that is the date as at which the data is being analyzed
    todays_date_imputed_from_data <- 
      read_file_unfiltered()$contacts_df_long_unfiltered %>% 
      filter(etat_suivi != "Données manquantes") %>% 
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
  

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~ all_contacts_tab_row_0 ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  output$contacts_per_day_value_box <-
    renderValueBox({
      req(input$select_date_of_review)
      read_file()$contacts_df_long %>% 
      contacts_per_day_value_box()
      
    })
  
  
  output$cumulative_contacts_value_box <-
    renderValueBox({
      req(input$select_date_of_review)
      read_file()$contacts_df_long %>% 
        cumulative_contacts_value_box()
      
    })
  
  
  output$contacts_under_surveillance_value_box <-
    renderValueBox({
      req(input$select_date_of_review)
      read_file()$contacts_df_long %>% 
        contacts_under_surveillance_value_box()
      
    })
  
  output$pct_contacts_followed_value_box <-
    renderValueBox({
      req(input$select_date_of_review)
      read_file()$contacts_df_long %>% 
        pct_contacts_followed_value_box()
      
    })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~ all_contacts_tab_row_1 ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  output$all_contacts_per_region_table <- 
    renderReactable({
      req(input$select_date_of_review)
      
      read_file()$contacts_df_long %>% 
        all_contacts_per_region_table()
      })
  
  
  output$all_contacts_per_region_sunburst_plot <- 
    renderHighchart({
      req(input$select_date_of_review)
      
      read_file()$contacts_df_long %>% 
        all_contacts_per_region_sunburst_plot()
      })
  
  output$all_contacts_per_region_text <- 
    renderUI({
      req(input$select_date_of_review)
      
      read_file()$contacts_df_long %>% 
        all_contacts_per_region_text()
      })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~ all_contacts_tab_row_2 ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  output$contacts_under_surveillance_per_region_over_time_bar_chart <- 
    renderHighchart({
      req(input$select_date_of_review)
      
      read_file()$contacts_df_long %>% 
        contacts_under_surveillance_per_region_over_time_bar_chart()
    })
  
  output$contacts_under_surveillance_per_region_over_time_bar_chart_relative <- 
    renderHighchart({
      req(input$select_date_of_review)
      
      read_file()$contacts_df_long %>% 
        contacts_under_surveillance_per_region_over_time_bar_chart_relative()
    })
  
  output$contacts_under_surveillance_per_region_over_time_text <- 
    renderUI({
      req(input$select_date_of_review)
      
      read_file()$contacts_df_long %>% 
        contacts_under_surveillance_per_region_over_time_text()
    })
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~ all_contacts_tab_row_3 ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$total_contacts_per_case_donut_plot <- 
    renderHighchart({
      req(input$select_date_of_review)
      
      read_file()$contacts_df_long %>% 
        total_contacts_per_case_donut_plot()
    })
  
  output$total_contacts_per_case_table <- 
    renderReactable({
      req(input$select_date_of_review)
      
      read_file()$contacts_df_long %>% 
        total_contacts_per_case_table()
    })
  
  
  output$total_contacts_per_case_text <- 
    renderUI({
      req(input$select_date_of_review)
      
      read_file()$contacts_df_long %>% 
        total_contacts_per_case_text()
    })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~ all_contacts_tab_row_4 ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$total_contacts_per_link_type_donut_plot <- 
    renderHighchart({
      req(input$select_date_of_review)
      
      read_file()$contacts_df_long %>% 
        total_contacts_per_link_type_donut_plot()
    })
    
  
  
  output$total_contacts_per_link_type_text <- 
    renderUI({
      req(input$select_date_of_review)
      
      read_file()$contacts_df_long %>% 
        total_contacts_per_link_type_text()
    })
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~ all_contacts_tab_row_5 ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$total_contacts_vaccinated_bar_plot <- 
    renderHighchart({
      req(input$select_date_of_review)
      
      read_file()$contacts_df_long %>% 
        total_contacts_vaccinated_bar_plot()
    })
  
  
  
  output$total_contacts_vaccinated_text <- 
    renderUI({
      req(input$select_date_of_review)
      
      read_file()$contacts_df_long %>% 
        total_contacts_vaccinated_text()
    })
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~ all_contacts_tab_row_6 ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$active_contacts_breakdown_bar_chart <- 
    renderEcharts4r({
      req(input$select_date_of_review)
      
      read_file()$contacts_df_long %>% 
        active_contacts_breakdown_bar_chart()
    })
  
  output$active_contacts_breakdown_table <- 
    renderReactable({
      req(input$select_date_of_review)
      
      read_file()$contacts_df_long %>% 
        active_contacts_breakdown_table()
    })
  
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~ all_contacts_tab_row_7 ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 
  # 
  # 
  # output$snake_plot_date_slider <- renderUI({
  #   req(input$select_date_of_review) # require general date of review select. Can't remember why
  #   
  #   read_file()$contacts_df  %>% 
  #     snake_plot_date_slider()
  #   
  # })
  # 
  # # ~~~ stage_output ----
  # 
  # output$snake_plot_nrows_selected_text <- renderUI({
  #   
  #   req(input$snake_plot_date_slider)
  #   
  #     snake_plot_nrows_selected_text(contacts_df = read_file()$contacts_df , 
  #                                    snake_plot_date_slider = input$snake_plot_date_slider)
  #   
  #   
  # })
  # 
  # # ~~~ snake_plot_sample_or_not ----
  # 
  # output$snake_plot_sample_or_not <- renderUI({
  #   
  #   req(input$snake_plot_date_slider)
  # 
  #   snake_plot_sample_or_not(contacts_df = read_file()$contacts_df, 
  #                            snake_plot_date_slider = input$snake_plot_date_slider)
  #   
  #   
  # })

  
  
  output$contacts_timeline_snake_text <- renderUI({
    
    contacts_timeline_snake_text(contacts_df_long = read_file()$contacts_df_long , 
                                 todays_date = todays_date_reactive())
    
    
  })
  
    
  # ~~~ generate_snake_plot_bttn ----
  output$generate_snake_plot_bttn <- renderUI({
    actionBttn(inputId = "generate_snake_plot_bttn", 
               label = "Generate snake plot", 
               style = "jelly", color = "primary",  size = "xs"
    )
  })
  
  
  # ~~~ stage_output ----
  output_staging <- reactiveValues()
  
  # ~~~ observeEvent_generate_snake_plot_bttn ----
  observeEvent(input$generate_snake_plot_bttn,{  
    output_staging$contacts_timeline_snake_plot <- 
      read_file()$contacts_df_long %>% 
      contacts_timeline_snake_plot(todays_date = todays_date_reactive())
  })
  
  # ~~~ contacts_timeline_snake_plot ----
  output$contacts_timeline_snake_plot <-
    renderEcharts4r({
      req(input$generate_snake_plot_bttn)
      output_staging$contacts_timeline_snake_plot
    })
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~ all_contacts_tab_row_8 ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  output$contacts_lost_24_to_72_hours <- 
    render_gt({
      req(input$select_date_of_review)
      
      read_file()$contacts_df_long %>% 
        contacts_lost_24_to_72_hours()
    })
  
  
  output$lost_contacts_linelist <- 
    renderReactable({
      req(input$select_date_of_review)
      
      read_file()$contacts_df_long %>% 
        lost_contacts_linelist()
    })
  
  
  

  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~~  all_contacts_tab_regional ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  output$contacts_tab_select_regional <- renderUI({
    
    req(input$select_date_of_review)

  selectInput("contacts_tab_select_regional", 
              label = "Choose region",
              choices = unique(read_file_raw()$prefecture),
              multiple = FALSE)
      
    })
  
  
  # ~~~ select_date_of_review_regional----
  output$select_date_of_review_regional <- renderUI({
    
    
    flattened_dates <- 
      read_file_unfiltered()$contacts_df_long%>% 
      select(follow_up_date) %>%
      pull(1)
    
    min_date <- min(flattened_dates, na.rm = T) 
    max_date <- max(flattened_dates, na.rm = T) 
    
    dateInput("select_date_of_review_regional", 
              label = "Select date", 
              value = todays_date, 
              min = min_date, 
              max = max_date)
    
  })
  
  
  # ~~~ all_contacts_tab_row_0_regional ----
  
  
  output$contacts_per_day_value_box_regional <-
    renderValueBox({
      req(input$select_date_of_review_regional)
      read_file_regional()$contacts_df_long %>% 
        contacts_per_day_value_box_regional()
      
    })
  
  
  output$cumulative_contacts_value_box_regional <-
    renderValueBox({
      req(input$select_date_of_review_regional)
      read_file_regional()$contacts_df_long %>% 
        cumulative_contacts_value_box_regional()
      
    })
  
  
  output$contacts_under_surveillance_value_box_regional <-
    renderValueBox({
      req(input$select_date_of_review_regional)
      read_file_regional()$contacts_df_long %>% 
        contacts_under_surveillance_value_box_regional()
      
    })
  
  output$pct_contacts_followed_value_box_regional <-
    renderValueBox({
      req(input$select_date_of_review_regional)
      read_file_regional()$contacts_df_long %>% 
        pct_contacts_followed_value_box_regional()
      
    })
  

  
  # ~~~ all_contacts_tab_row_1_regional ----
  
  
  output$all_contacts_per_region_table_regional <- 
    renderReactable({
      req(input$select_date_of_review_regional)
      
      read_file_regional()$contacts_df_long %>% 
        all_contacts_per_region_table_regional()
    })
  
  
  output$all_contacts_per_region_sunburst_plot_regional <- 
    renderHighchart({
      req(input$select_date_of_review_regional)
      
      read_file_regional()$contacts_df_long %>% 
        all_contacts_per_region_sunburst_plot_regional()
    })
  
  output$all_contacts_per_region_text_regional <- 
    renderUI({
      req(input$select_date_of_review_regional)
      
      read_file_regional()$contacts_df_long %>% 
        all_contacts_per_region_text_regional()
    })
  
  # ~~~ all_contacts_tab_row_2_regional ----
  
  
  output$contacts_under_surveillance_per_region_over_time_bar_chart_regional <- 
    renderHighchart({
      req(input$select_date_of_review_regional)
      
      read_file_regional()$contacts_df_long %>% 
        contacts_under_surveillance_per_region_over_time_bar_chart_regional()
    })
  
  output$contacts_under_surveillance_per_region_over_time_bar_chart_relative_regional <- 
    renderHighchart({
      req(input$select_date_of_review_regional)
      
      read_file_regional()$contacts_df_long %>% 
        contacts_under_surveillance_per_region_over_time_bar_chart_relative_regional()
    })
  
  output$contacts_under_surveillance_per_region_over_time_text_regional <- 
    renderUI({
      req(input$select_date_of_review_regional)
      
      read_file_regional()$contacts_df_long %>% 
        contacts_under_surveillance_per_region_over_time_text_regional()
    })
  
  
  
  # ~~~ all_contacts_tab_row_3_regional ----
  
  output$total_contacts_per_case_donut_plot_regional <- 
    renderHighchart({
      req(input$select_date_of_review_regional)
      
      read_file_regional()$contacts_df_long %>% 
        total_contacts_per_case_donut_plot_regional()
    })
  
  output$total_contacts_per_case_table_regional <- 
    renderReactable({
      req(input$select_date_of_review_regional)
      
      read_file_regional()$contacts_df_long %>% 
        total_contacts_per_case_table_regional()
    })
  
  
  output$total_contacts_per_case_text_regional <- 
    renderUI({
      req(input$select_date_of_review_regional)
      
      read_file_regional()$contacts_df_long %>% 
        total_contacts_per_case_text_regional()
    })
  
  
  # ~~~ all_contacts_tab_row_4_regional ----
  
  output$total_contacts_per_link_type_donut_plot_regional <- 
    renderHighchart({
      req(input$select_date_of_review_regional)
      
      read_file_regional()$contacts_df_long %>% 
        total_contacts_per_link_type_donut_plot_regional()
    })

  
  output$total_contacts_per_link_type_text_regional <- 
    renderUI({
      req(input$select_date_of_review_regional)
      
      read_file_regional()$contacts_df_long %>% 
        total_contacts_per_link_type_text_regional()
    })
  
  
  # ~~~ all_contacts_tab_row_5_regional ----
  
  output$total_contacts_vaccinated_bar_plot_regional <- 
    renderHighchart({
      req(input$select_date_of_review_regional)
      
      read_file_regional()$contacts_df_long %>% 
        total_contacts_vaccinated_bar_plot_regional()
    })
  
  
  output$total_contacts_vaccinated_text_regional <- 
    renderUI({
      req(input$select_date_of_review_regional)
      
      read_file_regional()$contacts_df_long %>% 
        total_contacts_vaccinated_text_regional()
    })
  

  
  # ~~~ all_contacts_tab_row_6_regional ----
  
  
  output$active_contacts_breakdown_bar_chart_regional <- 
    renderHighchart({
      req(input$select_date_of_review_regional)
      
      read_file_regional()$contacts_df_long %>% 
        active_contacts_breakdown_bar_chart_regional()
    })
  
  output$active_contacts_breakdown_table_regional <- 
    renderReactable({
      req(input$select_date_of_review_regional)
      
      read_file_regional()$contacts_df_long %>% 
        active_contacts_breakdown_table_regional()
    })
  

  
  # ~~~ all_contacts_tab_row_7_regional ----
  
  output$contacts_lost_24_to_72_hours_regional <- 
    render_gt({
      req(input$select_date_of_review_regional)
      
      read_file_regional()$contacts_df_long %>% 
        contacts_lost_24_to_72_hours_regional()
    })
  
  
  output$lost_contacts_linelist_regional <- 
    renderReactable({
      req(input$select_date_of_review_regional)
      
      read_file_regional()$contacts_df_long %>% 
        lost_contacts_linelist_regional()
    })
  
  
  
  # ~~~ all_contacts_tab_row_8_regional ----
  
  output$snake_plot_slider_regional <- renderUI({
    req(input$select_date_of_review_regional)
    
    
    contacts_df <- read_file_regional()$contacts_df %>% filter(prefecture == input$contacts_tab_select_regional)
    contacts_df_min_row_id <- contacts_df$row_id[1]
    contacts_df_max_row_id <- tail(contacts_df$row_id, 1)
    contacts_df_50th_row_id <- contacts_df$row_id[50]
    if(is.na(contacts_df_50th_row_id)){
      contacts_df_50th_row_id <- max(contacts_df$row_id)
    }
    
    sliderInput("snake_plot_slider_regional", 
                label = h4("Select row IDs"), 
                min = contacts_df_min_row_id, 
                max = contacts_df_max_row_id, 
                dragRange = TRUE,
                value = c(contacts_df_min_row_id, 
                          contacts_df_50th_row_id))
  })
  
  
  output$generate_snake_plot_bttn_regional <- renderUI({
    actionBttn(inputId = "generate_snake_plot_bttn_regional", 
               label = "Generate snake plot", 
               style = "jelly", color = "primary",  size = "xs"
    )
  })
  
  
  
  output$contacts_timeline_snake_plot_regional <- 
    renderHighchart({
      
      req(input$generate_snake_plot_bttn_regional)
      
      read_file_regional()$contacts_df_long %>% 
        contacts_timeline_snake_plot_regional()
    })
  
  
  
  
}

#polished::secure_server(server)



