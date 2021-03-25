server <- function(input, output) {  
  
  
  source(here("helper_scripts/server_functions.R"), local = T)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~~  reactives ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 
  # observeEvent(input$Analyze, {
  #   read_file()
  #   })
  # 
  # observeEvent(input$Analyze, {
  #   change_todays_date()
  # })

  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~~  load_data_tab ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # ~~~ Choose data to use UI element----
  output$data_to_use <- renderUI({
    radioButtons(inputId = "data_to_use_id", 
                 label = "Input Data", 
                 choices = c("Use preloaded data", 
                             "Use uploaded data"))
  })
  
  # ~~~ Load data UI element----
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
  
  # ~~~ Analyze action button UI element----
  output$analyze_action_bttn <- renderUI({
    actionBttn(inputId = "Analyze", label = "Analyze", 
               style = "jelly", color = "primary",
               )
  })
  
  
  
  # ~~~ select_date_of_review UI element----
  
  output$select_date_of_review <- renderUI({

      dateInput("select_date_of_review", 
                label = "Select date of review", 
                value = todays_date)
    
  })

   #~~ data_completeness_plot ---------------------------
  output$data_completeness_plot <- 
    renderPlot({
      read_file()$contacts_df_raw %>% 
      data_completeness_plot()
    }) 

  #~~ data_cardinality_plot ---------------------------
  output$data_cardinality_plot <- 
    renderPlot({
      read_file()$contacts_df_raw %>% 
      data_cardinality_plot()})
  
  #~~ reactable_table ---------------------------
  output$reactable_table <- 
    renderReactable({
      read_file()$contacts_df_raw %>% 
        reactable_table()})
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~~  all_contacts_tab ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # ~~~ all_contacts_tab_row_0 ----
  
  
  output$contacts_registered_per_day_value_box <-
    renderValueBox({
      read_file()$contacts_df_long %>% 
      contacts_registered_per_day_value_box()
      
    })
  
  
  output$cumulative_contacts_registered_value_box <-
    renderValueBox({
      read_file()$contacts_df_long %>% 
        cumulative_contacts_registered_value_box()
      
    })
  
  
  output$contacts_under_surveillance_value_box <-
    renderValueBox({
      read_file()$contacts_df_long %>% 
        contacts_under_surveillance_value_box()
      
    })
  
  output$pct_contacts_followed_value_box <-
    renderValueBox({
      read_file()$contacts_df_long %>% 
        pct_contacts_followed_value_box()
      
    })
  
  # ~~~ all_contacts_tab_row_1 ----
  
  output$all_contacts_per_region_table <- 
    renderReactable({
      read_file()$contacts_df_long %>% 
        all_contacts_per_region_table()
      })
  
  
  output$all_contacts_per_region_sunburst_plot <- 
    renderHighchart({
      read_file()$contacts_df_long %>% 
        all_contacts_per_region_sunburst_plot()
      })
  
  output$all_contacts_per_region_text <- 
    renderUI({
      read_file()$contacts_df_long %>% 
        all_contacts_per_region_text()
      })
  
  # ~~~ all_contacts_tab_row_2 ----
  
  output$contacts_under_surveillance_per_region_over_time_bar_chart <- 
    renderHighchart({
      read_file()$contacts_df_long %>% 
        contacts_under_surveillance_per_region_over_time_bar_chart()
    })
  
  output$contacts_under_surveillance_per_region_over_time_bar_chart_relative <- 
    renderHighchart({
      read_file()$contacts_df_long %>% 
        contacts_under_surveillance_per_region_over_time_bar_chart_relative()
    })
  
  output$contacts_under_surveillance_per_region_over_time_text <- 
    renderUI({
      read_file()$contacts_df_long %>% 
        contacts_under_surveillance_per_region_over_time_text()
    })
  
  
  
  # ~~~ all_contacts_tab_row_3 ----
  
  output$total_contacts_per_case_donut_plot <- 
    renderHighchart({
      read_file()$contacts_df_long %>% 
        total_contacts_per_case_donut_plot()
    })
  
  output$total_contacts_per_case_table <- 
    renderReactable({
      read_file()$contacts_df_long %>% 
        total_contacts_per_case_table()
    })
  
  
  output$total_contacts_per_case_text <- 
    renderUI({
      read_file()$contacts_df_long %>% 
        total_contacts_per_case_text()
    })
  
  
  # ~~~ all_contacts_tab_row_4 ----
  
  output$total_contacts_per_link_type_donut_plot <- 
    renderHighchart({
      read_file()$contacts_df_long %>% 
        total_contacts_per_link_type_donut_plot()
    })
    
  
  
  output$total_contacts_per_link_type_text <- 
    renderUI({
      read_file()$contacts_df_long %>% 
        total_contacts_per_link_type_text()
    })
  
  
  # ~~~ all_contacts_tab_row_5 ----
  
  output$total_contacts_vaccinated_bar_plot <- 
    renderHighchart({
      read_file()$contacts_df_long %>% 
        total_contacts_vaccinated_bar_plot()
    })
  
  
  
  output$total_contacts_vaccinated_text <- 
    renderUI({
      read_file()$contacts_df_long %>% 
        total_contacts_vaccinated_text()
    })
  
  # ~~~ all_contacts_tab_row_6 ----
  
  
  # ~~~ Generate snake plot UI element----
  
  output$snake_plot_slider <- renderUI({
    
    contacts_df <- read_file()$contacts_df
    contacts_df_min_row_id <- min(contacts_df$row_id)
    contacts_df_max_row_id <- max(contacts_df$row_id)
    
    
    
    sliderInput("snake_plot_slider", 
                label = h4("Select row IDs"), 
                min = contacts_df_min_row_id, 
                max = contacts_df_max_row_id, 
                dragRange = 50,
                value = c(contacts_df_min_row_id, 
                          contacts_df_min_row_id + 49))
    
  })
  
  
  output$generate_snake_plot_bttn <- renderUI({
    actionBttn(inputId = "generate_snake_plot_bttn", 
               label = "Generate snake plot", 
               style = "jelly", color = "primary",  size = "xs"
    )
  })
  
  
  
  output$contacts_timeline_snake_plot <- 
    renderHighchart({
      
      req(input$generate_snake_plot_bttn)
      
      read_file()$contacts_df_long %>% 
        active_contacts_timeline_snake_plot()
    })

  

  # ~~~ all_contacts_tab_row_7 ----
  
  output$contacts_lost_24_to_72_hours <- 
    render_gt({
      read_file()$contacts_df_long %>% 
        contacts_lost_24_to_72_hours()
    })
  
  
  output$lost_contacts_linelist <- 
    renderReactable({
      read_file()$contacts_df_long %>% 
        lost_contacts_linelist()
    })
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~~  regional_contacts_tab ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  output$regional_contacts_tab_select <- 
  
    renderUI({

  selectInput("regional_contacts_tab_select", 
              label = "Choose region",
              choices = unique(read_file()$contacts_df_long$prefecture),
              multiple = FALSE)
      
    })
  
  
  # ~~~ regional_select_date_of_review----
  output$regional_select_date_of_review <- renderUI({
    
    dateInput("regional_select_date_of_review", 
              label = "Select date of review", 
              value = todays_date)
    
  })
  
  

}

#polished::secure_server(server)



