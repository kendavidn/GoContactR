#' ---
#' title: "Server.R"
#' output:
#'  rmarkdown::html_document:
#'  toc: yes
#'  toc_depth: 2
#'  toc_float: yes
#'  number_sections: true
#' --- #tag_to_pull

## NOTE: Section headers on this file are duplicated. 
## One set of headers exist for knitting this to an Rmarkdown (for documentation)
## The second set are hooks for RStudio's document outline feature.

#' Increase upload size. See https://shiny.rstudio.com/articles/upload.html
options(shiny.maxRequestSize = 50 * 1024^2)


server <- function(input, output) {
  
  
#' # Source server functions
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~  Source server functions ------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Load in the primary functions that will be called within server.R
  source(here("helper_scripts/server_functions.R"), local = T)

  
  
#' # Source country-specific server functions
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~  Source country-specific server functions -----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Here we source a set of functions related to reading in and transforming the source data.
#' This is the only part of the app that changes between countries.
#' There are currently two versions of functions/reactives/UI elements that are sourced in this script.
#' - One version creates UI elements to enter Go.Data credentials and fetches the Go.Data data
#' - The other version creates UI elements to upload KoboCollect csvs.
#' 
  source(here(paste0("helper_scripts/server_functions_for_",
                     PARAMS$country_code, ".R")), local = T)


#' # Load data reactives
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~  Load data reactives --------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Each of the "read_file-" reactives sources its namesake function.
#' These functions are called in a reactive context to ensure that if the input data changes, all the output graphs will change as well.
  
  
#' ## read_file_raw_reactive
# ~~~~ read_file_raw_reactive --------------------
#' The read_file_raw function does either of two things.
#' - For countries using Go.Data, it takes in the input credentials, logs into a Go.Data session, and returns a list with the requisite dataframes.
#' - For countries using KoboCollect, it takes in the two uploaded csv files, (contact list and follow-up list), and returns them as a list of a dataframes.

  read_file_raw_reactive <- reactive({
    req(input$data_to_use)
    req(input$analyze_action_bttn)

    read_file_raw()
  })


#' ## read_file_transformed_reactive
# ~~~~ read_file_transformed_reactive -----------------
#' The 'read_file_transformed' function takes in data from read_file_raw_reactive, and 'transforms' it into a single, 'long' dataframe,
#' with one row per contact-follow-up-day
  
  read_file_transformed_reactive <- reactive({
    req(input$data_to_use)
    req(input$analyze_action_bttn)

    read_file_transformed(tracing_data_raw = read_file_raw_reactive())
  })


#' ## read_file_filtered_reactive
# ~~~~ read_file_filtered_reactive ------------------------
#' The 'read_file_filtered' function takes in data from read_file_transformed_reactive, as well as a date_of_review variable.
#' All contacts who had not begun follow-up by he selected date_of_review are removed from the dataframe.
#' In addition, for contacts in the midst of follow-up, follow-up days that are past the selected date_of_review will have their status changed to "Future follow-up" or "Suivi futur"
#' The output of read_file_filtered (which becomes the output of 'read_file_filtered_reactive') is the dataframe that feeds most graphs in the application.

  read_file_filtered_reactive <- reactive({
    req(input$analyze_action_bttn)
    req(input$filter_or_not)

    read_file_filtered()
  })
  
  

#' # action button observer
# ~~~~  action button observer ---------------------------
#' This observer simply triggers or re-triggers 'read_file_transformed_reactive' whenever the analyze action button is pressed
  
  observeEvent(input$analyze_action_bttn, {
    read_file_transformed_reactive()
  })



#' # Data overview section
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~  Data overview section----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' The plots below take in data from 'read_file_transformed_reactive' and output summary graphics.
#' Recall that the output of 'read_file_transformed_reactive' is the unfiltered, full, "long" dataframe, with one row per contact-follow-up-day.



#' ## data_completeness_plot
# ~~~~ data_completeness_plot ---------------------------
#' Here we output a visualization of the entire long dataframe.
#' Uses the viz_dat function
  
  output$data_completeness_plot <-
    renderPlot({
      req(read_file_transformed_reactive())

      if (input$data_to_use == "Use uploaded data") {
        req(input$uploaded_data_contacts_list)
        req(input$uploaded_data_follow_up_list)
      }

      if (input$data_to_use == "Use preloaded data") {
        req(input$preloaded_data_choice)
      }


      read_file_transformed_reactive() %>%
        data_completeness_plot()
    })

#' ## data_cardinality_plot
# ~~~~  data_cardinality_plot ---------------------------
#' This is a plot using the 'inspect_cat' function of the inspectdf package 
  output$data_cardinality_plot <-
    renderPlot({
      req(read_file_transformed_reactive())

      if (input$data_to_use == "Use uploaded data") {
        req(input$uploaded_data_contacts_list)
        req(input$uploaded_data_follow_up_list)
      }

      if (input$data_to_use == "Use preloaded data") {
        req(input$preloaded_data_choice)
      }

      read_file_transformed_reactive() %>%
        data_cardinality_plot()
    })


#' ## reactable_table
# ~~~~ reactable_table ---------------------------
#' Here, we output the entire long table for easy viewing, or searching
  
  output$reactable_table <-
    renderReactable({
      req(read_file_transformed_reactive())

      if (input$data_to_use == "Use uploaded data") {
        req(input$uploaded_data_contacts_list)
        req(input$uploaded_data_follow_up_list)
      }

      if (input$data_to_use == "Use preloaded data") {
        req(input$preloaded_data_choice)
      }

      read_file_transformed_reactive() %>%
        reactable_table()
    })



#' # Date selection
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~  Date selection --------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' This tab contains the primary graphs and tables that pertain to all contacts. 
#' (The next tab contains information pertaining to only active contacts, 
#' that is, contacts who are actively under surveillance)

  
  
#' ## select_date_of_review
# ~~~~ select_date_of_review --------------------
#' This picker lets you select a present or historical date, 
#' letting you see what the data looked like at each time point.
#' The range of dates that can be picked are bounded
#' by the range of dates in the dataframe (passed from 'read_file_transformed_reactive').
#' The picker also guesses what the date of review is. 
#' Basically, it obtains the last date for which follow-up status was not "missing" or "future",
#' (the last day on which there was any contact follow-up),
#' and assumes this is the date on which the user would like to view the data.
  
  output$select_date_of_review <- renderUI({
    
    ## date selection needs to use the unfiltered data frame
    ## because the selection from this input feeds the filtering function
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
               follow_up_status != "Future follow-up" &
               follow_up_statis != "NA" &
               !is.na(follow_up_status)) %>%
      select(follow_up_date) %>%
      pull(1) %>%
      max(na.rm = T)
    
    dateInput("select_date_of_review",
              label = "Select date of review",
              value = todays_date_imputed_from_data,
              min = min_date,
              max = max_date
    )
  })
  
#' ## todays_date_reactive
# ~~~~ todays_date_reactive --------------------
#' This reactive wraps the input from the select_date_of_review dateInput element,
#' It does not need to be inside of a reactive at the moment, 
#' but we have left it there anyways, as this would make it easy to trigger it 
#' later with ObservEevent, if needed.

  todays_date_reactive <- reactive({
    input$select_date_of_review
  })
  
  
#' # Generate downloadable report
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~  Generate downloadable report --------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  
#' ## select_report_format
#' Select which of five formats to use for the downloaded report.
#' (As at May 13, 2021, the PDF format is not working. 
#' Should hopefully work when this issue is sorted by the RStudio team: https://bit.ly/3oj0Kkv
#' )
# ~~~~ select_report_format---------------------------
  output$select_report_format <- renderUI({
    
    req(input$select_date_of_review)
    
    selectInput("report_format",
                label = "Select format",
                choices = c(
                  "pptx",
                  "docx",
                  #"pdf",
                  "html (page)",
                  "html (slides)"
                )
    )
  })
  
#' ## download_report_button
#' This is placed within a renderUI context so that we can hide it conditionally
# ~~~~ download_report_button---------------------------
  output$download_report_button <- renderUI({
    
    req(input$select_date_of_review)
    
    tagList(
      HTML("<p style='font-size:4px'>  <br><br>  </p>"),
      downloadBttn("report",
                   label = "Download report",
                   style = "jelly",
                   color = "primary", size = "md"
      )
    )
  })
  
  
#' ## download_report_button
#' This is placed within a renderUI context so that we can hide it conditionally
# ~~~~ download_report_button---------------------------

output$report <- download_report_function()
  

#' #  Dynamic filtering section
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~  Dynamic filtering section --------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' ## filters
#' A dynamic filtering section. The code below renders an input picker for each 
#' column in the dataset. The code is quite hairy, but we have tried to comment it extensively.
# ~~~~ ouput$filters----

  output$filters <- renderUI({
    req(input$analyze_action_bttn)
    req(input$select_date_of_review)
    req(input$filter_or_not)

    ## filters are only generated if the filter_or_not slider is set to TRUE
    if ((!is.null(input$filter_or_not)) && input$filter_or_not == TRUE) {
      
      my_data <-
        read_file_transformed_reactive() %>%
        as.data.frame() %>% ## not sure why but tibble doesn't work
        ## filters not created for rows that change across each contact
        select(-any_of(c("follow_up_date", "follow_up_day", "follow_up_status",
                         ## no filters for synthetic columns either
                         "row_id","row_number","sort_number", 
                         ## and no filters on names. 
                         ## Franck mentioned not printing names to app for privacy
                         "first_name", "last_name"))) %>%
        ## no filters for columns that are all NA
        janitor::remove_empty(which = "columns")
      
      
      selected_cols <- names(my_data)

      ## column names for each column
      labels <- lapply(1:length(selected_cols), function(x) {selected_cols[x]} )

      ## unique values of each column
      choices <- lapply(1:length(selected_cols), function(x) {
        unique(my_data[, selected_cols[x]])
      })


      ## render an input picker for each column
      lapply(1:length(labels), function(i) {
        
        ## create output$gender, output$region and so on based on column names
        output[[labels[[i]]]] <- renderUI({
          
          ## first subset the data to that column
          col <- my_data[, selected_cols[i]]
          
          ## then, below, we render an input picker based on whether the column is a character, number or date
          ## and also based on whether or not the column has NA values
          
          ## CHARACTER AND FACTOR COLUMNS  ~~~~~~~~~~~~~~~~~~~
          if (is.character(col) | is.factor(col)) {
            ## create pickerInput. 
            ## this would be accessed as input$gender, input$region and so on
            input_UI_element <-  pickerInput(id = labels[[i]],
                                             label = labels[[i]],
                                             choices = na.omit(choices[[i]]),
                                             selected = na.omit(choices[[i]]),
                                             options = list(`actions-box` = TRUE),
                                             multiple = TRUE)
            ## if col has NA values, checkboxinput asking whether to keep these
            if (any(is.na(col))) {
              tagList(input_UI_element, 
                      checkboxInput(id = paste0("na_", labels[[i]]),
                                    label = paste0(
                                      "Include contacts w. missing values for ",
                                      labels[[i]], "?"),
                                    value = TRUE))
              ## otherwise, print/return just the primary input picker
              } else { input_UI_element }
            
            
            ## NUMERIC COLUMNS  ~~~~~~~~~~~~~~~~~~~
            ## same procedure as with character columns
          } else if (is.numeric(col)) {
            input_UI_element <- sliderInput(labels[[i]],
                                            label = labels[[i]],
                                            min = min(col, na.rm = TRUE),
                                            max = max(col, na.rm = TRUE),
                                            value = c(min(col, na.rm = TRUE), 
                                                      max(col, na.rm = TRUE)))
            ## if col has NA values, checkboxinput asking whether to keep these
            if (any(is.na(col))) {
              tagList(input_UI_element, 
                      checkboxInput(id = paste0("na_", labels[[i]]),
                                    label = paste0(
                                      "Include contacts w. missing values for ",
                                      labels[[i]], "?"),
                                    value = TRUE))
              ## otherwise, print/return just the primary input picker
            } else { input_UI_element }
            
            
            ## DATE COLUMNS  ~~~~~~~~~~~~~~~~~~~
          } else if (lubridate::is.Date(col)) {
            input_UI_element <- dateRangeInput(labels[[i]],
                                               label = labels[[i]],
                                               min = min(col, na.rm = TRUE),
                                               max = max(col, na.rm = TRUE),
                                               start = min(col, na.rm = TRUE),
                                               end = max(col, na.rm = TRUE))
            ## if col has NA values, checkboxinput asking whether to keep these
            if (any(is.na(col))) {
              tagList(input_UI_element, 
                      checkboxInput(id = paste0("na_", labels[[i]]),
                                    label = paste0(
                                      "Include contacts w. missing values for ",
                                      labels[[i]], "?"),
                                    value = TRUE))
              ## otherwise, print/return just the primary input picker
            } else { input_UI_element }
            
          }
          })
        
      }
      )
            

      ## the large lapply function above created an input picker for each column
      ## now we output all those input pickers, 
      ## as in uiOutput("gender") and so on.
      lapply(1:length(labels), function(i) {
        uiOutput(labels[[i]])})
    }
    
    ## these uiOutputs are packages into a larger output, output$filters, 
    ## which will, finally, be placed in our UI, as in uiOutput("filters")
  })

  output$additional_filters_text <- renderUI({
    req(input$filter_or_not)

    if ((!is.null(input$filter_or_not)) && input$filter_or_not == TRUE) {
      tagList(
        h6("Use the input pickers and sliders to filter your data"),
        HTML("<font size='1'>
            Note that the following are not shown: <br>
          • Empty columns; <br>
          • Name columns; and <br>
          • the date of follow-up column <br>
          Also note that the options for each filter 
          do not react to selections on other filters.
                               </font>")
      )
    } else {
      HTML(c(" "))
    }
  })
  

#' #  OUTPUTS PERTAINING TO ALL CONTACTS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~ OUTPUTS PERTAINING TO ALL CONTACTS ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#' #  Value boxes
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~ Value boxes  --------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Below we call the value-box functions and return their outputs.
#' These functions return an HTML Shiny value-box when the report_format parameter
#' is "shiny", and a ggplot-based value-box otherwise.
#' Like most of the remaining functions that the app uses, these take in 
#' two primary inputs: the long contacts dataframe (one row per follow-up-day), 
#' and the date of review.

  output$contacts_per_day_value_box <-
    renderValueBox({
      
      req(input$select_date_of_review)
      req(input$analyze_action_bttn)
      
      ## require that there is actually data to be visualized
      shiny::validate(need(nrow(read_file_filtered_reactive()) > 0, message = FALSE))

      contacts_per_day_value_box(
        contacts_df_long = read_file_filtered_reactive(),
        todays_date = todays_date_reactive())
    })

  output$cumulative_contacts_value_box <-
    renderValueBox({
      req(input$select_date_of_review)
      req(input$analyze_action_bttn)
      shiny::validate(need(nrow(read_file_filtered_reactive()) > 0, message = FALSE))

      cumulative_contacts_value_box(
        contacts_df_long = read_file_filtered_reactive(),
        todays_date = todays_date_reactive())
      
    })

  output$contacts_under_surveillance_value_box <-
    renderValueBox({
      req(input$select_date_of_review)
      req(input$analyze_action_bttn)
      shiny::validate(need(nrow(read_file_filtered_reactive()) > 0, message = FALSE))

      contacts_under_surveillance_value_box(
        contacts_df_long = read_file_filtered_reactive(),
        todays_date = todays_date_reactive())
      
    })

  output$pct_contacts_followed_value_box <-
    renderValueBox({
      req(input$select_date_of_review)
      req(input$analyze_action_bttn)
      shiny::validate(need(nrow(read_file_filtered_reactive()) > 0, message = FALSE))

      pct_contacts_followed_value_box(
        contacts_df_long = read_file_filtered_reactive(),
        todays_date = todays_date_reactive())
      
    })

#' #  Contacts per region
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~ Contacts per region  --------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Below we call the contacts_per_admin_1 functions. These show the distribution 
#' of contacts over admin level 1 and admin level 2.

  output$all_contacts_per_admin_1_table <-
    renderReactable({
      req(input$select_date_of_review)

      all_contacts_per_admin_1_table(
        contacts_df_long = read_file_filtered_reactive(),
        todays_date = todays_date_reactive()
      )
    })


  output$all_contacts_per_admin_1_sunburst_plot <-
    renderHighchart({
      req(input$select_date_of_review)

      all_contacts_per_admin_1_sunburst_plot(
        contacts_df_long = read_file_filtered_reactive(),
        todays_date = todays_date_reactive()
      )
    })

  output$all_contacts_per_admin_1_bar_chart <-
    renderHighchart({
      req(input$select_date_of_review)

      all_contacts_per_admin_1_bar_chart(
        contacts_df_long = read_file_filtered_reactive(),
        todays_date = todays_date_reactive()
      )
    })

  ## dynamic text element
  output$all_contacts_per_admin_1_text <-
    renderUI({
      req(input$select_date_of_review)

      all_contacts_per_admin_1_text(
        contacts_df_long = read_file_filtered_reactive(),
        todays_date = todays_date_reactive()
      )
    })

#' #  Contacts surveilled over time
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~ Contacts surveilled over time  -----------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Functions showing how many contacts were under surveillance at each time point, 
#' segregated by region.

  output$contacts_surveilled_admin_1_bar_chart <-
    renderHighchart({
      req(input$select_date_of_review)

      contacts_surveilled_admin_1_bar_chart(
        contacts_df_long = read_file_filtered_reactive(),
        todays_date = todays_date_reactive()
      )
    })

  output$contacts_surveilled_admin_1_bar_chart_relative <-
    renderHighchart({
      req(input$select_date_of_review)

      contacts_surveilled_admin_1_bar_chart_relative(
        contacts_df_long = read_file_filtered_reactive(),
        todays_date = todays_date_reactive()
      )
    })

  output$contacts_surveilled_admin_1_text <-
    renderUI({
      req(input$select_date_of_review)

      contacts_surveilled_admin_1_text(
        contacts_df_long = read_file_filtered_reactive(),
        todays_date = todays_date_reactive()
      )
    })

  
#' #  Contacts per case
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~ Contacts per case  -----------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Functions that output the number of contacts linked to each case
#' At the moment (May 13, 2021), the Go.Data app version has no information for this column.

  output$total_contacts_per_case_donut_plot <-
    renderHighchart({
      req(input$select_date_of_review)

      total_contacts_per_case_donut_plot(
        contacts_df_long = read_file_filtered_reactive(),
        todays_date = todays_date_reactive()
      )
    })


  output$total_contacts_per_case_bar_chart <-
    renderHighchart({
      req(input$select_date_of_review)

      total_contacts_per_case_bar_chart(
        contacts_df_long = read_file_filtered_reactive(),
        todays_date = todays_date_reactive()
      )
    })


  output$total_contacts_per_case_text <-
    renderUI({
      req(input$select_date_of_review)

      total_contacts_per_case_text(
        contacts_df_long = read_file_filtered_reactive(),
        todays_date = todays_date_reactive()
      )
    })
  
#' #  Contacts per link type
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~ Contacts per link type  -----------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Functions that output the number of contacts for each type of link (e.g. family)
#' At the moment (May 13, 2021), the Go.Data app version has no information for this column.
  
  output$total_contacts_per_link_type_donut_plot <-
    renderHighchart({
      req(input$select_date_of_review)

      total_contacts_per_link_type_donut_plot(
        contacts_df_long = read_file_filtered_reactive(),
        todays_date = todays_date_reactive()
      )
    })

  output$total_contacts_per_link_type_bar_chart <-
    renderHighchart({
      req(input$select_date_of_review)

      total_contacts_per_link_type_bar_chart(
        contacts_df_long = read_file_filtered_reactive(),
        todays_date = todays_date_reactive()
      )
    })


  output$total_contacts_per_link_type_text <-
    renderUI({
      req(input$select_date_of_review)

      total_contacts_per_link_type_text(
        contacts_df_long = read_file_filtered_reactive(),
        todays_date = todays_date_reactive()
      )
    })
  

#' #  OUTPUTS PERTAINING TO ACTIVE CONTACTS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~ OUTPUTS PERTAINING TO ACTIVE CONTACTS ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#' #  Active contacts snake plot and bar chart
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~ Active contacts snake plot and bar chart  ------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' tbc

  output$active_contacts_timeline_snake_plot <-
    renderPlotly({
      req(input$select_date_of_review)

      active_contacts_timeline_snake_plot(
        contacts_df_long = read_file_filtered_reactive(),
        todays_date = todays_date_reactive(),
        legend_df = legend_df
      )
    })


  output$active_contacts_snake_plot_selected_table <-
    renderReactable({
      req(input$select_date_of_review)

      active_contacts_snake_plot_selected_table(
        contacts_df_long = read_file_filtered_reactive(),
        event_data("plotly_selecting")$customdata
      )
    })

  output$active_contacts_snake_plot_selected_table_download <-
    active_contacts_snake_plot_selected_table_download()


  output$active_contacts_timeline_table <-
    renderReactable({
      req(input$select_date_of_review)

      active_contacts_timeline_table(
        contacts_df_long = read_file_filtered_reactive(),
        todays_date = todays_date_reactive()
      )
    })

  output$active_contacts_timeline_table_download <-
    active_contacts_timeline_table_download()


  output$active_contacts_breakdown_bar_chart <-
    renderPlotly({
      req(input$select_date_of_review)

      active_contacts_breakdown_bar_chart(
        contacts_df_long = read_file_filtered_reactive(),
        todays_date = todays_date_reactive(),
        legend_df = legend_df
      )
    })


  output$active_contacts_breakdown_table <-
    renderReactable({
      req(input$select_date_of_review)

      active_contacts_breakdown_table(
        contacts_df_long = read_file_filtered_reactive(),
        todays_date = todays_date_reactive()
      )
    })

  output$active_contacts_breakdown_table_download <-
    active_contacts_breakdown_table_download()


  output$active_contacts_timeline_text <- renderUI({
    req(input$select_date_of_review)

    active_contacts_timeline_text(
      contacts_df_long = read_file_filtered_reactive(),
      todays_date = todays_date_reactive()
    )
  })

#' #  Contacts lost to follow-up
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~ Contacts lost to follow-up  ------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' tbc
  
  output$contacts_lost_24_to_72_hours_table <-
    render_gt({
      req(input$select_date_of_review)

      contacts_lost_24_to_72_hours_table(
        contacts_df_long = read_file_filtered_reactive(),
        todays_date = todays_date_reactive()
      )
    })

  output$contacts_lost_24_to_72_hours_table_download <-
    contacts_lost_24_to_72_hours_table_download()

  output$lost_contacts_linelist_table <-
    renderReactable({
      req(input$select_date_of_review)

      lost_contacts_linelist_table(
        contacts_df_long = read_file_filtered_reactive(),
        todays_date = todays_date_reactive()
      ) %>%
        .$output_table
    })

  output$lost_contacts_linelist_table_download <-
    lost_contacts_linelist_table_download()


  output$lost_contacts_linelist_table_title <-
    renderUI({
      req(input$select_date_of_review)

      lost_contacts_linelist_table(
        contacts_df_long = read_file_filtered_reactive(),
        todays_date = todays_date_reactive()
      ) %>%
        .$table_title
    })

  output$lost_contacts_linelist_text <- renderUI({
    req(input$select_date_of_review)

    lost_contacts_linelist_text(
      contacts_df_long = read_file_filtered_reactive(),
      todays_date = todays_date_reactive()
    )
  })
}
