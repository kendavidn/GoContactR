
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   load_data_tab ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



read_file_raw <- function(data_to_use,
                          preloaded_data_options, 
                          preloaded_data_choice, 
                          uploaded_data_contacts_list_path,
                          uploaded_data_follow_up_list_path){
  
  if (data_to_use == "Use preloaded data") {
    
    contacts_list <- 
      preloaded_data_options[[preloaded_data_choice]]$contacts_list %>% 
      clean_names() %>%
      type_convert() %>% 
      mutate(region_de_residence = str_to_sentence(region_de_residence)) # important for picking regions in data subset
    
    follow_up_list <-
      preloaded_data_options[[preloaded_data_choice]]$follow_up_list %>% 
      clean_names() %>%
      type_convert()
    
    
  } else if (data_to_use == "Use uploaded data") {
    contacts_list <-
      uploaded_data_contacts_list_path %>% 
      rio::import()  %>%
      clean_names() %>%
      type_convert() %>% 
      mutate(region_de_residence = str_to_sentence(region_de_residence)) # important for picking regions in data subset
    
    follow_up_list <-
      rio::import(uploaded_data_follow_up_list_path)  %>%
      clean_names() %>%
      type_convert() %>% 
      rename(code_unique_du_contact = quel_est_le_code_du_contact) ## need to rename now to permit join
  
  }
  
  
  tracing_data_raw  <- list(contacts_list = contacts_list, 
                            follow_up_list = follow_up_list)
  
  return(tracing_data_raw)
}


read_file_transformed <- function(tracing_data_raw){

  
  contacts_df_long_transformed <-
    tracing_data_raw %>%
    .$contacts_list %>% 
    mutate(counter = 1) %>%
    # row numbers to match Excel spreadsheet
    mutate(row_id = row_number() + 1) %>%
    # clean admin levels
    mutate(across(c(region_de_residence, district_de_residence),  # EDIT 2021-03-04 I changed it.  don't change region spelling as we used the raw spellings to populate the dropdown select on the regional tab
                  ~ .x %>%
                    str_to_lower() %>%
                    str_to_title() %>%
                    replace_na("NA") %>%
                    str_trim() %>% 
                    str_replace_all("  ", " "))) %>% 
    right_join(tracing_data_raw$follow_up_list, 
               by = "code_unique_du_contact") %>% 
    ## rename to match columns for which scripts were originally written
    rename_with(~
        case_when(.x == "code_unique_du_contact" ~ "id_contact",
                  .x == 'quel_est_le_nom_du_contact' ~ "nom",
                  .x == 'quel_est_le_prenom_du_contact' ~ 'prenom',
                  .x == 'quel_est_l_age_du_contact' ~ 'age',
                  .x == 'region_de_residence' ~ 'region',
                  .x == 'district_de_residence' ~ 'district',
                  .x == 'code_du_cas_index' ~ 'id',
                  .x == 'quel_est_le_lien_du_contact_avec_le_cas' ~ 'lien_avec_le_cas',
                  .x == 'quel_type_de_contact' ~ 'type_de_contact' ,
                  .x == 'date_du_dernier_contact_avec_le_cas' ~ 'date_du_dernier_contact',
                  .x == 'date_du_suivi' ~ 'follow_up_date',
                  .x == 'date_de_suivi' ~ 'follow_up_date',
                  .x == 'jour_du_suivi' ~ 'follow_up_day',
                  .x == 'jour_de_suivi' ~ 'follow_up_day',
                  .x == 'issue_du_suivi' ~ 'etat_suivi',
                  .x == 'issue_de_suivi' ~ 'etat_suivi',
                  .x == 'etat_du_suivi' ~ 'etat_suivi_simple',
                  .x == 'etat_de_suivi' ~ 'etat_suivi_simple',
                  TRUE ~ .x)) %>% 
    mutate(etat_suivi = replace_na(etat_suivi, "Manquant")) %>% 
    # - if follow-up lasted the full 21 days,
    # - change last follow_up state to "Fin de suivi"
    mutate(etat_suivi = if_else(follow_up_day == 10 & etat_suivi == "vu ou contacte",
                                    "Fin de suivi",
                                etat_suivi)) %>% 
    mutate(etat_suivi = str_to_sentence(etat_suivi)) %>% 
    ## shorten
    mutate(etat_suivi = recode(etat_suivi, 
                                      "Devenu symptomatique et resultats tests attendus" = "Symptomatique, resultats attendus"
                               )) %>% 
    ## shorten
    mutate(etat_suivi_simple = recode(etat_suivi_simple, 
                                      "vu ou contacte" = "Vu",
                                      "non vu ou contacte" = "Non vu"
                                      )) %>% 
    ## what exactly does poursuite du suivi mean?
    ## I am not sure. But in the meantime we replace it where possible
    mutate(etat_suivi = ifelse(etat_suivi_simple == "Non vu",
                               "Manquant",
                               etat_suivi)) %>%
    # convert dates to dates
    mutate(across(.cols = matches("date|Date"),
                  .fns = 
                    ~ .x %>% 
                    str_replace_all(" UTC", "") %>% 
                    as.Date())) %>% 
    mutate(region = replace_na(region, "Manquant")) %>% 
    mutate(id = replace_na(id, "Manquant")) %>% 
    mutate(lien_avec_le_cas = replace_na(lien_avec_le_cas, "Manquant")) %>% 
    group_by(row_id) %>% 
    complete(row_id, follow_up_date = seq.Date(unique(date_du_dernier_contact) + days(1), 
                                               unique(date_du_dernier_contact) + days(10),
                                               by = "1 days")) %>% 
    ## what does this do? I can't remember
    mutate(across(.cols = -tidyr::one_of("follow_up_date", "follow_up_day", 
                                         "etat_suivi", "etat_suivi_simple"),
                  .fns = ~ first(na.omit(.x))) ) %>% 
    ungroup() %>% 
    mutate(etat_suivi = replace_na(etat_suivi, "Manquant")) %>% 
    mutate(etat_suivi_simple = replace_na(etat_suivi_simple, "Manquant")) %>% 
    # for the simple version of follow up state, assume that manquant means no follow-up
    mutate(etat_suivi_simple = ifelse(etat_suivi_simple == "Manquant",
                                      "Non vu",
                                      etat_suivi_simple)) %>%
    mutate(row_number = row_number()) %>% 
    mutate(follow_up_day = as.numeric(follow_up_date - date_du_dernier_contact)) %>% 
    distinct(row_id, follow_up_date, .keep_all = TRUE) # not sure why there are duplicates but there are
  
  return(contacts_df_long_transformed)
  
}




read_file_transformed_regional <- function(contacts_df_long_transformed, 
                                           select_region){
  ##  why do we need read_file_transformed_regional AND read_file_transformed?
  ##  Because need to pass the version of the data that is filtered on REGION to the date picker for the region tab
  ##  we want to only show dates that are relevant for each region

  contacts_df_long_transformed_regional <- 
    contacts_df_long_transformed %>% 
    filter(region == select_region)
  
  return(contacts_df_long_transformed_regional)
    
  }



read_file_filtered <- function(contacts_df_long_transformed, 
                               todays_date, 
                               legend_df){
  
  # for legend colors. We only add them in after filtering
  # because after the filter, we are able to add in "suivi futur"
  
  contacts_df_long <-
    contacts_df_long_transformed %>%
    ## add future follow-up. 
    mutate(etat_suivi = if_else(follow_up_date > todays_date, 
                                "Suivi futur",
                                etat_suivi)) %>% 
    mutate(etat_suivi_simple = if_else(follow_up_date > todays_date, 
                                       "Suivi futur",
                                       etat_suivi_simple)) %>% 
    # keep only those for whom follow-up had begun by the date of review
    group_by(row_id) %>% 
    filter(min(follow_up_date) <= todays_date) %>%
    ungroup() %>% 
    # add legend colors
    left_join(legend_df, by = c("etat_suivi" = "breaks"))
  
  
  return(contacts_df_long)
  
}





read_file_filtered_regional <- function(contacts_df_long_transformed_regional,
                                        todays_date,
                                        legend_df){
      # for legend colors. We only add them in after filtering
      # because after the filter, we are able to add in "suivi futur"
      
    contacts_df_long_regional <-
      contacts_df_long_transformed_regional %>%
      ## add future follow-up.
      mutate(etat_suivi = if_else(follow_up_date > todays_date,
                                  "Suivi futur",
                                  etat_suivi)) %>%
      mutate(etat_suivi_simple = if_else(follow_up_date > todays_date,
                                         "Suivi futur",
                                         etat_suivi_simple)) %>%
      # keep only those for whom follow-up had begun by the date of review
      group_by(row_id) %>%
      filter(min(follow_up_date) <= todays_date) %>%
      ungroup() %>%
      # add legend colors
      left_join(legend_df, by = c("etat_suivi" = "breaks"))
      
      
      return(contacts_df_long_regional)
      
    }



# ~~~ data overview plots ----



data_completeness_plot <- function(contacts_df_long_transformed){
  
  contacts_df_long_transformed %>% 
    mutate(data_completeness_plot_row_id = row_number()) %>%  # long name so it does not conflict
    {if (nrow(.) > 1000) sample_n(., 1000) else .} %>% # sample if too large
    arrange(-data_completeness_plot_row_id) %>% 
    select(-data_completeness_plot_row_id) %>% 
    select(-any_of(x= c("sort_number", "counter", "row_id"))) %>% 
    select(-matches("autres_symptomes")) %>% 
    visdat::vis_dat(sort_type = FALSE) +
    scale_fill_paletteer_d(palette = "NineteenEightyR::sonny") +
    #my_theme+
    theme(axis.text.x = element_text(angle = 60, hjust = 0),) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) + 
    labs(title = "Summary of uploaded dataframe (two dataframes merged)", 
         subtitle = "Data types and missingness are shown.")+ 
    theme(title = element_text(face = "bold"))
  
}



data_cardinality_plot <- function(contacts_df_long_transformed){
  
  contacts_df_long_transformed %>% 
    {if (nrow(.) > 1000) sample_n(., 1000) else .} %>% # sample if too large
    select(!matches("colors")) %>% 
    inspectdf::inspect_cat() %>% 
    show_plot(col_palette = 4)+ 
    labs(title = "Freq. of categorical vars in dataset")
  
}


reactable_table <- 
  function(contacts_df_long_transformed){
    
    contacts_df_long_transformed %>% 
      select(!matches("nom|Nom")) %>% # remove names
      select(!matches("colors")) %>% 
      reactable(searchable = TRUE,
                striped = TRUE,
                highlight = TRUE,
                filterable = TRUE,
                theme = reactableTheme(stripedColor = "#f0f1fc",
                                       highlightColor = "#DADEFB",
                                       cellPadding = "8px 12px",
                                       style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif")
                ))
  }   


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~  app_tabs ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



download_report_function <- 
  function(
    #contacts_df_long, 
    #todays_date
    ) {
  downloadHandler(
  
  # For PDF output, change this to "report.pdf"
  ## Can't pass this in as a regular argument for some reason. See https://community.rstudio.com/t/switching-between-output-formats-while-generating-downloadable-rmarkdown-reports-from-shiny/71429
  
  filename = function(){
    
    extension <- switch(input$report_format, 
                            "pdf"= "pdf", 
                            "docx"= "docx", 
                            "html"= "html",
                            "pptx" = "pptx")
    
    paste0("gocontactr_report.", extension)
    },
  
  content = function(file) {
    
    withProgress(message = "Creating GoContactR Report", 
                 detail = "Initiating", {
      
      temp_dir <- tempdir()
      
      rmd_old_path <- "markdown/report.Rmd" 
      rmd_new_path <- file.path(temp_dir, "report.Rmd")
      
      docx_template_old_path <- "markdown/docx_template.docx"
      docx_template_new_path <- file.path(temp_dir, "docx_template.docx")
      
      pptx_template_old_path <- "markdown/pptx_template.pptx"
      pptx_template_new_path <- file.path(temp_dir, "pptx_template.pptx")
      
      # Copy Rmd to a temp dir. before knitting, in case we don't have write permissions to the current working dir (which can happen when deployed).
      file.copy(from = rmd_old_path, to = rmd_new_path, overwrite = TRUE)
      # templates in same temp dir. as rmd.
      file.copy(from = docx_template_old_path, to = docx_template_new_path, overwrite = TRUE)
      file.copy(from = pptx_template_old_path, to = pptx_template_new_path, overwrite = TRUE)
      
      
      # Set up params
      params <- list()
      ## normally I would pass all these params in as arguments to the function, 
      ## but they don't seem to work when I do this
      params$rendered_by_shiny <- TRUE
      params$contacts_df_long <- read_file_filtered_reactive()
      params$todays_date <- todays_date_reactive()
      params$report_format <- input$report_format
      

      if (input$report_format == "pdf"){
        # if output format is pdf
        # first create paged html
        html_doc <- rmarkdown::render(
          input = rmd_new_path,
          output_format = paged_windmill(logo = "markdown/logo.svg", 
                                                        logo_to_white = TRUE,
                                                        front_img = "markdown/front_img.jpg",
                                                        img_to_dark = TRUE),
          params = params,
          envir = new.env(parent = globalenv()) # child of the global environment (this isolates the code in the document from the code in this app). Not sure why. Rstudio says so,
        )
        # then covert paged html to pdf
        pagedown::chrome_print(html_doc, file, extra_args = c("--disable-gpu", 
                                                              "--no-sandbox"))
        
      } else {
      # for other output formats knit directly to output file
      rmarkdown::render(
        input = file.path(temp_dir, "report.Rmd"),
        output_file = file,
        output_format = switch(input$report_format, 
                               "docx"= rdocx_document(toc = TRUE, 
                                                      reference_docx = docx_template_new_path), 
                               "html"= html_document(theme = "cerulean", 
                                                     toc = TRUE,
                                                     toc_depth = 3,
                                                     toc_float = TRUE),
                               "pptx" = rpptx_document(reference_doc = pptx_template_new_path, 
                                                       slide_level = 3)
                               
                               ),
        
        params = params,
        envir = new.env(parent = globalenv())# child of the global environment (this isolates the code in the document from the code in this app). Not sure why. Rstudio says so,
      )
        
      }

      
      
    })
    
  }
)
  
}

# ~~~ app_tab_row_0 ----

contacts_per_day_value_box <- 
  function(contacts_df_long, todays_date){

    ## not sure why this is not working from the first time I created the follow_up_date_1 column
    contacts_df_long <- 
      contacts_df_long %>% 
      mutate(follow_up_date_1 = if_else(follow_up_day == 1, follow_up_date, NA_Date_))
      
    
      data_to_plot <- 
        contacts_df_long %>%
        filter(!is.na(follow_up_date_1)) %>% 
        count(follow_up_date_1) %>% 
        complete(follow_up_date_1 = seq.Date(from = min(.$follow_up_date_1), 
                                       to = todays_date, 
                                       by = "day"), 
                 fill = list(n = 0))
      
      cases_last_day <- 
        data_to_plot %>% 
        filter(follow_up_date_1 == todays_date) %>% 
        .$n
      
      date_last_day <- 
        todays_date %>% 
        format.Date("%b %d")
      
      
      highchart_to_plot <- 
        data_to_plot %>% 
        hchart("column", hcaes(x = follow_up_date_1, y = n), name = "No. contacts") %>% 
        hc_size(height = 85) %>% 
        hc_credits(enabled = FALSE) %>% 
        hc_add_theme(hc_theme_sparkline_vb()) 
      
      shiny_valuebox <- 
        valueBoxSpark(
        value = HTML(glue("{cases_last_day} <font size='1'> in past day ({date_last_day}) </font>")),
        title = toupper(glue("New contacts")),
        sparkobj = highchart_to_plot,
        info = "Bars show the no. of new contacts per day (based on first date of follow-up)",
        subtitle = HTML("<font size='1'> </font>"),
        #icon = icon("calendar-day"),
        width = 2,
        color = "aqua",
        href = NULL)
      
      
      ## ggplot version for rmarkdown
      ggplot_valuebox <- 
        data_to_plot %>% 
        ggplot() + 
        geom_col(aes(x= follow_up_date_1, y = n), fill = "white") + 
        labs(title = "**NEW CONTACTS**", 
             subtitle = glue::glue("**{cases_last_day}** in past day ({date_last_day})"), 
             x = "",
             y = "") + 
        scale_x_date(breaks = c(min(data_to_plot$follow_up_date_1),  
                                max(data_to_plot$follow_up_date_1)), 
                     labels = function(.x) format.Date(.x, format = "%b %d, '%y")) +
        theme_classic() +
        theme(panel.background = element_rect("#00BAEA"), 
              plot.background = element_rect("#00BAEA"), 
              plot.title = ggtext::element_textbox(color = "white", size = 15),
              plot.subtitle = ggtext::element_textbox(color = "white", size = 13), 
              panel.grid.major = element_blank(), 
              axis.line = element_blank(), 
              axis.text = element_text(size = 10, color = "white", face = "bold"), 
              axis.text.x = element_text(hjust = .8),
              axis.ticks.length = unit(.2, "cm"), 
              axis.ticks = element_line(color = "white", size = 1)) 
      
      return(list(shiny_valuebox = shiny_valuebox, 
                  ggplot_valuebox = ggplot_valuebox))
      
      
    
  }


cumulative_contacts_value_box <- 
  function(contacts_df_long, todays_date){
    
    
    ## not sure why this is not working from the first time I created the follow_up_date_1 column
    contacts_df_long <- 
      contacts_df_long %>% 
      mutate(follow_up_date_1 = if_else(follow_up_day == 1, follow_up_date, NA_Date_))
    
    
    data_to_plot <- 
      contacts_df_long %>%
      filter(!is.na(follow_up_date_1)) %>% 
      count(follow_up_date_1) %>% 
      complete(follow_up_date_1 = seq.Date(from = min(.$follow_up_date_1), 
                                           to = todays_date, 
                                           by = "day"), 
               fill = list(n = 0)) %>% 
      mutate(cum_n = cumsum(n))
    
    
    cases_last_day <- 
      data_to_plot %>% 
      filter(follow_up_date_1 == todays_date) %>% 
      .$cum_n
    
    date_last_day <- 
      todays_date %>% 
      format.Date("%b %d")
    
    
    highchart_to_plot <- 
      data_to_plot %>% 
      hchart("column", hcaes(x = follow_up_date_1, y = cum_n), name = "Cumul. contacts") %>% 
      hc_size(height = 85) %>% 
      hc_credits(enabled = FALSE) %>% 
      hc_add_theme(hc_theme_sparkline_vb()) 
    
    shiny_valuebox <- 
      valueBoxSpark(
      value = HTML(glue("{cases_last_day} <font size='1'> by {date_last_day} </font>")),
      title = toupper(glue("Cumul. contacts")),
      sparkobj = highchart_to_plot,
      info = "Bars show the cumulative contacts as at each day.",
      subtitle = HTML(" <font size='1'> </font>"),
      #icon = icon("calendar-alt"),
      width = 2,
      color = "teal",
      href = NULL)
    
    
    ## ggplot version for rmarkdown
    ggplot_valuebox <- 
      data_to_plot %>% 
      ggplot() + 
      geom_col(aes(x= follow_up_date_1, y = cum_n), fill = "white") + 
      labs(title = "**CUMUL. CONTACTS**", 
           subtitle = glue::glue("**{cases_last_day}** by {date_last_day}"), 
           x = "",
           y = "") + 
      scale_x_date(breaks = c(min(data_to_plot$follow_up_date_1),  
                              max(data_to_plot$follow_up_date_1)), 
                   labels = function(.x )format.Date(.x, format = "%b %d, '%y")) +
      theme_classic() +
      theme(panel.background = element_rect("#0EC6C5"), 
            plot.background = element_rect("#0EC6C5"), 
            plot.title = ggtext::element_textbox(color = "white", size = 15),
            plot.subtitle = ggtext::element_textbox(color = "white", size = 13), 
            panel.grid.major = element_blank(), 
            axis.line = element_blank(), 
            axis.text = element_text(size = 10, color = "white", face = "bold"), 
            axis.text.x = element_text(hjust = .8),
            axis.ticks.length = unit(.2, "cm"), 
            axis.ticks = element_line(color = "white", size = 1)) 
    
    return(list(shiny_valuebox = shiny_valuebox, 
                ggplot_valuebox = ggplot_valuebox))
    
  }


contacts_under_surveillance_value_box <- 
  function(contacts_df_long, todays_date){
    
      
    data_to_plot <- 
      contacts_df_long %>%
      count(follow_up_date) %>% 
      # drop follow'ups past today's date
      filter(follow_up_date <= todays_date) %>% 
      complete(follow_up_date = seq.Date(from = min(contacts_df_long$follow_up_date), 
                                     to = todays_date, 
                                     by = "day"), 
               fill = list(n = 0))
    
    
    cases_last_day <- 
      data_to_plot %>% 
      filter(follow_up_date == todays_date) %>% 
      .$n
    
    date_last_day <- 
      todays_date %>% 
      format.Date("%b %d")
    

    highchart_to_plot <- 
      data_to_plot %>% 
      hchart("column", hcaes(x = follow_up_date, y = n), name = "No. under surveillance") %>% 
      hc_size(height = 85) %>% 
      hc_credits(enabled = FALSE) %>% 
      hc_add_theme(hc_theme_sparkline_vb()) 
    
    shiny_valuebox <- 
      valueBoxSpark(
      value = HTML(glue("{cases_last_day} <font size='1'> as at {date_last_day} </font>")),
      title = toupper(glue("No. under surveillance")),
      sparkobj = highchart_to_plot,
      info = " Bars show the number that should be under surveillance on each day, based on date of last contact with a case",
      subtitle = HTML(" <font size='1'> </font>"),
      #icon = icon("binoculars"),
      width = 2,
      color = "purple",
      href = NULL)
    
    ## ggplot version for rmarkdown
    ggplot_valuebox <- 
      data_to_plot %>% 
      ggplot() + 
      geom_col(aes(x= follow_up_date, y = n), fill = "white") + 
      labs(title = "**NO. UNDER SURVEILLANCE**", 
           subtitle = glue::glue("**{cases_last_day}** as at {date_last_day}"), 
           x = "",
           y = "") + 
      scale_x_date(breaks = c(min(data_to_plot$follow_up_date),  
                              max(data_to_plot$follow_up_date)), 
                   labels = function(.x )format.Date(.x, format = "%b %d, '%y")) +
      theme_classic() +
      theme(panel.background = element_rect("#51539B"), 
            plot.background = element_rect("#51539B"), 
            plot.title = ggtext::element_textbox(color = "white", size = 15),
            plot.subtitle = ggtext::element_textbox(color = "white", size = 13), 
            panel.grid.major = element_blank(), 
            axis.line = element_blank(), 
            axis.text = element_text(size = 10, color = "white", face = "bold"), 
            axis.text.x = element_text(hjust = .8),
            axis.ticks.length = unit(.2, "cm"), 
            axis.ticks = element_line(color = "white", size = 1)) 
    
    return(list(shiny_valuebox = shiny_valuebox, 
                ggplot_valuebox = ggplot_valuebox))
    
    
  }



pct_contacts_followed_value_box <- 
  function(contacts_df_long, todays_date){
    

      data_to_plot <- 
      contacts_df_long %>%
      filter(follow_up_date <= todays_date) %>% 
      count(follow_up_date, etat_suivi_simple) %>% 
      bind_rows(data.frame(etat_suivi_simple = c("Vu", "Non vu"), 
                n = 0,
                follow_up_date = as.Date("2100-01-01")
                )) %>% 
      complete(follow_up_date = seq.Date(from = min(contacts_df_long$follow_up_date), 
                                         to = todays_date, 
                                         by = "day"), 
               etat_suivi_simple,
               fill = list(n = 0)) %>% 
      filter(follow_up_date != as.Date("2100-01-01")) %>% 
      group_by(follow_up_date) %>% 
      mutate(total = sum(n)) %>% 
      mutate(prop = n/total) %>% 
      mutate(pct = round(100*prop,0)) %>% 
      mutate(pct_paste = percent(prop)) %>% 
      ungroup() %>% 
      filter(etat_suivi_simple == "Vu") %>% 
      mutate(hc_ttip = glue("<b>Date:</b> {format.Date(follow_up_date,  format = '%a %b %d, %Y')}<br>
                        <b>{etat_suivi_simple}:</b> {pct_paste} ({n} of {total} )
                        "))
    
    pct_last_day <- 
      data_to_plot %>% 
      filter(follow_up_date == todays_date) %>% 
      .$pct_paste
    
    date_last_day <- 
      todays_date %>% 
      format.Date("%b %d")
    
    highchart_to_plot <- 
      data_to_plot %>%  
      hchart("area", hcaes(x = follow_up_date, y = pct)) %>% 
      hc_tooltip(formatter = JS("function(){return(this.point.hc_ttip)}")) %>% 
      hc_size(height = 85) %>% 
      hc_credits(enabled = FALSE) %>% 
      hc_add_theme(hc_theme_sparkline_vb()) 
    
    shiny_valuebox <- 
      valueBoxSpark(
      value = HTML(glue("{pct_last_day} <font size='1'> on {date_last_day} </font>")),
      title = toupper("% contacts followed"),
      sparkobj = highchart_to_plot,
      info = "Line plot shows the percentage followed on each day",
      subtitle = HTML(" <font size='1'> </font>"),
      #icon = icon("check-square"),
      width = 2,
      color = "yellow",
      href = NULL)
    
    ## ggplot version for rmarkdown
    ggplot_valuebox <- 
      data_to_plot %>% 
      ggplot() + 
      geom_line(aes(x= follow_up_date, y = pct), color = "white", 
                size = 2) + 
      labs(title = "**% CONTACTS FOLLOWED**", 
           subtitle = glue::glue("**{pct_last_day}** on {date_last_day}"), 
           x = "",
           y = "") + 
      scale_x_date(breaks = c(min(data_to_plot$follow_up_date),  
                              max(data_to_plot$follow_up_date)), 
                   labels = function(.x )format.Date(.x, format = "%b %d, '%y")) +
      theme_classic() +
      theme(panel.background = element_rect("#F88F26"), 
            plot.background = element_rect("#F88F26"), 
            plot.title = ggtext::element_textbox(color = "white", size = 15),
            plot.subtitle = ggtext::element_textbox(color = "white", size = 13), 
            panel.grid.major = element_blank(), 
            axis.line = element_blank(), 
            axis.text = element_text(size = 10, color = "white", face = "bold"), 
            axis.text.x = element_text(hjust = .8),
            axis.ticks.length = unit(.2, "cm"), 
            axis.ticks = element_line(color = "white", size = 1)) 
    
    return(list(shiny_valuebox = shiny_valuebox, 
                ggplot_valuebox = ggplot_valuebox))
    
    
  }




# ~~~ app_tab_row_1 ----


all_contacts_per_region_bar_chart <- 
  function(contacts_df_long, todays_date, report_format = "shiny"){
    
    
    contact_regions <-
      contacts_df_long %>%
      # filter out dates that are past the input days date
      filter(follow_up_date <= todays_date) %>% 
      group_by(row_id) %>% 
      # slice long frame
      slice_head() %>% 
      ungroup() %>%
      select(region, district) %>%
      add_count(region, name = "n_region") %>%
      mutate(pct_region = round(100 * n_region / nrow(.), 
                                digits = 2)) %>%
      group_by(region) %>%
      mutate(district = fct_lump(other_level = "Autres", 
                                 district, prop = 0.01)) %>%
      add_count(district, name = "n_district") %>%
      mutate(pct_district = round(100 * n_district / nrow(.), 
                                  digits = 2)) %>%
      mutate(data_label = paste0(n_district, " (", pct_district, "%", ")")) %>%
      group_by(region, district) %>%
      slice_head(n = 1) %>%
      ungroup() %>%
      arrange(-n_region, -n_district)
    
    
    color_df <- 
      data.frame(region = unique(contact_regions$region)) %>% 
      add_column(color_lvl_1 = highcharter_palette[1:nrow(.)])
    
    
    if (nrow(contact_regions) == 0) {
      return(highchart() %>% hc_title(text  = "No data to plot"))
    }
    
    
    subtitle <-  
      contact_regions %>% 
      left_join(color_df) %>% 
      select(region, pct_region, color_lvl_1) %>% 
      unique.data.frame() %>% 
      mutate(sep = case_when(row_number() == (n() - 1) ~ " and ",
                             row_number() == n() ~ "",
                             TRUE ~ ",")
      ) %>% 
      mutate(txt = stringr::str_glue("<strong><span style='background-color: {color_lvl_1};color:white'>
                                      &nbsp;{region}&nbsp;</span></strong> ({pct_region}% of contacts){sep}")) %>% 
      summarise(paste0(txt, collapse = "")) %>% 
      pull() %>% 
      stringr::str_c("Regions shown: ", .)
    
    
    
    output_highchart <- 
      contact_regions %>% 
      left_join(color_df) %>% 
      hchart("bar", hcaes(x = district , y = n_district, color = color_lvl_1),
             size = 4,
             name = "n",
             dataLabels = list(enabled = TRUE,
                               formatter = JS("function(){return(this.point.data_label)}"))) %>%
      hc_legend(enabled = TRUE) %>% 
      hc_plotOptions(series = list(groupPadding = 0)) %>% 
      hc_subtitle(text = subtitle, useHTML = TRUE) %>% 
      hc_xAxis(title = list(text = "District")) %>% 
      hc_yAxis(title = list(text = "Number of contacts"))
    
    
    
    if (report_format %in% c("pptx", "docx", "pdf")){
      # if report is one of the static rmarkdown output formats (not shiny or html)
      # then remove the animation so that the screenshot taken is of a fully loaded plot
      # and remove exporting as that is useless
      output_highchart <-
        output_highchart %>% 
        hc_exporting(enabled = FALSE) %>%
        hc_plotOptions(series = list(animation = FALSE)) %>% 
        html_webshot()
      # no need to return anything. html_webshot prints automatically
    }
    
    if (report_format %in% c("html", "shiny")){
      
    return(output_highchart)
      
    }
    
    
  }


all_contacts_per_region_sunburst_plot <- 
  function(contacts_df_long, todays_date, report_format = "shiny"){
    
    # filter out dates that are past the input days date
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(follow_up_date <= todays_date)
    
    
    contact_regions <-
      contacts_df_long %>%
      group_by(row_id) %>% 
      # slice long frame
      slice_head() %>% 
      ungroup() %>%
      select(region, district) %>%
      add_count(region, name = "n_region") %>%
      mutate(pct_region = round(100 * n_region / nrow(.), 
                                digits = 2)) %>%
      mutate(region = paste0(region, 
                             " (", pct_region, "%", ")")) %>%
      group_by(region) %>%
      mutate(district = fct_lump(other_level = "Autres", 
                                 district, prop = 0.01)) %>%
      add_count(district, name = "n_district") %>%
      mutate(pct_district = round(100 * n_district / nrow(.), 
                                  digits = 2)) %>%
      mutate(district = paste0(district, " (", pct_district, "%", ")")) %>%
      group_by(region, district) %>%
      slice_head(n = 1) %>%
      ungroup() %>%
      arrange(-n_region)
    
    if (nrow(contact_regions) == 0) {
      return(highchart() %>% hc_title(text  = "No data to plot"))
    }
    
    color_df <- 
      data.frame(region = unique(contact_regions$region)) %>% 
      add_column(color_lvl_1 = highcharter_palette[1:nrow(.)])
    
    contact_regions_list <- 
      contact_regions %>% 
      data_to_hierarchical(group_vars = c(region, 
                                          district), 
                           size_var = n_district, 
                           colors= color_df$color_lvl_1)
    
    
    x <- c("Type: ", "n = ")
    y <- c("{point.name}", "{point.value}")
    tltip <- tooltip_table(x, y)
    
    
    output_highchart <- 
      highchart() %>% 
      hc_chart(type = "sunburst") %>% 
      hc_add_series(data = contact_regions_list,
                    allowDrillToNode = TRUE,
                    levelIsConstant = FALSE,
                    #textOverflow = "clip",
                    levels = list(list(level = 1,
                                       dataLabels = list(enabled = TRUE,
                                                         color = "#FFFFFF",
                                                         style = list(textOverflow = "clip"))), 
                                  list(level = 2, 
                                       dataLabels = list(enabled = TRUE, 
                                                         color = "#FFFFFF",
                                                         style = list(textOverflow = "clip"))), 
                                  list(level = 3,
                                       dataLabels = list(enabled = TRUE, 
                                                         color = "#FFFFFF",
                                                         style = list(textOverflow = "clip"))))) %>% 
      hc_plotOptions(sunburst = list(dataLabels = list(enabled = TRUE) )) %>% 
      hc_tooltip(useHTML = TRUE, 
                 headerFormat = "", pointFormat = tltip) %>% 
      hc_exporting(enabled = TRUE)
    
    if (report_format %in% c("pptx", "docx", "pdf")){
      # if report is one of the static rmarkdown output formats (not shiny or html)
      # then remove the animation so that the screenshot taken is of a fully loaded plot
      # and remove exporting as that is useless
      output_highchart <-
        output_highchart %>% 
        hc_exporting(enabled = FALSE) %>%
        hc_plotOptions(series = list(animation = FALSE)) %>% 
        html_webshot()
      # no need to return anything. html_webshot prints automatically
    }
    
    if (report_format %in% c("html", "shiny")){
      
      return(output_highchart)
      
    }
    
  }



all_contacts_per_region_table <- 
  function(contacts_df_long, todays_date, report_format = "shiny"){
    
    # filter out dates that are past the input days date
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(follow_up_date <= todays_date)
    
    data_to_plot <- 
      contacts_df_long %>%
      group_by(row_id) %>% 
      # slice long frame
      slice_head() %>% 
      ungroup() %>% 
      select(region, district) %>%
      count(region, district) %>%
      group_by(region, district) %>%
      summarise(n = sum(n)) %>%
      ungroup() %>% 
      mutate(percent = round(100 * n/sum(n), 2)) %>% 
      group_by(region) %>%
      mutate(region_sum = sum(n)) %>% 
      ungroup() %>% 
      mutate(region_percent = region_sum/sum(n)) %>% 
      ungroup() %>% 
      arrange(-region_percent, -n) %>% 
      select(Region = region, 
             `District` = district, 
             `Total contacts` = n,
             `%` = percent)
    

    
    
    if (report_format %in% c("shiny","html", "pdf")){
    
      output_table <-  
        data_to_plot %>%
        reactable(columns = list(`Total contacts` = colDef(cell = data_bars_gradient(data_to_plot, 
                                                                                   colors = c(peach, bright_yellow_crayola),
                                                                                   background = "transparent"),
                                                         style = list(fontFamily = "Courier", whiteSpace = "pre", fontSize = 13)), 
                                 Region = colDef(style = JS("function(rowInfo, colInfo, state) {
                                                          var firstSorted = state.sorted[0]
                                                          // Merge cells if unsorted or sorting by region
                                                          if (!firstSorted || firstSorted.id === 'Region') {
                                                          var prevRow = state.pageRows[rowInfo.viewIndex - 1]
                                                          if (prevRow && rowInfo.row['Region'] === prevRow['Region']) {
                                                          return { visibility: 'hidden' }
                                                          }
                                                          }}"))),
                striped = TRUE,
                highlight = TRUE,
                theme = reactableTheme(stripedColor = "#f0f1fc70",
                                       backgroundColor = "#FFFFFF00",
                                       highlightColor = "#DADEFB"),
                defaultPageSize = 15)
    }
    
      
    if (report_format %in% c("pptx","docx", "pdf")){

      output_table <- 
        data_to_plot %>% 
        janitor::adorn_totals() %>% 
        huxtable() %>% 
        set_all_padding(0.5) %>%
        merge_repeated_rows(col = "Region") %>% 
        theme_blue()
      
      }
      
      return(output_table)
      
    
  }



all_contacts_per_region_text <- 
  function(contacts_df_long, todays_date, report_format = "shiny"){
  
    # filter out dates that are past the input days date
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(follow_up_date <= todays_date)
    
    
    data_to_plot <- 
      contacts_df_long %>%
      group_by(row_id) %>% 
      # slice long frame
      slice_head() %>% 
      ungroup() %>% 
      select(region, district) %>%
      count(region, district) %>%
      group_by(region, district) %>%
      summarise(n = sum(n)) %>%
      ungroup() %>% 
      mutate(percent = round(100 * n/sum(n), 2)) %>% 
      group_by(region) %>%
      mutate(region_sum = sum(n)) %>% 
      ungroup() %>% 
      mutate(region_percent = region_sum/sum(n)) %>% 
      ungroup() %>% 
      select(region, 
             region_sum,
             region_percent,
             district, 
             district_sum = n,
             district_percent = percent)
    
    region_w_most_contacts <- 
      data_to_plot %>% 
      arrange(-region_percent) %>% 
      .$region %>% .[1]
    
    n_contacts_in_region_w_most_contacts <- 
      data_to_plot %>% 
      arrange(-region_percent) %>% 
      .$region_sum %>% .[1]
    
    pct_contacts_in_region_w_most_contacts <- 
      data_to_plot %>% 
      arrange(-region_percent) %>% 
      .$region_percent %>% .[1] %>% 
      magrittr::multiply_by(100) %>% 
      round(1) %>% 
      paste0(., "%")
    
    
    str1 <- glue("<br>The region with the most total contacts since database inception is <b> {region_w_most_contacts}</b>, 
                 with <b>{n_contacts_in_region_w_most_contacts}</b> contacts (<b>{pct_contacts_in_region_w_most_contacts}</b> of the total)" )
    
    info <- c("<br>
            <span style='color: rgb(97, 189, 109);'>ℹ:</span>
            <font size='1'>The table and plots show the count of all contacts recorded in each region since database inception. </font>")
      
    output_text <- HTML(paste(info, str1, sep = '<br/>'))
    
    
    if (report_format %in% c("pptx", "docx", "pdf")) {
      output_text %>%
        charToRaw() %>%
        read_html() %>%
        html_text2() %>%
        str_trim() %>%
        pander::pandoc.p()
      # no need to return anything. pandoc.p prints automatically
    }
    
    if (report_format %in% c("shiny", "html")) {
      return(output_text)
    }
  
  
}

# ~~~ app_tab_row_2 ----


contacts_under_surveillance_per_region_over_time_bar_chart <- 
  function(contacts_df_long, todays_date, report_format = "shiny"){
    
    # filter out dates that are past the input days date
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(follow_up_date <= todays_date)
    
    data_to_plot <- 
      contacts_df_long %>%
      select(region, follow_up_date) %>%
      group_by(follow_up_date, region) %>% 
      count(region) %>% 
      ungroup() %>% 
      arrange(region, follow_up_date) %>% 
      bind_rows(tibble(follow_up_date =  seq.Date(from = min(.$follow_up_date), 
                                                  to = max(.$follow_up_date), 
                                                  by = "day"), 
                       region = "temporary"
      )) %>% 
      complete(follow_up_date, region, fill = list(n = 0)) %>% 
      filter(region != "temporary") %>% 
      ungroup() %>% 
      # region with the most cases should be at the top
      group_by(region) %>% 
      mutate(total = sum(n)) %>% 
      ungroup() %>% 
      mutate(region = fct_rev(fct_reorder(region, total)))
    
    if (nrow(data_to_plot) == 0) {
      return(highchart() %>% hc_title(text  = "No data to plot"))
    }
    
    output_highchart <- 
      data_to_plot %>% 
      hchart("column", hcaes(x = follow_up_date, y = n, group = region)) %>% 
      hc_yAxis(visible = TRUE) %>% 
      hc_plotOptions(column = list(stacking = "normal", 
                                   pointPadding = 0, 
                                   groupPadding = 0, 
                                   borderWidth= 0.05,
                                   stickyTracking = T
      )) %>% 
      hc_plotOptions(column = list(states = list(inactive = list(opacity = 0.7)))) %>% 
      hc_xAxis(title = list(text = "Date")) %>% 
      hc_yAxis(title = list(text = "No of contacts that were under surveillance")) %>% 
      hc_exporting(enabled = TRUE)
    
    
    if (report_format %in% c("pptx", "docx", "pdf")){
      # if report is one of the static rmarkdown output formats (not shiny or html)
      # then remove the animation so that the screenshot taken is of a fully loaded plot
      # and remove exporting as that is useless
      output_highchart <-
        output_highchart %>% 
        hc_exporting(enabled = FALSE) %>%
        hc_plotOptions(series = list(animation = FALSE)) %>% 
        html_webshot()
      # no need to return anything. html_webshot prints automatically
    }
    
    if (report_format %in% c("html", "shiny")){
      
      return(output_highchart)
      
    }

  }




contacts_under_surveillance_per_region_over_time_bar_chart_relative <- 
  function(contacts_df_long, todays_date, report_format = "shiny"){
    
    # filter out dates that are past the input days date
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(follow_up_date <= todays_date )
    
    data_to_plot <- 
      contacts_df_long %>%
      select(region, follow_up_date) %>%
      group_by(follow_up_date, region) %>% 
      count(region) %>% 
      group_by(follow_up_date) %>% 
      mutate(prop = n/sum(n)) %>% 
      mutate(prop = round(prop, digits = 4)) %>% 
      ungroup() %>% 
      arrange(region, follow_up_date) %>% 
      bind_rows(tibble(follow_up_date =  seq.Date(from = min(.$follow_up_date), 
                                                  to = max(.$follow_up_date), 
                                                  by = "day"), 
                       region = "temporary"
      )) %>% 
      complete(follow_up_date, region, fill = list(n = 0, prop = 0)) %>% 
      filter(region != "temporary") %>% 
      ungroup() %>% 
      # region with the most cases should be at the top
      group_by(region) %>% 
      mutate(total = sum(n)) %>% 
      ungroup() %>% 
      mutate(region = fct_rev(fct_reorder(region, total)))
    
    if (nrow(data_to_plot) == 0) {
      return(highchart() %>% hc_title(text  = "No data to plot"))
    }
    
    output_highchart <- 
      data_to_plot %>% 
      hchart("column", hcaes(x = follow_up_date, y = prop, group = region)) %>% 
      hc_yAxis(visible = TRUE) %>% 
      hc_plotOptions(column = list(stacking = "normal", 
                                   pointPadding = 0, 
                                   groupPadding = 0, 
                                   borderWidth= 0.05,
                                   stickyTracking = T
      )) %>% 
      hc_plotOptions(column = list(states = list(inactive = list(opacity = 0.7)))) %>% 
      hc_xAxis(title = list(text = "Date")) %>% 
      hc_yAxis(title = list(text = "% of all contacts that were under surveillance")) %>% 
      hc_exporting(enabled = TRUE)
    
    
    if (report_format %in% c("pptx", "docx", "pdf")){
      # if report is one of the static rmarkdown output formats (not shiny or html)
      # then remove the animation so that the screenshot taken is of a fully loaded plot
      # and remove exporting as that is useless
      output_highchart <-
        output_highchart %>% 
        hc_exporting(enabled = FALSE) %>%
        hc_plotOptions(series = list(animation = FALSE)) %>% 
        html_webshot()
      # no need to return anything. html_webshot prints automatically
    }
    
    if (report_format %in% c("html", "shiny")){
      
      return(output_highchart)
      
    }
    
  }



contacts_under_surveillance_per_region_over_time_text <- 
  function(contacts_df_long, todays_date, report_format = "shiny"){
    
    # filter out dates that are past the input days date
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(follow_up_date <= todays_date)
    
    
    data_to_plot <- 
      contacts_df_long %>%
      select(region, follow_up_date) %>%
      group_by(follow_up_date, region) %>% 
      count(region) %>% 
      ungroup() %>% 
      arrange(region, follow_up_date) %>% 
      bind_rows(tibble(follow_up_date =  seq.Date(from = min(.$follow_up_date), 
                                                  to = max(.$follow_up_date), 
                                                  by = "day"), 
                       region = "temporary"
      )) %>% 
      complete(follow_up_date, region, fill = list(n = 0)) %>% 
      filter(region != "temporary") %>% 
      ungroup() %>% 
      # region with the most cases should be at the top
      group_by(region) %>% 
      mutate(total = sum(n)) %>% 
      ungroup() %>% 
      mutate(region = fct_rev(fct_reorder(region, total)))
    
    
    max_n_under_surveillance <- 
      data_to_plot %>% 
      group_by(follow_up_date) %>% 
      mutate(total_that_day = sum(n)) %>% 
      slice_head() %>% 
      ungroup() %>% 
      arrange(-total_that_day) %>% 
      .$total_that_day %>% 
      .[1]
    
    date_of_max_n_under_surveillance <- 
      data_to_plot %>% 
      group_by(follow_up_date) %>% 
      mutate(total_that_day = sum(n)) %>% 
      slice_head() %>% 
      ungroup() %>% 
      arrange(-total_that_day) %>% 
      .$follow_up_date %>% 
      .[1]  %>% 
      format.Date("%b %d, %Y")
      
    
    
    info <- c("<br>
            <span style='color: rgb(97, 189, 109);'>ℹ:</span>
            <font size='1'>
            The plots show the number of contacts under surveillance on each day, whether or not they were successfully contacted.
            </font>")
    str1 <- glue("<br>The day on which the highest number contacts were under surveillance was <b>{date_of_max_n_under_surveillance}</b>, 
                 with <b>{max_n_under_surveillance}</b> contacts under surveillance." )

    output_text <- HTML(paste(info, str1, sep = '<br/>'))
    
    if (report_format %in% c("pptx","docx", "pdf")){
      
      output_text <- 
        output_text %>% 
        charToRaw() %>% 
        read_html() %>% 
        html_text2() %>% 
        str_trim() %>% 
        pander::pandoc.p()
      # no need to return anything. pandoc.p prints automatically
    }
    
    if (report_format %in% c("shiny", "html")) {
      return(output_text)
    }
    
    
  }




# ~~~ app_tab_row_3 ----


total_contacts_per_case_donut_plot <- 
  function(contacts_df_long, todays_date, report_format = "shiny"){
    
    # filter out dates that are past the input days date
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(follow_up_date <= todays_date)
    
    data_to_plot <- 
      contacts_df_long %>%
      group_by(row_id) %>% 
      # slice long frame
      slice_head() %>% 
      # count cases per id
      ungroup() %>% 
      select(id) %>% 
      mutate(id = fct_lump_n(id, 10, ties.method = "random")) %>% 
      count(id) %>% 
      arrange(-n) %>% 
      arrange(id == "Other") %>% 
      rename(`Case ID` = id,
             `Total linked contacts` = n)
    
    number_of_cases <- nrow(data_to_plot) - 1
    
    if (nrow(data_to_plot) == 0) {
      return(highchart() %>% hc_title(text  = "No data to plot"))
    }
    
    output_highchart <- 
      data_to_plot %>%
      hchart("pie", hcaes(name = `Case ID` , y = `Total linked contacts`),
             name = "n ",
             innerSize = "40%",
             showInLegend = TRUE,
             dataLabels = list(enabled = TRUE,
                               style = list(fontSize = 12),
                               format = '{point.percentage:.1f} %')) %>% 
      hc_exporting(enabled = TRUE) %>% 
      hc_subtitle(text = glue("Contacts per case for the <b>{number_of_cases}</b> 
                              cases with the most contacts"), 
                  useHTML = TRUE)
    
    
    if (report_format %in% c("pptx", "docx", "pdf")){
      # if report is one of the static rmarkdown output formats (not shiny or html)
      # then remove the animation so that the screenshot taken is of a fully loaded plot
      # and remove exporting as that is useless
      output_highchart <-
        output_highchart %>% 
        hc_exporting(enabled = FALSE) %>%
        hc_plotOptions(series = list(animation = FALSE)) %>% 
        html_webshot()
      # no need to return anything. html_webshot prints automatically
    }
    
    if (report_format %in% c("html", "shiny")){
      
      return(output_highchart)
      
    }
    
  }


total_contacts_per_case_bar_chart <- 
  function(contacts_df_long, todays_date, report_format = "shiny"){
    
    # filter out dates that are past the input days date
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(follow_up_date <= todays_date)
    
    data_to_plot <- 
      contacts_df_long %>%
      group_by(row_id) %>% 
      # slice long frame
      slice_head() %>% 
      # count cases per id
      ungroup() %>% 
      select(id) %>% 
      mutate(id = fct_lump_n(id, 10, ties.method = "random")) %>% 
      count(id) %>% 
      mutate(pct = round(100 * n/sum(n))) %>% 
      mutate(hc_label = glue("{n}<br>({pct}%)")) %>% 
      arrange(-n) %>% 
      arrange(id == "Other") %>% 
      rename(`Case ID` = id,
             `Total linked contacts` = n)
    
    color_df <- 
      tibble(`Case ID` = unique(data_to_plot$`Case ID`)) %>% 
      add_column(color_lvl_1 = highcharter_palette[1:nrow(.)])
    
    number_of_cases <- nrow(data_to_plot) - 1
    
    if (nrow(data_to_plot) == 0) {
      return(highchart() %>% hc_title(text  = "No data to plot"))
    }
    
    
    output_highchart <- 
      data_to_plot %>%
      left_join(color_df) %>% 
      hchart("column", hcaes(name = `Case ID` ,
                             y = `Total linked contacts`, 
                             color = color_lvl_1),
             name = "n ",
             showInLegend = TRUE,
             dataLabels = list(enabled = TRUE,
                               style = list(fontSize = 12),
                               format = '{point.hc_label}')) %>% 
      hc_exporting(enabled = TRUE) %>% 
      hc_xAxis(categories = data_to_plot$`Case ID`) %>% 
      hc_subtitle(text = glue("Contacts per case for the <b>{number_of_cases}</b> 
                              cases with the most contacts"), 
                  useHTML = TRUE) %>% 
      hc_legend(enabled = FALSE)
    
    
    
    if (report_format %in% c("pptx", "docx", "pdf")){
      # if report is one of the static rmarkdown output formats (not shiny or html)
      # then remove the animation so that the screenshot taken is of a fully loaded plot
      # and remove exporting as that is useless
      output_highchart <-
        output_highchart %>% 
        hc_exporting(enabled = FALSE) %>%
        hc_plotOptions(series = list(animation = FALSE)) %>% 
        html_webshot()
      # no need to return anything. html_webshot prints automatically
    }
    
    if (report_format %in% c("html", "shiny")){
      
      return(output_highchart)
      
    }
    
  }




# total_contacts_per_case_table <- 
#   function(contacts_df_long, todays_date){
#     
#     # filter out dates that are past the input days date
#     contacts_df_long <- 
#       contacts_df_long %>% 
#       filter(follow_up_date <= todays_date)
#     
#     
#     data_to_plot <- 
#       contacts_df_long %>%
#       group_by(row_id) %>% 
#       # slice long frame
#       slice_head() %>% 
#       # count cases per id
#       group_by(id) %>% 
#       count() %>% 
#       arrange(-n) %>% 
#       select(`Case ID` = id, 
#              `Total linked contacts` = n
#       ) %>% 
#       ungroup()
#     
#     
# 
#     data_to_plot %>%
#       reactable(columns = list(`Total linked contacts` = colDef(cell = data_bars_gradient(data_to_plot, 
#                                                                                               colors = c(peach, bright_yellow_crayola),
#                                                                                               background = "transparent"),
#                                                                     style = list(fontFamily = "Courier", whiteSpace = "pre", fontSize = 13))), 
#                 defaultPageSize = 15, 
#                 striped = TRUE,
#                 highlight = TRUE,
#                 theme = reactableTheme(stripedColor = "#f0f1fc",
#                                        highlightColor = "#DADEFB"))
#     
#     
#     
#   }
  


total_contacts_per_case_text <- 
  function(contacts_df_long, todays_date, report_format = "shiny"){
    
    # filter out dates that are past the input days date
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(follow_up_date <= todays_date)
    
    data_to_plot <- 
      contacts_df_long %>%
      group_by(row_id) %>% 
      # slice long frame
      slice_head() %>% 
      # count cases per id
      group_by(id) %>% 
      count() %>% 
      arrange(-n) %>% 
      select(`Case ID` = id, 
             `Total linked contacts` = n
      ) %>% 
      ungroup()
    
    mean_number_of_contacts_per_case <- 
      data_to_plot$`Total linked contacts` %>% 
      mean() %>% 
      round(1)
    
    sd_number_of_contacts_per_case <- 
      data_to_plot$`Total linked contacts` %>% 
      sd() %>% 
      round(1)
    
    min_number_of_contacts_per_case <- 
      data_to_plot$`Total linked contacts` %>% 
      min()
    
    max_number_of_contacts_per_case <- 
      data_to_plot$`Total linked contacts` %>% 
      max()
    
    
    
    info <- c("<br>
            <span style='color: rgb(97, 189, 109);'>ℹ:</span>
            <font size='1'>
            The plots show the number of contacts linked to each case.
            </font>")

    str1 <- glue("<br>The <b>mean</b> number of contacts per case is <b>{mean_number_of_contacts_per_case}</b>, (<b>SD:{sd_number_of_contacts_per_case}</b>) 
                 with a <b>minimum</b> of <b>{min_number_of_contacts_per_case}</b> and a maximum of <b>{max_number_of_contacts_per_case}</b>" )

    output_text <- HTML(paste(info, str1, sep = '<br/>'))
    
    
    if (report_format %in% c("pptx","docx", "pdf")){
      
      output_text <- 
        output_text %>% 
        charToRaw() %>% 
        read_html() %>% 
        html_text2() %>% 
        str_trim() %>% 
        pander::pandoc.p()
      # no need to return anything. pandoc.p prints automatically
    }
    
    if (report_format %in% c("shiny", "html")) {
      return(output_text)
    }
    
  }


# ~~~ app_tab_row_4 ----


total_contacts_per_link_type_donut_plot <- 
  function(contacts_df_long, todays_date, report_format = "shiny"){
    
    # filter out dates that are past the input days date
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(follow_up_date <= todays_date)
    
    data_to_plot <- 
      contacts_df_long %>%
      group_by(row_id) %>% 
      # slice long frame
      slice_head() %>% 
      ungroup() %>% 
      # count cases per id
      group_by(lien_avec_le_cas) %>% 
      count() %>% 
      arrange(-n) %>% 
      select(`Link with the case` = lien_avec_le_cas, 
             `Number of contacts` = n)
    
    if (nrow(data_to_plot) == 0) {
      return(highchart() %>% hc_title(text  = "No data to plot"))
    }

    output_highchart <- 
      data_to_plot %>%
      hchart("pie", hcaes(name = `Link with the case`, y = `Number of contacts` ),
             innerSize = "40%",
             name = "n",
             showInLegend = TRUE,
             dataLabels = list(enabled = TRUE,
                               style = list(fontSize = 12),
                               format = '{point.name}: {point.y}, ({point.percentage:.1f} %)'))  %>%
      hc_exporting(enabled = TRUE)
      
      
      
      if (report_format %in% c("pptx", "docx", "pdf")){
        # if report is one of the static rmarkdown output formats (not shiny or html)
        # then remove the animation so that the screenshot taken is of a fully loaded plot
        # and remove exporting as that is useless
        output_highchart <-
          output_highchart %>% 
          hc_exporting(enabled = FALSE) %>%
          hc_plotOptions(series = list(animation = FALSE)) %>% 
          html_webshot()
        # no need to return anything. html_webshot prints automatically
      }
    
    if (report_format %in% c("html", "shiny")){
      
      return(output_highchart)
      
    }
    
  }


total_contacts_per_link_type_bar_chart <- 
  function(contacts_df_long, todays_date, report_format = "shiny"){
    
    # filter out dates that are past the input days date
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(follow_up_date <= todays_date)
    
    data_to_plot <- 
      contacts_df_long %>%
      group_by(row_id) %>% 
      # slice long frame
      slice_head() %>% 
      ungroup() %>% 
      # count cases per id
      group_by(lien_avec_le_cas) %>% 
      count() %>% 
      ungroup() %>% 
      mutate(pct = round(100 * n/sum(n))) %>% 
      mutate(hc_label = glue("{n}<br>({pct}%)")) %>% 
      arrange(-n) %>% 
      select(`Link with the case` = lien_avec_le_cas, 
             `Number of contacts` = n, 
             hc_label)
    
    color_df <- 
      tibble(`Link with the case` = unique(data_to_plot$`Link with the case`)) %>% 
      add_column(color_lvl_1 = highcharter_palette[1:nrow(.)])
    
    if (nrow(data_to_plot) == 0) {
      return(highchart() %>% hc_title(text  = "No data to plot"))
    }
    
    
    output_highchart <- 
      data_to_plot %>%
      left_join(color_df) %>% 
      hchart("column", hcaes(name = `Link with the case` ,
                             y = `Number of contacts`, 
                             color = color_lvl_1),
             name = "n ",
             showInLegend = TRUE,
             dataLabels = list(enabled = TRUE,
                               style = list(fontSize = 12),
                               format = '{point.hc_label}')) %>% 
      hc_exporting(enabled = TRUE) %>% 
      hc_xAxis(categories = data_to_plot$`Link with the case`) %>% 
      hc_legend(enabled = FALSE)
    
    if (report_format %in% c("pptx", "docx", "pdf")){
      # if report is one of the static rmarkdown output formats (not shiny or html)
      # then remove the animation so that the screenshot taken is of a fully loaded plot
      # and remove exporting as that is useless
      output_highchart <-
        output_highchart %>% 
        hc_exporting(enabled = FALSE) %>%
        hc_plotOptions(series = list(animation = FALSE)) %>% 
        html_webshot()
      # no need to return anything. html_webshot prints automatically
    }
    
    if (report_format %in% c("html", "shiny")){
      
      return(output_highchart)
      
    }
    
  }






total_contacts_per_link_type_text <-
  function(contacts_df_long, todays_date, report_format = "shiny"){
    
    # filter out dates that are past the input days date
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(follow_up_date <= todays_date)
    
    data_to_plot <-
      contacts_df_long %>%
      group_by(row_id) %>%
      # slice long frame
      slice_head() %>%
      ungroup() %>%
      # count cases per id
      group_by(lien_avec_le_cas) %>%
      count() %>%
      ungroup() %>%
      arrange(-n) %>%
      mutate(Percent = percent(n / sum(n))) %>%
      select(`Link with the case` = lien_avec_le_cas,
             `Number of contacts` = n,
             Percent)
    
    link_type_with_most_contacts <-
      data_to_plot %>%
      .$`Link with the case` %>%
      .[1]
    
    number_of_contacts_link_type_with_most_contacts <-
      data_to_plot %>%
      .$`Number of contacts` %>%
      .[1]
    
    percent_link_type_with_most_contacts <-
      data_to_plot %>%
      .$Percent %>%
      .[1]
    
  
    info <- c("<br>
            <span style='color: rgb(97, 189, 109);'>ℹ:</span>
            <font size='1'>
            The plots show the number of contacts per type of link. The categories have been cleaned and condensed.
            Access the data in tabular form by clicking on the top-right button.
            </font>")
    
    str1 <-
      glue(
        "<br>The most common link category is <b>'{link_type_with_most_contacts}'</b>,
                 with <b>{number_of_contacts_link_type_with_most_contacts}</b> contacts (<b>{percent_link_type_with_most_contacts}</b>)."
      )
    
    output_text <- HTML(paste(info, str1, sep = '<br/>'))
    
    if (report_format %in% c("pptx","docx", "pdf")){
      
      output_text <- 
        output_text %>% 
        charToRaw() %>% 
        read_html() %>% 
        html_text2() %>% 
        str_trim() %>% 
        pander::pandoc.p()
      # no need to return anything. pandoc.p prints automatically
    }
    
    if (report_format %in% c("shiny", "html")) {
      return(output_text)
    }
    
    
  }



# ~~~ app_tab_row_6 ----


active_contacts_timeline_snake_plot <- 
  function(contacts_df_long, todays_date, report_format = "shiny"){
  
    
    active_contacts <-
      contacts_df_long %>%
      group_by(row_id) %>% 
      ## keep active cases. 
      ## active cases are those whose last day of follow-up is today or any day past today
      filter(max(follow_up_date, na.rm = T) >= todays_date) %>%
      ## AND whose first day of follow_up is today or anyday before today. BUT this is actually redundant since read_file already removes all IDs for which follow-up had not begun by date of review
      # filter(min(follow_up_date, na.rm = T) <= todays_date) %>%
      ungroup()
    
    # to be fed to plotter
    colors <- 
      active_contacts %>% 
      select(etat_suivi, colors) %>% 
      unique.data.frame()
    
    data_to_plot <-
      active_contacts %>%
      group_by(row_id) %>% 
      mutate(hc_ttip = glue("<b>ID: </b> {id_contact} <br>
                         <b>Date: </b> {format.Date(follow_up_date, format = '%b %d')} (Jour de suivi {follow_up_day}) <br>
                         <b>Status: </b> {etat_suivi} <br>
                         ")) %>% 
      ungroup() %>% 
      arrange(etat_suivi) %>%   # arranging is necessary so that that colors are pulled in the right order for highcharter
      mutate(follow_up_date_timestamp = datetime_to_timestamp(follow_up_date)) %>% 
      select(id_contact, row_id, follow_up_day, follow_up_date,  etat_suivi, row_id, hc_ttip, colors)
    
    if (nrow(data_to_plot) == 0) {
      return(e_charts() %>% e_title(text  = "No data to plot"))
    }
    
    output_echart <- 
      data_to_plot %>% 
      group_by(etat_suivi) %>%
      e_charts(follow_up_date, height = "550px") %>%
      e_scatter(row_id, bind = hc_ttip, symbol_size = 5,  itemStyle = list(opacity = 0.9)) %>%
      e_tooltip(trigger = "item") %>% 
      e_tooltip(formatter = htmlwidgets::JS("
                                        function(params){
                                        return(params.name) }")) %>% 
      e_toolbox_feature("dataZoom", title = list(zoom = "zoom", back = "back")) %>%
      e_toolbox_feature("saveAsImage", type = "png", pixelRatio = 4) %>%
      e_color(unique(data_to_plot$colors)) %>% 
      e_legend(show = FALSE) %>% 
      e_axis_labels(x = "Date", y = "Row ID") 
    
    
    if (report_format %in% c("pptx", "docx", "pdf")){
      # if report is one of the static rmarkdown output formats (not shiny or html)
      # then remove the animation so that the screenshot taken is of a fully loaded plot
      # and remove exporting as that is useless
      output_echart <-
        data_to_plot %>% 
        group_by(etat_suivi) %>%
        e_charts(follow_up_date, height = "550px") %>%
        e_scatter(row_id, bind = hc_ttip, symbol_size = 5,  itemStyle = list(opacity = 0.9)) %>%
        e_tooltip(trigger = "item") %>% 
        e_tooltip(formatter = htmlwidgets::JS("
                                        function(params){
                                        return(params.name) }")) %>% 
        e_color(unique(data_to_plot$colors)) %>% 
        e_legend(show = TRUE) %>% ### this is the main difference. But apparently I can't overwrite an old function
        e_axis_labels(x = "Date", y = "Row ID") %>% 
        html_webshot()
      # no need to return anything. html_webshot prints automatically
    }
    
    if (report_format %in% c("html", "shiny")){
      
      return(output_echart)
      
    }
  }

active_contacts_timeline_table <- 
  function(contacts_df_long, todays_date, download = FALSE){
    
    
    
    data_to_plot <-
      contacts_df_long %>%
      group_by(row_id) %>% 
      ## keep active cases. 
      ## active cases are those whose last day of follow-up is today or any day past today
      filter(max(follow_up_date, na.rm = T) >= todays_date) %>%
      ## AND whose first day of follow_up is today or anyday before today. BUT this is actually redundant since read_file already removes all IDs for which follow-up had not begun by date of review
      # filter(min(follow_up_date, na.rm = T) <= todays_date) %>%
      ungroup() %>% 
      select(ID = id_contact, 
             `Follow-up day` = follow_up_day, 
             `Follow-up date` = follow_up_date,
             `Follow-up status` = etat_suivi)
    
    if (download == TRUE) {
      return(data_to_plot)
      } else {
        
        data_to_plot %>% 
          reactable(searchable = TRUE,
                    striped = TRUE,
                    highlight = TRUE,
                    theme = reactableTheme(stripedColor = "#f0f1fc70",
                                           backgroundColor = "#FFFFFF00",
                                           highlightColor = "#DADEFB"),
                    defaultPageSize = 10)
        
      }
      
  }
    
active_contacts_timeline_table_download <- 
  function(contacts_df_long, todays_date){
    downloadHandler(
      filename = function() "follow_up_timelines.csv",
      
      content = function(file){
        file_to_write <- active_contacts_timeline_table(contacts_df_long, 
                                                        todays_date, 
                                                        download = TRUE)
        write.csv(file_to_write, file)}
      
    )
    
  }

active_contacts_breakdown_bar_chart <- 
  function(contacts_df_long, todays_date, report_format = "shiny"){
    
    
    active_contacts <-
      contacts_df_long %>%
      group_by(row_id) %>% 
      ## keep active cases. 
      ## active cases are those whose last day of follow-up is today or any day past today
      filter(max(follow_up_date, na.rm = T) >= todays_date) %>%
      ## AND whose first day of follow_up is today or anyday before today. BUT this is actually redundant since read_file already removes all IDs for which follow-up had not begun by date of review
      # filter(min(follow_up_date, na.rm = T) <= todays_date) %>%
      ungroup()
 
    # to be fed to plotter
    colors <- 
      active_contacts %>% 
      select(etat_suivi, colors) %>% 
      unique.data.frame()
    
    data_to_plot <-
      active_contacts %>%
      group_by(follow_up_date) %>% 
      count(etat_suivi) %>% 
      complete(follow_up_date, etat_suivi, fill = list(n = 0)) %>% 
      mutate(total = sum(n)) %>% 
      mutate(prop = n/total) %>% 
      mutate(hc_ttip = glue("<b>Date:</b> {format.Date(follow_up_date,  format = '%b %d' )}
                        <b>{etat_suivi}:</b> {n}
                        ")) %>% 
      ungroup() %>% 
      arrange(etat_suivi) %>%   # arranging is necessary so that that colors are pulled in the right order for highcharter
      left_join(colors)
    
    if (nrow(data_to_plot) == 0) {
      return(e_charts() %>% e_title(text  = "No active contacts to show"))
    }
    
    output_echart <- 
      data_to_plot %>% 
      group_by(etat_suivi) %>%
      e_charts(follow_up_date, height = "300px") %>%
      e_bar(n, bind = hc_ttip, stack = "grp") %>%
      e_tooltip(formatter = htmlwidgets::JS("
                                        function(params){
                                        return(params.name) }")) %>% 
      e_color(unique(data_to_plot$colors)) %>% 
      e_legend(y = "top") %>% 
      e_axis_labels(x = "Date", y = "n") %>% 
      e_y_axis(max = max(data_to_plot$total))
    
    
    if (report_format %in% c("pptx", "docx", "pdf")){
      # if report is one of the static rmarkdown output formats (not shiny or html)
      # then remove the animation so that the screenshot taken is of a fully loaded plot
      # and remove exporting as that is useless
      output_echart <-
        output_echart %>% 
        ## e_toolbox(show = FALSE) %>% ## this line breaks it for some silly reason
        e_animation(show = FALSE) %>% 
        html_webshot()
      # no need to return anything. html_webshot prints automatically
    }
    
    if (report_format %in% c("html", "shiny")){
      
      return(output_echart)
      
    }

    
  }


active_contacts_breakdown_table <- 
  function(contacts_df_long, todays_date, download = FALSE){
    
    
    active_contacts <-
      contacts_df_long %>%
      group_by(row_id) %>% 
      ## keep active cases. 
      ## active cases whose last day of follow-up is today any day past today
      filter(max(follow_up_date) >= todays_date) %>%
      ## AND whose first day of follow_up is today or anyday before today
      filter(min(follow_up_date, na.rm = T) <= todays_date) %>%
      ungroup()
    
    colors <- 
      active_contacts %>% 
      select(etat_suivi, colors) %>% 
      unique.data.frame()
    
    
    data_to_plot <-
      active_contacts %>%
      group_by(follow_up_date) %>% 
      count(etat_suivi) %>% 
      ungroup() %>% 
      complete(follow_up_date, etat_suivi, fill = list(n = 0)) %>% 
      # replace 0s with NA for Upcoming follow ups before today's date
      # and replace 0s with NA for past follow ups after today's date
      mutate(n = ifelse(etat_suivi == "Suivi futur" & 
                          follow_up_date <= todays_date, 
                        NA_integer_, 
                        n)) %>% 
      mutate(prop = n/sum(n)) %>% 
      mutate(hc_ttip = glue("<b>Date:</b> {format.Date(follow_up_date,  format = '%b %d')}
                        <b>{etat_suivi}:</b> {n}
                        ")) %>% 
      arrange(etat_suivi) %>%   # arranging is necessary so that that colors are pulled in the right order for highcharter
      left_join(colors) %>% 
      select(1:3) %>% 
      pivot_wider(names_from = etat_suivi, values_from = n) %>% 
      select(Date = follow_up_date, everything())
    
    if (download == TRUE){
      return(data_to_plot)
    } else {
      
      data_to_plot %>% 
        reactable(searchable = TRUE,
                  striped = TRUE,
                  highlight = TRUE,
                  theme = reactableTheme(stripedColor = "#f0f1fc70",
                                         backgroundColor = "#FFFFFF00",
                                         highlightColor = "#DADEFB"),
                  defaultPageSize = 10)
    }
    
    
  }

active_contacts_breakdown_table_download <- 
  function(contacts_df_long, todays_date){
    downloadHandler(
      filename = function() paste("follow_up_summary.csv"),
      
      content = function(file){
        file_to_write <- active_contacts_breakdown_table(contacts_df_long, 
                                                         todays_date, 
                                                         download = TRUE)
        write.csv(file_to_write, file)}
      
    )
    
  }


active_contacts_timeline_text <- 
  function(contacts_df_long, todays_date, report_format = "shiny"){
  
  
  n_active_contacts <-
    contacts_df_long %>%
    group_by(row_id) %>% 
    ## keep active cases. 
    ## active cases whose last day of follow-up is today or any day past today
    filter(max(follow_up_date) >= todays_date) %>%
    ## AND whose first day of follow_up is today or anyday before today
    filter(min(follow_up_date, na.rm = T) <= todays_date) %>%
    ungroup() %>% 
    .$row_id %>%
    unique() %>%
    length()
  
  
  info <- c("<br>
            <span style='color: rgb(97, 189, 109);'>ℹ:</span>
            <font size='1'>
            The plots track the status of each active contact over all 10 days of follow-up.
            </font>")
  
  str1 <- glue("<br>There are <b>{n_active_contacts}</b> active contacts")
  
  output_text <- HTML(paste(info, str1, sep = '<br/>'))
  
  if (report_format %in% c("pptx","docx", "pdf")){
    
    output_text <- 
      output_text %>% 
      charToRaw() %>% 
      read_html() %>% 
      html_text2() %>% 
      str_trim() %>% 
      pander::pandoc.p()
    # no need to return anything. pandoc.p prints automatically
  }
  
  if (report_format %in% c("shiny", "html")) {
    return(output_text)
  }
  
  
}

  




# ~~~ app_tab_row_7 ----


contacts_lost_24_to_72_hours_table <- 
  function(contacts_df_long, todays_date, report_format = "shiny", download = FALSE){
    
    contacts_df_long_filtered <- 
      contacts_df_long %>% 
      group_by(row_id) %>% 
      ## keep active cases. 
      ## active cases are those whose last day of follow-up is today or any day past today
      filter(max(follow_up_date, na.rm = T) >= todays_date) %>%
      ## AND whose first day of follow_up is today or anyday before today. BUT this is actually redundant since read_file already removes all IDs for which follow-up had not begun by date of review
      # filter(min(follow_up_date, na.rm = T) <= todays_date) %>%
      ungroup() %>% 
      filter(etat_suivi_simple != "Suivi futur")
      
    
    active_contacts_count <- 
      contacts_df_long %>%
       filter(follow_up_date == todays_date) %>% 
       nrow()
    
    
    ## if there is no data, early return with empty table
    if(active_contacts_count== 0){
      
      if (download == TRUE ) {
        return(
          "No data to show" %>% 
            data.frame()
        )
      }
      
      if (report_format %in% c("shiny", "html")){
        return(
          "No data to show" %>% 
            data.frame() %>% 
            reactable())}
      
      if (report_format %in% c("pdf", "pptx", "docx")){
        return(
          "No data to show" %>% 
            data.frame() %>% 
            huxtable() %>% 
            theme_blue())}
    }
    
    vu_non_vu_today <-
      contacts_df_long_filtered %>%
      filter(follow_up_date == todays_date) %>% 
      group_by(district) %>% 
      count(etat_suivi_simple) %>% 
      ungroup() %>% 
      bind_rows(data.frame(etat_suivi_simple = c("Vu", "Non vu"), 
                           district = "!!+temporary",
                           n = 0)) %>% 
      complete(district, etat_suivi_simple, 
               fill = list(n = 0)) %>% 
      filter(district != "!!+temporary") %>% 
      pivot_wider(names_from = etat_suivi_simple, values_from = n) %>% 
      clean_names() %>% 
      mutate(total = non_vu + vu) %>% 
      mutate(pct_non_vu = percent(non_vu/ total, accuracy = 0.1)) %>% 
      select(`District` = district, 
             `No under surveillance` = total, 
             `Not seen today` = non_vu, 
             `% Not seen` = pct_non_vu) %>% 
      rename_with(.cols = c(`No under surveillance`, `Not seen today`, `% Not seen`), 
                  .fn = ~ paste0("Today.", .x))
      
      
    vu_non_vu_past_two_days <- 
      contacts_df_long_filtered %>%
      filter(follow_up_date >= todays_date - 1) %>% 
      mutate(counter = 1) %>% 
      group_by(id_contact) %>% 
      mutate(number_of_days = sum(counter)) %>% 
      ungroup() %>% 
      filter(number_of_days >= 2) %>% 
      group_by(id_contact) %>% 
      mutate(etat_suivi_simple = ifelse( all(etat_suivi_simple == "Non vu"),
                                          "Non vu",
                                          "Vu")) %>% 
      slice_head(n = 1) %>% 
      ungroup() %>% 
      group_by(district) %>% 
      count(etat_suivi_simple) %>% 
      ungroup() %>% 
      bind_rows(data.frame(etat_suivi_simple = c("Vu", "Non vu"), 
                           district = "!!+temporary",
                           n = 0)) %>% 
      complete(district, etat_suivi_simple, 
               fill = list(n = 0)) %>% 
      filter(district != "!!+temporary") %>% 
      pivot_wider(names_from = etat_suivi_simple, values_from = n) %>% 
      clean_names() %>% 
      mutate(total = non_vu + vu) %>% 
      mutate(pct_non_vu = percent(non_vu/ total, accuracy = 0.1)) %>% 
      select(`District` = district, 
             `No under surveillance` = total, 
             `Not seen past 2d` = non_vu, 
             `% Not seen` = pct_non_vu) %>% 
      rename_with(.cols = c(`No under surveillance`, `Not seen past 2d`, `% Not seen`),
                  .fn = ~ paste0("Past 2d.", .x))
    
    
    
    vu_non_vu_past_three_days <- 
      contacts_df_long_filtered %>%
      filter(follow_up_date >= todays_date - 2) %>% 
      mutate(counter = 1) %>% 
      group_by(id_contact) %>% 
      mutate(number_of_days = sum(counter)) %>% 
      ungroup() %>% 
      filter(number_of_days >= 3) %>% 
      group_by(id_contact) %>% 
      mutate(etat_suivi_simple = ifelse( all(etat_suivi_simple == "Non vu"),
                                         "Non vu",
                                         "Vu")) %>% 
      slice_head(n = 1) %>% 
      ungroup() %>% 
      group_by(district) %>% 
      count(etat_suivi_simple) %>% 
      ungroup() %>% 
      bind_rows(data.frame(etat_suivi_simple = c("Vu", "Non vu"), 
                           district = "!!+temporary",
                           n = 0)) %>% 
      complete(district, etat_suivi_simple, 
               fill = list(n = 0)) %>% 
      filter(district != "!!+temporary") %>% 
      pivot_wider(names_from = etat_suivi_simple, values_from = n) %>% 
      clean_names() %>% 
      mutate(total = non_vu + vu) %>% 
      mutate(pct_non_vu = percent(non_vu/ total, accuracy = 0.1)) %>% 
      select(`District` = district, 
             `No under surveillance` = total, 
             `Not seen past 3d` = non_vu, 
             `% Not seen` = pct_non_vu) %>% 
      rename_with(.cols = c(`No under surveillance`, `Not seen past 3d`, `% Not seen`),
                  .fn = ~ paste0("Past 3d.", .x))
    
    
    ## Number of days covered.
    ## If there has only been one active date, we do not want the table to show any inactives for the past 2 days, or 3 days etc)
     number_of_days_covered <- 
       contacts_df_long_filtered %>%
       filter(etat_suivi != "Suivi futur") %>% 
       count(follow_up_date) %>% 
       nrow()
    
    
    # only show "not seen in past day" if there was only one day. etc.
    data_to_plot <- 
      vu_non_vu_today %>% 
      {if (number_of_days_covered > 1){
        left_join(., vu_non_vu_past_two_days)}
        else {.}
         } %>% 
      {if (number_of_days_covered > 2){
        left_join(., vu_non_vu_past_three_days)}
        else {.}
      }
    
    
    ## if data is to for download, return at this stage
    if (download == TRUE ) {
      return(data_to_plot)
    }
    

    
    if( nrow(data_to_plot) == 0){
      no_data_message <- 
        "No data to show" %>% 
        data.frame() %>% 
        gt() %>% 
        fmt_markdown(columns = 1) %>%
        tab_options(column_labels.hidden = T)
      
      return_html_or_webshot(no_data_message, report_format)
    }
    
    
    output_table <- 
      data_to_plot %>% 
      gt() %>% 
      fmt_missing(columns = everything(), missing_text = "-") %>% 
      {if (number_of_days_covered > 2){
        data_color(., columns = "Past 3d.Not seen past 3d", 
                 colors = scales::col_numeric(palette = paletteer_d(palette = "ggsci::red_material") %>% as.character(), 
                                              domain = c(1, (max(vu_non_vu_past_three_days$`Past 3d.Not seen past 3d`) + 1) * 4)
                                              ),
                 autocolor_text = F )} 
        else {.} } %>% 
      tab_spanner_delim(delim = ".") %>%
      tab_style(locations = cells_column_labels(columns = everything()),
                style = list(cell_borders(sides = "bottom", weight = px(3)),
                     cell_text(weight = "bold", size = "small"))) %>%
      tab_style(locations = cells_column_spanners(spanners = everything()),
                style = list(cell_text(weight = "bold", size = "large"))) %>%
      opt_row_striping() %>% 
      {if (number_of_days_covered > 2){
      tab_source_note(., source_note = md("*Not seen in past 2/3 days means not seen on **any** of the past 2/3 days"))
      } else {.} }
    
    
    
    ## for static outputs, use huxtable
    if (report_format %in% c("pptx", "docx", "pdf")){
        gt_webshot(output_table) 
    } else {
      return(output_table)
    }
    
  }


contacts_lost_24_to_72_hours_table_download <- 
  function(contacts_df_long, todays_date){
    downloadHandler(
      filename = function() paste("contacts_lost_summary.csv"),
      
      content = function(file){
        file_to_write <- contacts_lost_24_to_72_hours_table(contacts_df_long, 
                                                            todays_date, 
                                                            download = TRUE)
        write.csv(file_to_write, file)}
      
    )
    
  }



lost_contacts_linelist_table <- 
  function(contacts_df_long, todays_date, download = FALSE, report_format = "shiny"){
    
    contacts_df_long_filtered <- 
      contacts_df_long %>% 
      group_by(row_id) %>% 
      ## keep active cases. 
      filter(max(follow_up_date, na.rm = T) >= todays_date) %>%
      ungroup() %>% 
      filter(etat_suivi_simple != "Suivi futur")
    
    ## first isolate those who were missing in past three days
    vu_non_vu_past_three_days <- 
      contacts_df_long_filtered %>%
      filter(follow_up_date >= todays_date - 2) %>% 
      mutate(counter = 1) %>% 
      group_by(id_contact) %>% 
      mutate(number_of_days = sum(counter)) %>% 
      ungroup() %>% 
      filter(number_of_days >= 3) %>% 
      group_by(id_contact) %>% 
      mutate(etat_suivi_simple = ifelse( all(etat_suivi_simple == "Non vu"),
                                         "Non vu",
                                         "Vu")) %>% 
      filter(etat_suivi_simple == "Non vu")
    
    
    ## if there is no data, early return with empty table
    if(nrow(vu_non_vu_past_three_days) == 0){
           
           if (download == TRUE ) {
             return(
               "No data to show" %>% 
                 data.frame()
             )
           }
           
           if (report_format %in% c("shiny", "html")){
           return(
             "No data to show" %>% 
               data.frame() %>% 
               reactable())}
           
           if (report_format %in% c("pdf", "pptx", "docx")){
             return(
               "No data to show" %>% 
                 data.frame() %>% 
                 huxtable() %>% 
                 theme_blue())}
    }
    
    
    ## filter out lost-to-follow-up individuals from the regular data frame
    data_to_plot <- 
      contacts_df_long %>%
      filter(id_contact %in% vu_non_vu_past_three_days$id_contact) %>% 
      select(ID = id_contact, 
             `District` = district, 
             Dates = follow_up_date, 
             `Follow-up Day` = follow_up_day,
             `Follow-up States` = etat_suivi) %>% 
      group_by(ID) %>% 
      arrange(Dates) %>% 
      ungroup()  %>% 
      arrange(order(mixedorder(ID)))
    
    ## if there is no data, early return with empty table
    if(nrow(data_to_plot) == 0){
      
      if (download == TRUE ) {
        return(
          "No data to show" %>% 
            data.frame()
        )
      }
      
      if (report_format %in% c("shiny", "html")){
        return(
          "No data to show" %>% 
            data.frame() %>% 
            reactable())}
      
      if (report_format %in% c("pdf", "pptx", "docx")){
        return(
          "No data to show" %>% 
            data.frame() %>% 
            huxtable() %>% 
            theme_blue())}
    }
    
    
    ## if data is to for download, return at this stage
    if (download == TRUE ) {
        return(data_to_plot)
      }
    

    ## create output reacttable
    output_table <- data_to_plot %>% 
      reactable(groupBy = "ID", 
                columns = list(`Follow-up States` = colDef(aggregate = "frequency"), 
                               `District` = colDef(aggregate = "unique")
                               ),
                searchable = TRUE,
                striped = TRUE,
                highlight = TRUE,
                theme = reactableTheme(stripedColor = "#f0f1fc70",
                                       backgroundColor = "#FFFFFF00",
                                       highlightColor = "#DADEFB")
      ) 

        
    ## title depending on number of days
    number_of_days_covered <- 
      contacts_df_long_filtered %>%
      filter(etat_suivi != "Suivi futur") %>% 
      count(follow_up_date) %>% 
      nrow()
    
    table_title_init <- "Contacts not seen in the past day"
    if (number_of_days_covered > 1) table_title_init <- "Contacts not seen in the past 2 days"
    if (number_of_days_covered > 2) table_title_init <- "Contacts not seen in the past 3 days"
    
    table_title <- h4(table_title_init)
    
    ## for static outputs, use huxtable
    if (report_format %in% c("pptx", "docx", "pdf")){
      
      output_table <-
        data_to_plot %>% 
        select(ID) %>% 
        unique.data.frame() %>% 
        split_long_df(15) %>% 
        huxtable() %>% 
        set_all_padding(0.5) %>%
        theme_blue()

    }
    
    return(list(table_title = table_title, 
                output_table = output_table
                ))
    
  }


lost_contacts_linelist_table_download <- 
  function(contacts_df_long, todays_date){
    downloadHandler(
      filename = function() paste("lost_contacts_linelist.csv"),
      
      content = function(file){
        file_to_write <- lost_contacts_linelist_table(contacts_df_long, 
                                                      todays_date, 
                                                      download = TRUE)
        write.csv(file_to_write, file)}
      
    )
    
  }


lost_contacts_linelist_text <- 
  function(contacts_df_long, todays_date, report_format = "shiny"){
    
    lost_contacts_table <- 
      lost_contacts_linelist_table(contacts_df_long, 
                                 todays_date, 
                                 download = TRUE)
    
    ### if there is no data,
    if (lost_contacts_table[1,1] == "No data to show"){
      
      info <- HTML("<br>
         <span style='color: rgb(97, 189, 109);'>ℹ:</span>
         <font size='1'>
         The tables track the contacts who should be under surveillance but have not been followed for an extended period.
                    </font>")
      
      str1 <- glue("<br><br>
                   No lost to follow-up (LTFU) contacts to show. 
                   (An LTFU contact is a contact that has been seen for more than two days.)")
      
      output_text <- HTML(paste(info, str1, sep = '<br/>'))
      
      if (report_format %in% c("pptx","docx", "pdf")){
        
        output_text <- 
          output_text %>% 
          charToRaw() %>% 
          read_html() %>% 
          html_text2() %>% 
          str_trim() %>% 
          pander::pandoc.p()
        # no need to return anything. pandoc.p prints automatically
        return(pander::pandoc.p(" "))
      }
      
      if (report_format %in% c("shiny", "html")) {
        return(output_text)
      }
      
      
    }
    
    ## if there is data, do it all over again, but with actual numbers
    
    number_of_contacts_lost <-  
    lost_contacts_table %>% 
      pull(ID) %>% 
      unique() %>% 
      length()
    
    info <- HTML("<br>
         <span style='color: rgb(97, 189, 109);'>ℹ:</span>
         <font size='1'>
         The tables track the contacts who should be under surveillance but have not been followed for an extended period.
                    </font>")
    
    str1 <- glue("<br><br>
                 <b>{number_of_contacts_lost}</b> contacts have not been seen for the past three days.")
    
    output_text <- HTML(paste(info, str1, sep = '<br/>'))
    
    if (report_format %in% c("pptx","docx", "pdf")){
      
      output_text <- 
        output_text %>% 
        charToRaw() %>% 
        read_html() %>% 
        html_text2() %>% 
        str_trim() %>% 
        pander::pandoc.p()
      # no need to return anything. pandoc.p prints automatically
      
      
    }
    
    if (report_format %in% c("shiny", "html")) {
      return(output_text)
      }
    
    }


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~  app_tab_regional ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# ~~~ app_tab_row_1_regional ----




all_contacts_per_region_bar_chart <- 
  function(contacts_df_long, todays_date, report_format = "shiny"){
    
    
    contact_regions <-
      contacts_df_long %>%
      # filter out dates that are past the input days date
      filter(follow_up_date <= todays_date) %>% 
      group_by(row_id) %>% 
      # slice long frame
      slice_head() %>% 
      ungroup() %>%
      select(region, district) %>%
      add_count(region, name = "n_region") %>%
      mutate(pct_region = round(100 * n_region / nrow(.), 
                                digits = 2)) %>%
      group_by(region) %>%
      mutate(district = fct_lump(other_level = "Autres", 
                                 district, prop = 0.01)) %>%
      add_count(district, name = "n_district") %>%
      mutate(pct_district = round(100 * n_district / nrow(.), 
                                  digits = 2)) %>%
      mutate(data_label = paste0(n_district, " (", pct_district, "%", ")")) %>%
      group_by(region, district) %>%
      slice_head(n = 1) %>%
      ungroup() %>%
      arrange(-n_region, -n_district)
    
    
    color_df <- 
      data.frame(region = unique(contact_regions$region)) %>% 
      add_column(color_lvl_1 = highcharter_palette[1:nrow(.)])
    
    
    if (nrow(contact_regions) == 0) {
      return(highchart() %>% hc_title(text  = "No data to plot"))
    }
    
    
    subtitle <-  
      contact_regions %>% 
      left_join(color_df) %>% 
      select(region, pct_region, color_lvl_1) %>% 
      unique.data.frame() %>% 
      mutate(sep = case_when(row_number() == (n() - 1) ~ " and ",
                             row_number() == n() ~ "",
                             TRUE ~ ",")
      ) %>% 
      mutate(txt = stringr::str_glue("<strong><span style='background-color: {color_lvl_1};color:white'>
                                      &nbsp;{region}&nbsp;</span></strong> ({pct_region}% of contacts){sep}")) %>% 
      summarise(paste0(txt, collapse = "")) %>% 
      pull() %>% 
      stringr::str_c("Regions shown: ", .)
    
    
    
    output_highchart <- 
      contact_regions %>% 
      left_join(color_df) %>% 
      hchart("bar", hcaes(x = district , y = n_district, color = color_lvl_1),
             size = 4,
             name = "n",
             dataLabels = list(enabled = TRUE,
                               formatter = JS("function(){return(this.point.data_label)}"))) %>%
      hc_legend(enabled = TRUE) %>% 
      hc_plotOptions(series = list(groupPadding = 0)) %>% 
      hc_subtitle(text = subtitle, useHTML = TRUE) %>% 
      hc_xAxis(title = list(text = "District")) %>% 
      hc_yAxis(title = list(text = "Number of contacts"))
    
    
    
    if (report_format %in% c("pptx", "docx", "pdf")){
      # if report is one of the static rmarkdown output formats (not shiny or html)
      # then remove the animation so that the screenshot taken is of a fully loaded plot
      # and remove exporting as that is useless
      output_highchart <-
        output_highchart %>% 
        hc_exporting(enabled = FALSE) %>%
        hc_plotOptions(series = list(animation = FALSE)) %>% 
        html_webshot()
      # no need to return anything. html_webshot prints automatically
    }
    
    if (report_format %in% c("html", "shiny")){
      
      return(output_highchart)
      
    }
    
    
  }



all_contacts_per_region_table_regional <- 
  function(contacts_df_long, todays_date){
    

    # filter out dates that are past the input days date
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(follow_up_date <= todays_date)
    
    data_to_plot <- 
      contacts_df_long %>%
      group_by(row_id) %>% 
      # slice long frame
      slice_head() %>% 
      ungroup() %>% 
      count(district, name = "district_sum") %>%
      mutate(district  = replace_na(district, "Manquant")) %>% 
      mutate(percent = round(100 * district_sum/sum(district_sum), 2)) %>% 
      arrange(-district_sum) %>% 
      select(`District` = district, 
             `Total contacts` = district_sum,
             `%` = percent)
    

    
    data_to_plot %>%
      reactable(columns = list(`Total contacts` = colDef(cell = data_bars_gradient(data_to_plot, 
                                                                                   colors = c(peach, bright_yellow_crayola),
                                                                                   background = "transparent"),
                                                         style = list(fontFamily = "Courier", whiteSpace = "pre", fontSize = 13))),
                striped = TRUE,
                highlight = TRUE,
                theme = reactableTheme(stripedColor = "#f0f1fc",
                                       highlightColor = "#DADEFB"),
                defaultPageSize = 15)
    
  }




all_contacts_per_region_sunburst_plot_regional <- 
  function(contacts_df_long, todays_date){
    

    # filter out dates that are past the input days date
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(follow_up_date <= todays_date)
    
    
    data_to_plot <- 
      contacts_df_long %>%
      group_by(row_id) %>% 
      # slice long frame
      slice_head() %>% 
      ungroup() %>% 
      count(district, name = "district_sum") %>%
      mutate(district  = replace_na(district, "Manquant")) %>% 
      mutate(percent = round(100 * district_sum/sum(district_sum), 2)) %>% 
      arrange(-district_sum) %>% 
      select(`District` = district, 
             `Total contacts` = district_sum,
             `%` = percent)
    

    
    if (nrow(data_to_plot) == 0) {
      return(highchart() %>% hc_title(text  = "No data to plot"))
    }
    
    data_to_plot %>%
      hchart("pie", hcaes(name = `District`, y = `Total contacts` ),
             innerSize = "40%",
             name = "n",
             showInLegend = TRUE,
             dataLabels = list(enabled = TRUE,
                               style = list(fontSize = 12),
                               format = '{point.name}: {point.y}, ({point.percentage:.1f} %)'))  %>%
      hc_exporting(enabled = TRUE)
    
    
    
  }




all_contacts_per_region_text_regional <- 
  function(contacts_df_long, todays_date){
    

    # filter out dates that are past the input days date
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(follow_up_date <= todays_date )
    
    data_to_plot <- 
      contacts_df_long %>%
      group_by(row_id) %>% 
      # slice long frame
      slice_head() %>% 
      ungroup() %>% 
      count(district, name = "district_sum") %>%
      mutate(district  = replace_na(district, "Manquant")) %>% 
      mutate(percent = round(100 * district_sum/sum(district_sum), 2)) %>% 
      arrange(-district_sum) 
    
    
    region_w_most_contacts <- 
      data_to_plot %>% 
      arrange(-percent) %>% 
      .$district %>% .[1]
    
    n_contacts_in_region_w_most_contacts <- 
      data_to_plot %>% 
      arrange(-percent) %>% 
      .$district_sum %>% .[1]
    
    pct_contacts_in_region_w_most_contacts <- 
      data_to_plot %>% 
      arrange(-percent) %>% 
      .$percent %>% .[1] %>% 
      round(1) %>% 
      paste0(., "%")
    
    
    str1 <- glue("<br>The district with the most total contacts since database inception is <b> {region_w_most_contacts}</b>, 
                 with <b>{n_contacts_in_region_w_most_contacts}</b> contacts (<b>{pct_contacts_in_region_w_most_contacts}</b> of the total)" )
    
    info <- c("<br>
            <span style='color: rgb(97, 189, 109);'>ℹ:</span>
            <font size='1'>The table and plot show the count of all contacts seen in each district, since the beginning of the epidemic. </font>")
    
    HTML(paste(info, str1, sep = '<br/>'))
    
  }




# ~~~ app_tab_row_2_regional ----


contacts_under_surveillance_per_region_over_time_bar_chart_regional <- 
  function(contacts_df_long, todays_date){
    
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(region == input$select_region)
    
    
    # filter out dates that are past the input days date
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(follow_up_date <= todays_date)
    
    
    if (nrow(contacts_df_long) == 0) {
      return(highchart() %>% hc_title(text  = "No data to plot"))
    }
    
    data_to_plot <- 
      contacts_df_long %>%
      select(district, follow_up_date) %>%
      group_by(follow_up_date, district) %>% 
      count(district) %>% 
      ungroup() %>% 
      arrange(district, follow_up_date) %>% 
      bind_rows(tibble(follow_up_date =  seq.Date(from = min(.$follow_up_date), 
                                                  to = max(.$follow_up_date), 
                                                  by = "day"), 
                       district = "temporary"
      )) %>% 
      complete(follow_up_date, district, fill = list(n = 0)) %>% 
      filter(district != "temporary") %>% 
      ungroup() %>% 
      # region with the most cases should be at the top
      group_by(district) %>% 
      mutate(total = sum(n)) %>% 
      ungroup() %>% 
      mutate(district = fct_rev(fct_reorder(district, total)))
    
    
    if (nrow(data_to_plot) == 0) {
      return(highchart() %>% hc_title(text  = "No data to plot"))
    }
    
    data_to_plot %>% 
      hchart("column", hcaes(x = follow_up_date, y = n, group = district)) %>% 
      hc_yAxis(visible = TRUE) %>% 
      hc_plotOptions(column = list(stacking = "normal", 
                                   pointPadding = 0, 
                                   groupPadding = 0, 
                                   borderWidth= 0.05,
                                   stickyTracking = T
      )) %>% 
      hc_plotOptions(column = list(states = list(inactive = list(opacity = 0.7)))) %>% 
      hc_xAxis(title = list(text = "Date")) %>% 
      hc_yAxis(title = list(text = "No of contacts that were under surveillance")) %>% 
      hc_exporting(enabled = TRUE)
    
    
  }




contacts_under_surveillance_per_region_over_time_bar_chart_relative_regional <- 
  function(contacts_df_long, todays_date){
    
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(region == input$select_region) 
    
    
    # filter out dates that are past the input days date
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(follow_up_date <= todays_date)
    
    if (nrow(contacts_df_long) == 0) {
      return(highchart() %>% hc_title(text  = "No data to plot"))
    }
    
    data_to_plot <- 
      contacts_df_long %>%
      select(district, follow_up_date) %>%
      group_by(follow_up_date, district) %>% 
      count(district) %>% 
      group_by(follow_up_date) %>% 
      mutate(prop = n/sum(n)) %>% 
      mutate(prop = round(prop, digits = 4)) %>% 
      ungroup() %>% 
      arrange(district, follow_up_date) %>% 
      bind_rows(tibble(follow_up_date =  seq.Date(from = min(.$follow_up_date), 
                                                  to = max(.$follow_up_date), 
                                                  by = "day"), 
                       district = "temporary"
      )) %>% 
      complete(follow_up_date, district, fill = list(n = 0, prop = 0)) %>% 
      filter(district != "temporary") %>% 
      ungroup() %>% 
      # region with the most cases should be at the top
      group_by(district) %>% 
      mutate(total = sum(n)) %>% 
      ungroup() %>% 
      mutate(district = fct_rev(fct_reorder(district, total)))
    
    
    if (nrow(data_to_plot) == 0) {
      return(highchart() %>% hc_title(text  = "No data to plot"))
    }
    
    data_to_plot %>% 
      hchart("column", hcaes(x = follow_up_date, y = prop, group = district)) %>% 
      hc_yAxis(visible = TRUE) %>% 
      hc_plotOptions(column = list(stacking = "normal", 
                                   pointPadding = 0, 
                                   groupPadding = 0, 
                                   borderWidth= 0.05,
                                   stickyTracking = T
      )) %>% 
      hc_plotOptions(column = list(states = list(inactive = list(opacity = 0.7)))) %>% 
      hc_xAxis(title = list(text = "Date")) %>% 
      hc_yAxis(title = list(text = "% of all contacts that were under surveillance")) %>% 
      hc_exporting(enabled = TRUE)
    
    
    
  }



contacts_under_surveillance_per_region_over_time_text_regional <- 
  function(contacts_df_long, todays_date){
    
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(region == input$select_region)
    
    # filter out dates that are past the input days date
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(follow_up_date <= todays_date)
    
    
    if (nrow(contacts_df_long) == 0) {
      return(HTML("No data"))
    }
    
    
    data_to_plot <- 
      contacts_df_long %>%
      select(district, follow_up_date) %>%
      group_by(follow_up_date, district) %>% 
      count(district) %>% 
      ungroup() %>% 
      arrange(district, follow_up_date) %>% 
      bind_rows(tibble(follow_up_date =  seq.Date(from = min(.$follow_up_date), 
                                                  to = max(.$follow_up_date), 
                                                  by = "day"), 
                       district = "temporary"
      )) %>% 
      complete(follow_up_date, district, fill = list(n = 0)) %>% 
      filter(district != "temporary") %>% 
      ungroup() %>% 
      # region with the most cases should be at the top
      group_by(district) %>% 
      mutate(total = sum(n)) %>% 
      ungroup() %>% 
      mutate(district = fct_rev(fct_reorder(district, total)))
    
    
    max_n_under_surveillance <- 
      data_to_plot %>% 
      group_by(follow_up_date) %>% 
      mutate(total_that_day = sum(n)) %>% 
      slice_head() %>% 
      ungroup() %>% 
      arrange(-total_that_day) %>% 
      .$total_that_day %>% 
      .[1]
    
    date_of_max_n_under_surveillance <- 
      data_to_plot %>% 
      group_by(follow_up_date) %>% 
      mutate(total_that_day = sum(n)) %>% 
      slice_head() %>% 
      ungroup() %>% 
      arrange(-total_that_day) %>% 
      .$follow_up_date %>% 
      .[1]  %>% 
      format.Date("%b %d, %Y")
    
    
    
    info <- c("<br>
            <span style='color: rgb(97, 189, 109);'>ℹ:</span>
            <font size='1'>
            The plots show the number of contacts under surveillance on each day, whether or not they were successfully contacted.
            Access the data in tabular form by clicking on top-right button.
            </font>")
    str1 <- glue("<br>The day on which the highest number contacts in {input$select_region} was under surveillance was <b>{date_of_max_n_under_surveillance}</b>, 
                 with <b>{max_n_under_surveillance}</b> contacts under surveillance." )
    
    HTML(paste(info, str1, sep = '<br/>'))
    
    
  }



# ~~~ app_tab_row_3_regional - EMPTY ----
## uses the same functions as the national data. Data is subsetted to the region before sending into the function


# ~~~ app_tab_row_4_regional - EMPTY ----
## uses the same functions as the national data. Data is subsetted to the region before sending into the function


# ~~~ app_tab_row_6_regional - EMPTY ----
## uses the same functions as the national data. Data is subsetted to the region before sending into the function


# ~~~ app_tab_row_7_regional - EMPTY ----
## uses the same functions as the national data. Data is subsetted to the region before sending into the function




















