

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   load_data_tab ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



todays_date_reactive <- reactive({
  input$select_date_of_review
})

# ~~~~ read_file ----
read_file_raw <- reactive({
  
  req(input$data_to_use_id)
  
  if (input$data_to_use_id == "Use preloaded data") {
    contacts_df_raw <- preloaded_data_options[[input$preloaded_data]]
    
  } else if (input$data_to_use_id == "Use uploaded data") {
    contacts_df_raw <-
      rio::import(input$uploaded_data$datapath)  %>%
      clean_names() %>%
      type_convert()
    
  } 
  
  contacts_df_raw <- 
    contacts_df_raw %>% 
    mutate(prefecture = str_to_sentence(prefecture))
  
  return(contacts_df_raw)
})



read_file_unfiltered <- reactive({
  
  req(input$data_to_use_id)
  
  
  contacts_df_raw <- read_file_raw() 
  # data in
  contacts_df_unfiltered <-
    contacts_df_raw %>%
    # clean vaccination column, type_de_contact, unite_age, prefecture, lien_avec_las_cas. 
    # No etat_suivi column to clean yet because I have not created it
    {suppressWarnings(clean_variable_spelling(., wordlists = cleaning_rules)) }%>%
    # add counter column. 1 for all records
    mutate(counter = 1) %>%
    # row numbers to match Excel spreadsheet
    mutate(row_id = row_number() + 3) %>%
    # convert dates to dates
    mutate(across(.cols = matches("date_"),
                  .fns = ~ as.Date(.x))) %>%
    # rename the follow_up_day columns
    rename_with(.cols = paste0("j", 1:21),
                .fn = ~ paste("follow_up_day", str_remove_all(.x, "j"), sep = "_")) %>%
    # clean vaccination column
    mutate(across(vaccination,
                  ~ .x %>%
                    stri_trans_general("Latin-ASCII") %>%
                    str_to_lower() %>%
                    str_to_sentence())) %>%
    # clean admin levels
    mutate(across(c(prefecture, sous_prefecture, quartier),  # EDIT 2021-03-04 I changed it.  don't change prefecture spelling as we used the raw spellings to populate the dropdown select on the regional tab
                  ~ .x %>%
                    str_to_sentence() %>% 
                    str_trim() %>% 
                    str_replace_all("  ", " ")))
  
  
  pivot_day <- 
    contacts_df_unfiltered %>% 
    select(row_id, paste0("follow_up_day_", 1:21)) %>% 
    pivot_longer(cols = c(paste0("follow_up_day_", 1:21)), 
                 names_to = "follow_up_day", 
                 values_to = "etat_suivi") %>% 
    # clean "etat de suivi" column
    {suppressWarnings(clean_variable_spelling(., wordlists = cleaning_rules)) }
    
  
  
  contacts_df_long_unfiltered <- 
    contacts_df_unfiltered %>%
    inner_join(pivot_day, by = "row_id") %>%
    # paste the day of followup
    mutate(follow_up_day = str_extract(follow_up_day, paste(21:1, collapse = "|"))) %>%
    mutate(follow_up_day = as.numeric(follow_up_day)) %>%
    # assume that follow up begins from date of last interaction
    mutate(follow_up_date = follow_up_day + date_du_dernier_contact) %>%
    mutate(follow_up_date_1 = if_else(follow_up_day == 1, follow_up_date, NA_Date_)) %>% 
    mutate(follow_up_date_21 = if_else(follow_up_day == 21, follow_up_date, NA_Date_)) %>% 
    mutate(etat_suivi = as.character(etat_suivi)) %>%
    mutate(etat_suivi = replace_na(etat_suivi, "Données manquantes")) %>% 
    # - if follow-up lasted the full 21 days,
    # - change last follow_up state to "Fin de suivi"
    mutate(etat_suivi = if_else(follow_up_day == 21 & etat_suivi == "Vu",
                                "Fin de suivi",
                                etat_suivi)) %>% 
    mutate(etat_suivi_simple = recode(etat_suivi,
                                      "Vu" = "Vu",
                                      "Non vu" = "Non vu",
                                      "Cas suspect" = "Vu",
                                      "Decedé" = "Vu",
                                      "Deplacé" = "Non vu",
                                      "Cas confirmé" = "Vu",
                                      "Transferé" = "Vu",
                                      "Refus" = "Non vu",
                                      "Reco. pas passé" = "Non vu",
                                      "Doublon" = "Doublon",
                                      "Recyclé" = "Vu", 
                                      "Fin de suivi" = "Vu", 
                                      "Données manquantes" = "Non vu"
    )) %>% 
    filter(etat_suivi != "Doublon")
  
  return(list(contacts_df_unfiltered = contacts_df_unfiltered, 
              contacts_df_long_unfiltered = contacts_df_long_unfiltered ))
  
})


read_file_unfiltered_regional <- reactive({
  ##  why do we need read_file_unfiltered_regional AND read_file_unfiltered?
  ##  Because need to pass the version of the data that is filtered on REGION to the date picker for the region tab
  ##  we want to only show dates that are relevant for each region
  
  req(input$contacts_tab_select_regional)
  
  
  contacts_df_unfiltered <- read_file_unfiltered()$contacts_df_unfiltered
  contacts_df_long_unfiltered <- read_file_unfiltered()$contacts_df_long_unfiltered
  
  
  contacts_df_unfiltered_regional <- 
    contacts_df_unfiltered %>% 
    filter(prefecture == input$contacts_tab_select_regional)
  ## still unfiltered in the sense that it is not filtered for date of review
  
    
  contacts_df_long_unfiltered_regional <- 
    contacts_df_long_unfiltered %>% 
    filter(prefecture == input$contacts_tab_select_regional)
  
  return(list(contacts_df_unfiltered_regional = contacts_df_unfiltered_regional, 
              contacts_df_long_unfiltered_regional = contacts_df_long_unfiltered_regional ))
    
  })

read_file <- reactive({
  
  req(input$data_to_use_id)
  
  
  contacts_df_long_unfiltered <- read_file_unfiltered()$contacts_df_long_unfiltered
  
  
  # for legend colors. We only add them in after filtering
  # because after the filter, we are able to add in "suivi futur"

  legend_df <- 
    tribble(    
      ~breaks ,                  ~colors,                   
      "Non vu",                  col2hex("orangered"),                  
      "Vu",                      col2hex("lightseagreen"),  
      "Cas confirmé",            col2hex("purple3"),     
      "Cas suspect",             col2hex("yellow"),       
      "Manquant",                col2hex("pink4"),          
      "Deplacé",                 col2hex("wheat3"),  
      "Recyclé",                 col2hex("yellow4"),          
      "Fin de suivi",            col2hex("dodgerblue3"),    
      "Suivi futur",             col2hex("goldenrod"),      
      "Données manquantes",      col2hex("black"),          
      "Doublon",                 col2hex("slategray3"),          
    ) %>% 
    arrange(breaks) %>% 
    mutate(breaks = fct_inorder(breaks)) %>% 
    mutate(legend_index = row_number())
  
  
  
  contacts_df_long <-
    contacts_df_long_unfiltered %>%
    group_by(row_id) %>%
    # keep only those for whom follow-up had begun by the date of review
    filter(min(follow_up_date) <= todays_date_reactive()) %>%
    ungroup() %>% 
    # change follow-up status for any day after the date of review to "suivi futur"
    mutate(across(c(etat_suivi, etat_suivi_simple),
                  .fns = ~ if_else(follow_up_date > todays_date_reactive(), 
                                   "Suivi futur",
                                   .x
                                   )
                  )) %>% 
  # add legend colors
  left_join(legend_df, by = c("etat_suivi" = "breaks"))
  
  
  return(list(contacts_df_long = contacts_df_long))
  
})





data_completeness_plot <- function(contacts_df){
  
  contacts_df %>% 
    mutate(data_completeness_plot_row_id = row_number()) %>%  # long name so it does not conflict
    {if (nrow(.) > 1000) sample_n(., 1000) else .} %>% # sample if too large
    select(-c(paste0("j", 1:21))) %>% 
    arrange(-data_completeness_plot_row_id) %>% 
    select(-data_completeness_plot_row_id) %>% 
    visdat::vis_dat(sort_type = FALSE) +
    scale_fill_paletteer_d(palette = "NineteenEightyR::sonny") +
    #my_theme+
    theme(axis.text.x = element_text(angle = 60, hjust = 0)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) + 
    labs(title = "Summary of uploaded dataframe", 
         subtitle = "Data types and missingness are shown.")+ 
    theme(title = element_text(face = "bold"))
  
}



data_cardinality_plot <- function(contacts_df){
  
  contacts_df %>% 
    {if (nrow(.) > 1000) sample_n(., 1000) else .} %>% # sample if too large
    select(-c(paste0("j", 1:21))) %>% 
    inspectdf::inspect_cat() %>% 
    show_plot(col_palette = 4)+ 
    labs(title = "Freq. of categorical vars in dataset")
  
}


reactable_table <- 
  function(contacts_df){
    
    contacts_df %>% 
      select(!matches("nom|Nom")) %>% # remove names
      select(-cas_index) %>%# remove names
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
# ~~  all_contacts_tab ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# ~~~ all_contacts_tab_row_0 ----

contacts_per_day_value_box <- 
  function(contacts_df_long, todays_date, region = NULL){
    
    ## regional subset
    if(!is.null(region)){
      contacts_df_long <- 
        contacts_df_long %>% 
        filter(prefecture == region) 
    }

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
      
      valueBoxSpark(
        value = HTML(glue("{cases_last_day} <font size='1'> in past day ({date_last_day}) </font>")),
        title = toupper(glue("New contacts")),
        sparkobj = highchart_to_plot,
        info = "Bars show the no. of new contacts per day (based on first date of follow-up)",
        subtitle = HTML(" <font size='1'> </font>"),
        #icon = icon("calendar-day"),
        width = 2,
        color = "aqua",
        href = NULL)
      
    
  }


cumulative_contacts_value_box <- 
  function(contacts_df_long, todays_date, region = NULL){
    
    ## regional subset
    if(!is.null(region)){
      contacts_df_long <- 
        contacts_df_long %>% 
        filter(prefecture == region) 
    }
    
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
    
    valueBoxSpark(
      value = HTML(glue("{cases_last_day} <font size='1'> by {date_last_day} </font>")),
      title = toupper(glue("Total contacts")),
      sparkobj = highchart_to_plot,
      info = "Bars show the cumulative contacts as at each day.",
      subtitle = HTML(" <font size='1'> </font>"),
      #icon = icon("calendar-alt"),
      width = 2,
      color = "teal",
      href = NULL)
    
  }


contacts_under_surveillance_value_box <- 
  function(contacts_df_long, todays_date, region = NULL){
    
    ## regional subset
    if(!is.null(region)){
      contacts_df_long <- 
        contacts_df_long %>% 
        filter(prefecture == region) 
    }
    
      
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
    
    
  }



pct_contacts_followed_value_box <- 
  function(contacts_df_long, todays_date, region = NULL){
    
    ## regional subset
    if(!is.null(region)){
      contacts_df_long <- 
        contacts_df_long %>% 
        filter(prefecture == region) 
    }
    

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
    
    valueBoxSpark(
      value = HTML(glue("{pct_last_day} <font size='1'> on {date_last_day} </font>")),
      title = toupper("% of cases followed"),
      sparkobj = highchart_to_plot,
      info = "Line plot shows the percentage followed on each day",
      subtitle = HTML(" <font size='1'> </font>"),
      #icon = icon("check-square"),
      width = 2,
      color = "yellow",
      href = NULL)
    
    
  }




# ~~~ all_contacts_tab_row_1 ----

all_contacts_per_region_table <- 
  function(contacts_df_long){
    
    # filter out dates that are past the input days date
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(follow_up_date <= todays_date_reactive())
    
    data_to_plot <- 
      contacts_df_long %>%
      group_by(row_id) %>% 
      # slice long frame
      slice_head() %>% 
      ungroup() %>% 
      select(prefecture, sous_prefecture) %>%
      count(prefecture, sous_prefecture) %>%
      group_by(prefecture, sous_prefecture) %>%
      summarise(n = sum(n)) %>%
      ungroup() %>% 
      mutate(percent = round(100 * n/sum(n), 2)) %>% 
      group_by(prefecture) %>%
      mutate(prefecture_sum = sum(n)) %>% 
      ungroup() %>% 
      mutate(prefecture_percent = prefecture_sum/sum(n)) %>% 
      ungroup() %>% 
      arrange(-prefecture_percent, -n) %>% 
      select(Prefecture = prefecture, 
             `Sous-prefecture` = sous_prefecture, 
             `Total contacts` = n,
             `%` = percent)
    

      data_to_plot %>%
        reactable(columns = list(`Total contacts` = colDef(cell = data_bars_gradient(data_to_plot, 
                                                                                   colors = c(peach, bright_yellow_crayola),
                                                                                   background = "transparent"),
                                                         style = list(fontFamily = "Courier", whiteSpace = "pre", fontSize = 13)), 
                               Prefecture = colDef(style = JS("function(rowInfo, colInfo, state) {
                                                          var firstSorted = state.sorted[0]
                                                          // Merge cells if unsorted or sorting by Prefecture
                                                          if (!firstSorted || firstSorted.id === 'Prefecture') {
                                                          var prevRow = state.pageRows[rowInfo.viewIndex - 1]
                                                          if (prevRow && rowInfo.row['Prefecture'] === prevRow['Prefecture']) {
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




all_contacts_per_region_sunburst_plot <- 
  function(contacts_df_long){
    
    # filter out dates that are past the input days date
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(follow_up_date <= todays_date_reactive())
    
    
    contact_regions <-
      contacts_df_long %>%
      group_by(row_id) %>% 
      # slice long frame
      slice_head() %>% 
      ungroup() %>%
      select(prefecture, sous_prefecture, quartier) %>%
      mutate(across(
        .fns =
          ~ .x %>%
          str_to_lower() %>%
          str_to_title() %>%
          replace_na("NA") %>%
          str_trim()
      )) %>%
      add_count(prefecture, name = "n_prefecture") %>%
      mutate(pct_prefecture = round(100 * n_prefecture / nrow(.), 
                                    digits = 2)) %>%
      mutate(prefecture = paste0(prefecture, 
                                 " (", pct_prefecture, "%", ")")) %>%
      group_by(prefecture) %>%
      mutate(sous_prefecture = fct_lump(other_level = "Autres", 
                                        sous_prefecture, prop = 0.01)) %>%
      add_count(sous_prefecture, name = "n_sous_prefecture") %>%
      mutate(pct_sous_prefecture = round(100 * n_sous_prefecture / nrow(.), 
                                         digits = 2)) %>%
      mutate(sous_prefecture = paste0(sous_prefecture, " (", pct_sous_prefecture, "%", ")")) %>%
      group_by(prefecture, sous_prefecture) %>%
      mutate(quartier = fct_lump(other_level = "Autres", quartier, prop = 0.02)) %>%
      add_count(quartier, name = "n_quartier") %>%
      mutate(pct_quartier = round(100 * n_quartier / nrow(.), digits = 2)) %>%
      mutate(quartier = paste0(quartier, " (", pct_quartier, "%", ")")) %>%
      group_by(prefecture, sous_prefecture, quartier) %>%
      slice_head(n = 1) %>%
      ungroup() %>%
      arrange(-n_prefecture)
    
    if (nrow(contact_regions) == 0) {
      return(highchart() %>% hc_title(text  = "No data to plot"))
    }
    
    color_df <- 
      data.frame(prefecture = unique(contact_regions$prefecture)) %>% 
      add_column(color_lvl_1 = final_palette[1:nrow(.)])
    
    contact_regions_list <- 
      contact_regions %>% 
      data_to_hierarchical(group_vars = c(prefecture, 
                                          sous_prefecture, 
                                          quartier
      ), 
      size_var = n_quartier, 
      colors= color_df$color_lvl_1)
    
    
    x <- c("Type: ", "n = ")
    y <- c("{point.name}", "{point.value}")
    tltip <- tooltip_table(x, y)
    
    
    highchart() %>% 
      hc_chart(type = "sunburst") %>% 
      #hc_subtitle(text = subtitle, useHTML = TRUE) %>% 
      hc_add_series(data = contact_regions_list,
                    allowDrillToNode = TRUE,
                    levelIsConstant = FALSE,
                    #textOverflow = "clip",
                    levels = list(list(level = 1,
                                       borderWidth = 1, 
                                       dataLabels = list(enabled = TRUE,
                                                         color = "#FFFFFF",
                                                         style = list(fontSize = "14px",
                                                                      #fontWeight = "bold",
                                                                      #textOutline = "white", 
                                                                      opacity = 0.8))), 
                                  list(level = 2, 
                                       borderWidth = 0,
                                       dataLabels = list(enabled = TRUE, 
                                                         color = "#FFFFFF",
                                                         style = list(fontSize = "12px", 
                                                                      textOutline = FALSE, 
                                                                      opacity = 0.8))), 
                                  list(level = 3,
                                       borderWidth = 0,
                                       dataLabels = list(enabled = TRUE, 
                                                         color = "#FFFFFF",
                                                         style = list(fontSize = "8px", 
                                                                      textOutline = FALSE, 
                                                                      opacity = 0.8))))) %>% 
      hc_plotOptions(sunburst = list(dataLabels = list(enabled = TRUE) )) %>% 
      hc_tooltip(useHTML = TRUE, 
                 headerFormat = "", pointFormat = tltip) %>% 
      hc_exporting(enabled = TRUE)
    
    
  }




all_contacts_per_region_text <- 
  function(contacts_df_long){
  
    # filter out dates that are past the input days date
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(follow_up_date <= todays_date_reactive())
    
    
    data_to_plot <- 
      contacts_df_long %>%
      group_by(row_id) %>% 
      # slice long frame
      slice_head() %>% 
      ungroup() %>% 
      select(prefecture, sous_prefecture) %>%
      count(prefecture, sous_prefecture) %>%
      group_by(prefecture, sous_prefecture) %>%
      summarise(n = sum(n)) %>%
      ungroup() %>% 
      mutate(percent = round(100 * n/sum(n), 2)) %>% 
      group_by(prefecture) %>%
      mutate(prefecture_sum = sum(n)) %>% 
      ungroup() %>% 
      mutate(prefecture_percent = prefecture_sum/sum(n)) %>% 
      ungroup() %>% 
      select(prefecture, 
             prefecture_sum,
             prefecture_percent,
             sous_prefecture, 
             sous_prefecture_sum = n,
             sous_prefecture_percent = percent)
    
    region_w_most_contacts <- 
      data_to_plot %>% 
      arrange(-prefecture_percent) %>% 
      .$prefecture %>% .[1]
    
    n_contacts_in_region_w_most_contacts <- 
      data_to_plot %>% 
      arrange(-prefecture_percent) %>% 
      .$prefecture_sum %>% .[1]
    
    pct_contacts_in_region_w_most_contacts <- 
      data_to_plot %>% 
      arrange(-prefecture_percent) %>% 
      .$prefecture_percent %>% .[1] %>% 
      magrittr::multiply_by(100) %>% 
      round(1) %>% 
      paste0(., "%")
    
    
    str1 <- glue("<br>The prefecture with the most total contacts since database inception is <b> {region_w_most_contacts}</b>, 
                 with <b>{n_contacts_in_region_w_most_contacts}</b> contacts (<b>{pct_contacts_in_region_w_most_contacts}</b> of the total)" )
    
    info <- "<br>
            <span style='color: rgb(97, 189, 109);'>ℹ:</span>
            <font size='1'>The table and plot show the count of all contacts seen in each prefecture, since the beginning of the epidemic. </font>"
      
    HTML(paste(info, str1, sep = '<br/>'))
    

  
  
}

# ~~~ all_contacts_tab_row_2 ----


contacts_under_surveillance_per_region_over_time_bar_chart <- 
  function(contacts_df_long){
    
    # filter out dates that are past the input days date
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(follow_up_date <= todays_date_reactive())
    
    data_to_plot <- 
      contacts_df_long %>%
      select(prefecture, follow_up_date) %>%
      group_by(follow_up_date, prefecture) %>% 
      count(prefecture) %>% 
      ungroup() %>% 
      arrange(prefecture, follow_up_date) %>% 
      bind_rows(tibble(follow_up_date =  seq.Date(from = min(.$follow_up_date), 
                                                  to = max(.$follow_up_date), 
                                                  by = "day"), 
                       prefecture = "temporary"
      )) %>% 
      complete(follow_up_date, prefecture, fill = list(n = 0)) %>% 
      filter(prefecture != "temporary") %>% 
      ungroup() %>% 
      # region with the most cases should be at the top
      group_by(prefecture) %>% 
      mutate(total = sum(n)) %>% 
      ungroup() %>% 
      mutate(prefecture = fct_rev(fct_reorder(prefecture, total)))
    
    if (nrow(data_to_plot) == 0) {
      return(highchart() %>% hc_title(text  = "No data to plot"))
    }
    
    data_to_plot %>% 
      hchart("column", hcaes(x = follow_up_date, y = n, group = prefecture)) %>% 
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




contacts_under_surveillance_per_region_over_time_bar_chart_relative <- 
  function(contacts_df_long){
    
    # filter out dates that are past the input days date
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(follow_up_date <= todays_date_reactive())
    
    data_to_plot <- 
      contacts_df_long %>%
      select(prefecture, follow_up_date) %>%
      group_by(follow_up_date, prefecture) %>% 
      count(prefecture) %>% 
      group_by(follow_up_date) %>% 
      mutate(prop = n/sum(n)) %>% 
      mutate(prop = round(prop, digits = 4)) %>% 
      ungroup() %>% 
      arrange(prefecture, follow_up_date) %>% 
      bind_rows(tibble(follow_up_date =  seq.Date(from = min(.$follow_up_date), 
                                                  to = max(.$follow_up_date), 
                                                  by = "day"), 
                       prefecture = "temporary"
      )) %>% 
      complete(follow_up_date, prefecture, fill = list(n = 0, prop = 0)) %>% 
      filter(prefecture != "temporary") %>% 
      ungroup() %>% 
      # region with the most cases should be at the top
      group_by(prefecture) %>% 
      mutate(total = sum(n)) %>% 
      ungroup() %>% 
      mutate(prefecture = fct_rev(fct_reorder(prefecture, total)))
    
    if (nrow(data_to_plot) == 0) {
      return(highchart() %>% hc_title(text  = "No data to plot"))
    }
    
    data_to_plot %>% 
      hchart("column", hcaes(x = follow_up_date, y = prop, group = prefecture)) %>% 
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



contacts_under_surveillance_per_region_over_time_text <- 
  function(contacts_df_long){
    
    # filter out dates that are past the input days date
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(follow_up_date <= todays_date_reactive())
    
    
    data_to_plot <- 
      contacts_df_long %>%
      select(prefecture, follow_up_date) %>%
      group_by(follow_up_date, prefecture) %>% 
      count(prefecture) %>% 
      ungroup() %>% 
      arrange(prefecture, follow_up_date) %>% 
      bind_rows(tibble(follow_up_date =  seq.Date(from = min(.$follow_up_date), 
                                                  to = max(.$follow_up_date), 
                                                  by = "day"), 
                       prefecture = "temporary"
      )) %>% 
      complete(follow_up_date, prefecture, fill = list(n = 0)) %>% 
      filter(prefecture != "temporary") %>% 
      ungroup() %>% 
      # region with the most cases should be at the top
      group_by(prefecture) %>% 
      mutate(total = sum(n)) %>% 
      ungroup() %>% 
      mutate(prefecture = fct_rev(fct_reorder(prefecture, total)))
    
    
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
      
    
    
    info <- "<br>
            <span style='color: rgb(97, 189, 109);'>ℹ:</span>
            <font size='1'>
            The plots show the number of contacts under surveillance on each day, whether or not they were successfully contacted.
            Access the data in tabular form by clicking on top-right button.
            </font>"
    str1 <- glue("<br>The day on which the highest number contacts were under surveillance was <b>{date_of_max_n_under_surveillance}</b>, 
                 with <b>{max_n_under_surveillance}</b> contacts under surveillance." )

   HTML(paste(info, str1, sep = '<br/>'))
    
    
  }


# ~~~ all_contacts_tab_row_3 ----

total_contacts_per_case_donut_plot <- 
  function(contacts_df_long){
    
    # filter out dates that are past the input days date
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(follow_up_date <= todays_date_reactive())
    
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
    
    if (nrow(data_to_plot) == 0) {
      return(highchart() %>% hc_title(text  = "No data to plot"))
    }
    
    data_to_plot %>%
      mutate(`Case ID` = fct_lump(`Case ID`, prop = 0.05, w = `Total linked contacts` )) %>% 
      group_by(`Case ID`) %>% 
      mutate(`Total linked contacts` = sum(`Total linked contacts`) ) %>% 
      slice_head() %>% 
      ungroup() %>% 
      hchart("pie", hcaes(name = `Case ID` , y = `Total linked contacts`),
             name = "n ",
             innerSize = "40%",
             showInLegend = TRUE,
             dataLabels = list(enabled = TRUE,
                               style = list(fontSize = 12),
                               format = '{point.y}, ({point.percentage:.1f} %)')) %>% 
      hc_exporting(enabled = TRUE)
    
    
  }


total_contacts_per_case_table <- 
  function(contacts_df_long){
    
    # filter out dates that are past the input days date
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(follow_up_date <= todays_date_reactive())
    
    
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
    
    

    data_to_plot %>%
      reactable(columns = list(`Total linked contacts` = colDef(cell = data_bars_gradient(data_to_plot, 
                                                                                              colors = c(peach, bright_yellow_crayola),
                                                                                              background = "transparent"),
                                                                    style = list(fontFamily = "Courier", whiteSpace = "pre", fontSize = 13))), 
                defaultPageSize = 15, 
                striped = TRUE,
                highlight = TRUE,
                theme = reactableTheme(stripedColor = "#f0f1fc",
                                       highlightColor = "#DADEFB"))
    
    
    
  }
  


total_contacts_per_case_text <- 
  function(contacts_df_long){
    
    # filter out dates that are past the input days date
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(follow_up_date <= todays_date_reactive())
    
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
    
    
    
    info <- "<br>
            <span style='color: rgb(97, 189, 109);'>ℹ:</span>
            <font size='1'>
            The plots show the number of contacts linked to each case.
            </font>"

    str1 <- glue("<br>The <b>mean</b> number of contacts per case is <b>{mean_number_of_contacts_per_case}</b>, (<b>SD:{sd_number_of_contacts_per_case}</b>) 
                 with a <b>minimum</b> of <b>{min_number_of_contacts_per_case}</b> and a maximum of <b>{max_number_of_contacts_per_case}</b>" )

    HTML(paste(info, str1, sep = '<br/>'))
    
  }


# ~~~ all_contacts_tab_row_4 ----


total_contacts_per_link_type_donut_plot <- 
  function(contacts_df_long){
    
    # filter out dates that are past the input days date
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(follow_up_date <= todays_date_reactive())
    
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
             `Number of contacts` = n
      )
    
    if (nrow(data_to_plot) == 0) {
      return(highchart() %>% hc_title(text  = "No data to plot"))
    }

      data_to_plot %>%
        hchart("pie", hcaes(name = `Link with the case`, y = `Number of contacts` ),
             innerSize = "40%",
             name = "n",
             showInLegend = TRUE,
             dataLabels = list(enabled = TRUE,
                               style = list(fontSize = 12),
                               format = '{point.name}: {point.y}, ({point.percentage:.1f} %)'))  %>%
        hc_exporting(enabled = TRUE)
    
    
  }





total_contacts_per_link_type_text <-
  function(contacts_df_long) {
    
    # filter out dates that are past the input days date
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(follow_up_date <= todays_date_reactive())
    
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
    
    
    second_link_type_with_most_contacts <-
      data_to_plot %>%
      .$`Link with the case` %>%
      .[2]
    
    second_number_of_contacts_link_type_with_most_contacts <-
      data_to_plot %>%
      .$`Number of contacts` %>%
      .[2]
    
    second_percent_link_type_with_most_contacts <-
      data_to_plot %>%
      .$Percent %>%
      .[2]
    
    
    
    info <- "<br>
            <span style='color: rgb(97, 189, 109);'>ℹ:</span>
            <font size='1'>
            The plots show the number of contacts per type of type of link. The categories have been cleaned and condensed.
            Access the data in tabular form by clicking on the top-right button.
            </font>"
    # 
    # str1 <-
    #   glue(
    #     "<br>The most common link category is <b>'{link_type_with_most_contacts}'</b>,
    #              with <b>{number_of_contacts_link_type_with_most_contacts}</b> contacts (<b>{percent_link_type_with_most_contacts}</b>),
    #              followed by <b>'{second_link_type_with_most_contacts}'</b>
    #              with <b>{second_number_of_contacts_link_type_with_most_contacts}</b> contacts (<b>{second_percent_link_type_with_most_contacts}</b>)
    #              "
    #   )
    
    
    str1 <-
      glue(
        "<br>The most common link category is <b>'{link_type_with_most_contacts}'</b>,
                 with <b>{number_of_contacts_link_type_with_most_contacts}</b> contacts (<b>{percent_link_type_with_most_contacts}</b>)."
      )
    
    HTML(paste(info, str1, sep = '<br/>'))
    
  }



# ~~~ all_contacts_tab_row_5 ----

total_contacts_vaccinated_bar_plot <-
  function(contacts_df_long) {
    
    # filter out dates that are past the input days date
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(follow_up_date <= todays_date_reactive())
    
    data_to_plot <-
      contacts_df_long %>%
      group_by(row_id) %>%
      # slice long frame
      slice_head() %>%
      ungroup() %>%
      count(sous_prefecture, vaccination) %>%
      group_by(sous_prefecture) %>%
      mutate(prop = n / sum(n)) %>%
      ungroup() %>%
      complete(sous_prefecture, vaccination, fill = list(n = 0, prop = 0, sum_n  = 0)) %>%
      group_by(sous_prefecture) %>%
      mutate(sum_n = sum(n)) %>%  # for arranging
      ungroup() %>%
      # region with the most cases should be at the top
      arrange(-sum_n, sous_prefecture) %>%
      mutate(vaccination = recode(
        vaccination,
        "Vaccine" =  "Vacciné",
        "Non_vaccine" = "Non vacciné"
      )) %>%
      mutate(vaccination = factor(vaccination, levels = c("Vacciné", "Non vacciné"))) %>% 
      mutate(hc_data_label = ifelse(vaccination == "Vacciné", 
                                    glue("{n} of {sum_n} vaccinated ({percent(prop)})"), 
                                    NA_character_
                                    ))
    
    if (nrow(data_to_plot) == 0) {
      return(highchart() %>% hc_title(text  = "No data to plot"))
    }
  
    data_to_plot %>%
      hchart("column", hcaes(y = n, x = sous_prefecture, group = vaccination), 
             dataLabels = list(enabled = TRUE,formatter = JS("
                                                             function(){return(this.point.hc_data_label)}") 
             ))  %>%
      hc_yAxis(visible = TRUE) %>%
      hc_plotOptions(
        column = list(
          stacking = "normal",
          groupPadding = 0.1,
          borderWidth = 0.05,
          stickyTracking = T
        )
      ) %>%
      hc_plotOptions(column = list(states = list(inactive = list(opacity = 0.7)))) %>%
      hc_xAxis(title = list(text = "Sous-prefecture")) %>%
      hc_yAxis(title = list(text = "Nombre de contacts")) %>%
      hc_exporting(enabled = TRUE) %>% 
      hc_chart(inverted = TRUE)
    
    
  }



total_contacts_vaccinated_text <-
  function(contacts_df_long) {
    
    # filter out dates that are past the input days date
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(follow_up_date <= todays_date_reactive())
    
    
    data_to_plot <-
      contacts_df_long %>%
      group_by(row_id) %>%
      # slice long frame
      slice_head() %>%
      ungroup() %>%
      count(sous_prefecture, vaccination) %>%
      group_by(sous_prefecture) %>%
      mutate(prop = n / sum(n)) %>%
      ungroup() %>%
      complete(sous_prefecture, vaccination, fill = list(n = 0, prop = 0, sum_n  = 0)) %>%
      group_by(sous_prefecture) %>%
      mutate(sum_n = sum(n)) %>%  # for arranging
      ungroup() %>%
      # region with the most cases should be at the top
      arrange(-sum_n, sous_prefecture) %>%
      mutate(vaccination = recode(
        vaccination,
        "Vaccine" =  "Vacciné",
        "Non_vaccine" = "Non vacciné"
      )) %>%
      mutate(vaccination = factor(vaccination, levels = c("Vacciné", "Non vacciné"))) %>% 
      mutate(hc_data_label = ifelse(vaccination == "Vacciné", 
                                    glue("{n} of {sum_n} vaccinated ({percent(prop)})"), 
                                    NA_character_
      )) %>% 
      pivot_wider(sous_prefecture, vaccination, values_from = c(n, prop, sum_n, hc_data_label))
    
    
    
    prefecture_1<- 
      data_to_plot %>% 
      .$sous_prefecture %>% 
      .[1]
       
    prefecture_1_vaccination_coverage<- 
      data_to_plot %>% 
      .$prop_Vacciné %>% 
      .[1] %>% 
      percent()
    
    # prefecture_2<- 
    #   data_to_plot %>% 
    #   .$sous_prefecture %>% 
    #   .[2]
    # 
    # prefecture_2_vaccination_coverage<- 
    #   data_to_plot %>% 
    #   .$prop_Vacciné %>% 
    #   .[2] %>% 
    #   percent()

    info <- "<br>
            <span style='color: rgb(97, 189, 109);'>ℹ:</span>
            <font size='1'>
            The plot shows the number and percentage of contacts vaccinated in each region.
            Access the data in tabular form by clicking on the top-right button.
            </font>"
# 
#     str1 <-
#       glue(
#         "<br><b>{prefecture_1_vaccination_coverage}</b> of all contacts have been vaccinated in <b>{prefecture_1}</b>, 
#         and <b>{prefecture_2_vaccination_coverage}</b> have been vaccinated in <b>{prefecture_2}</b>
#         "
#       )
#     
    
    str1 <-
      glue(
        "<br><b>{prefecture_1_vaccination_coverage}</b> of all contacts have been vaccinated in <b>{prefecture_1}</b>
        "
      )

    HTML(paste(info, str1, sep = '<br/>'))

  }



# ~~~ all_contacts_tab_row_6 ----


contacts_timeline_snake_plot <- 
  function(contacts_df_long, todays_date){
    
    # number_to_sample <- 50
    
    ## - if sampling dropdown has not been generated nor clicked yet, do not sample
    if(is.null(input$snake_plot_sample_or_not)){
      snake_plot_sample_or_not <- c("Do not sample")
    } else {
      snake_plot_sample_or_not <- input$snake_plot_sample_or_not
    }
    
    
    
    active_contacts <-
      contacts_df_long %>%
      group_by(row_id) %>% 
      ## keep active cases. 
      ## active cases whose last day of follow-up is today any day past today
      filter(max(follow_up_date) >= todays_date_reactive()) %>%
      ungroup()
    
    data_to_plot <-
      active_contacts %>%
      group_by(row_id) %>% 
      mutate(hc_ttip = glue("<b>ID: </b> {id_contact} <br>
                         <b>Sous-prefecture: </b> {sous_prefecture}<br>
                         <b>Date: </b> {format.Date(follow_up_date, format = '%b %d')} (Jour de suivi {follow_up_day}) <br>
                         <b>Status:</b> {etat_suivi}
                         ")) %>%
      mutate(hc_ttip = glue("<b>ID: </b> {id_contact} <br>
                         <b>Date: </b> {format.Date(follow_up_date, format = '%b %d')} (Jour de suivi {follow_up_day}) <br>
                         ")) %>% 
      ungroup() %>% 
      arrange(etat_suivi) %>%   # arranging is necessary so that that colors are pulled in the right order for highcharter
      mutate(follow_up_date_timestamp = datetime_to_timestamp(follow_up_date))
    
    if (nrow(data_to_plot) == 0) {
      return(e_charts() %>% e_title(text  = "No data to plot"))
    }
    
    data_to_plot %>% 
      group_by(etat_suivi) %>%
      e_charts(follow_up_date, height = "900px") %>%
      e_scatter(row_id, bind = hc_ttip, symbol_size = 5,  itemStyle = list(opacity = 0.9)) %>%
      e_tooltip(trigger = "item") %>% 
      e_tooltip(formatter = htmlwidgets::JS("
                                        function(params){
                                        return(params.name) }")) %>% 
      e_toolbox_feature("dataZoom", title = list(zoom = "zoom", back = "back")) %>%
      e_toolbox_feature("saveAsImage", type = "png", pixelRatio = 4) %>%
      e_color(unique(data_to_plot$colors)) %>% 
      e_legend(show = FALSE) %>% 
      e_axis_labels(x = "Date", y = "Row ID") %>% 
      e_y_axis(max = round(max(data_to_plot$row_id)), 
               min = round(max(data_to_plot$row_id))
               )
    
    
  }


active_contacts_breakdown_bar_chart <- 
  function(contacts_df_long){
    
    
    active_contacts <-
      contacts_df_long %>%
      group_by(row_id) %>% 
      ## keep active cases. 
      ## active cases whose last day of follow-up is today any day past today
      filter(max(follow_up_date) >= todays_date_reactive()) %>%
      ungroup()
 
    # to be fed to highcharter
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

    
  }


contacts_timeline_snake_text <- function(contacts_df_long, todays_date){
  
  
  
  
  n_active_contacts <-
    contacts_df_long %>%
    group_by(row_id) %>% 
    ## keep active cases. 
    ## active cases whose last day of follow-up is today any day past today
    filter(max(follow_up_date) >= todays_date_reactive()) %>%
    ungroup() %>% 
    .$row_id %>%
    unique() %>%
    length()
  
  HTML(glue("There are <b>{n_active_contacts}</b> active contacts"))
  
}



active_contacts_breakdown_table <- 
  function(contacts_df_long){
    
    
    active_contacts <-
      contacts_df_long %>%
      group_by(row_id) %>% 
      ## keep active cases. 
      ## active cases whose last day of follow-up is today any day past today
      filter(max(follow_up_date) >= todays_date_reactive()) %>%
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
                          follow_up_date <= todays_date_reactive(), 
                        NA_integer_, 
                        n)) %>% 
      mutate(prop = n/sum(n)) %>% 
      mutate(hc_ttip = glue("<b>Date:</b> {format.Date(follow_up_date,  format = '%b %d')}
                        <b>{etat_suivi}:</b> {n}
                        ")) %>% 
      arrange(etat_suivi) %>%   # arranging is necessary so that that colors are pulled in the right order for highcharter
      left_join(colors)
    
    
    data_to_plot %>% 
      select(1:3) %>% 
      pivot_wider(names_from = etat_suivi, values_from = n) %>% 
      select(Date = follow_up_date, everything()) %>% 
      reactable(searchable = TRUE,
                striped = TRUE,
                highlight = TRUE,
                theme = reactableTheme(stripedColor = "#f0f1fc",
                                      highlightColor = "#DADEFB",
                                      cellPadding = "8px 12px",
                                      style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif")
                )
      )
    
  
  }




# ~~~ all_contacts_tab_row_7 ----


contacts_lost_24_to_72_hours <- 
  function(contacts_df_long){
    
    
    if((contacts_df_long %>%
        filter(follow_up_date == todays_date_reactive()) %>% 
        nrow()) == 0){
      return(
        "No data to show" %>% 
          data.frame() %>% 
          gt() %>% 
          fmt_markdown(columns = 1) %>%
          tab_options(column_labels.hidden = T)
      )
    }
    
    
    vu_non_vu_today <-
      contacts_df_long %>%
      filter(follow_up_date == todays_date_reactive()) %>% 
      group_by(sous_prefecture) %>% 
      count(etat_suivi_simple) %>% 
      ungroup() %>% 
      bind_rows(data.frame(etat_suivi_simple = c("Vu", "Non vu"), 
                           sous_prefecture = "!!+temporary",
                           n = 0)) %>% 
      complete(sous_prefecture, etat_suivi_simple, 
               fill = list(n = 0)) %>% 
      filter(sous_prefecture != "!!+temporary") %>% 
      pivot_wider(names_from = etat_suivi_simple, values_from = n) %>% 
      clean_names() %>% 
      mutate(total = non_vu + vu) %>% 
      mutate(pct_non_vu = percent(non_vu/ total, accuracy = 0.1)) %>% 
      select(`Sous-prefecture` = sous_prefecture, 
             Total = total, 
             `Not seen` = non_vu, 
             `% Not seen` = pct_non_vu) %>% 
      rename_with(.cols = c(Total, `Not seen`, `% Not seen`), 
                  .fn = ~ paste0("Today.", .x))
      
      
    vu_non_vu_past_two_days <- 
    contacts_df_long %>%
      filter(follow_up_date >= todays_date_reactive() - 1) %>% 
      group_by(id_contact) %>% 
      mutate(etat_suivi_simple = ifelse( any(etat_suivi_simple == "Vu"),
                                          "Vu",
                                          "Non vu")) %>% 
      slice_head(n = 1) %>% 
      ungroup() %>% 
      group_by(sous_prefecture) %>% 
      count(etat_suivi_simple) %>% 
      ungroup() %>% 
      bind_rows(data.frame(etat_suivi_simple = c("Vu", "Non vu"), 
                           sous_prefecture = "!!+temporary",
                           n = 0)) %>% 
      complete(sous_prefecture, etat_suivi_simple, 
               fill = list(n = 0)) %>% 
      filter(sous_prefecture != "!!+temporary") %>% 
      pivot_wider(names_from = etat_suivi_simple, values_from = n) %>% 
      clean_names() %>% 
      mutate(total = non_vu + vu) %>% 
      mutate(pct_non_vu = percent(non_vu/ total, accuracy = 0.1)) %>% 
      select(`Sous-prefecture` = sous_prefecture, 
             Total = total, 
             `Not seen` = non_vu, 
             `% Not seen` = pct_non_vu) %>% 
      rename_with(.cols = c(Total, `Not seen`, `% Not seen`),
                  .fn = ~ paste0("Past 2d.", .x))
    
    
    
    vu_non_vu_past_three_days <- 
      contacts_df_long %>%
      filter(follow_up_date >= todays_date_reactive() - 2) %>% 
      group_by(id_contact) %>% 
      mutate(etat_suivi_simple = ifelse( any(etat_suivi_simple == "Vu"),
                                         "Vu",
                                         "Non vu")) %>% 
      slice_head(n = 1) %>% 
      ungroup() %>% 
      group_by(sous_prefecture) %>% 
      count(etat_suivi_simple) %>% 
      ungroup() %>% 
      bind_rows(data.frame(etat_suivi_simple = c("Vu", "Non vu"), 
                           sous_prefecture = "!!+temporary",
                           n = 0)) %>% 
      complete(sous_prefecture, etat_suivi_simple, 
               fill = list(n = 0)) %>% 
      filter(sous_prefecture != "!!+temporary") %>% 
      pivot_wider(names_from = etat_suivi_simple, values_from = n) %>% 
      clean_names() %>% 
      mutate(total = non_vu + vu) %>% 
      mutate(pct_non_vu = percent(non_vu/ total, accuracy = 0.1)) %>% 
      select(`Sous-prefecture` = sous_prefecture, 
             Total = total, 
             `Not seen` = non_vu, 
             `% Not seen` = pct_non_vu) %>% 
      rename_with(.cols = c(Total, `Not seen`, `% Not seen`),
                  .fn = ~ paste0("Past 3d.", .x))
    
    
    ## Number of days covered. At certain points, there is only one date covered 
     number_of_days_covered <- contacts_df_long %>%
      filter(follow_up_date >= todays_date_reactive() - 1) %>% 
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
    
    if(is.null(data_to_plot) | nrow(data_to_plot) == 0){
      return(
        "No data to show" %>% 
          data.frame() %>% 
          gt() %>% 
          fmt_markdown(columns = 1) %>%
          tab_options(column_labels.hidden = T)
      )
    }
    data_to_plot %>% 
      gt() %>% 
      fmt_missing(columns = everything(), missing_text = "-") %>% 
      data_color(columns = "Past 3d.Not seen", 
                 colors = scales::col_numeric(palette = paletteer_d(palette = "ggsci::red_material") %>% as.character(), 
                                              domain = c(1, max(vu_non_vu_past_three_days$`Past 3d.Not seen`) * 4)
                                              ),
                 autocolor_text = F ) %>% 
      tab_spanner_delim(delim = ".") %>%
      tab_style(locations = cells_column_labels(columns = everything()),
                style = list(cell_borders(sides = "bottom", weight = px(3)),
                     cell_text(weight = "bold"))) %>%
      tab_style(locations = cells_column_spanners(spanners = everything()),
                style = list(cell_text(weight = "bold", size = "large"))) %>%
      opt_row_striping() %>% 
      tab_source_note(source_note = md("*Not seen in past 2/3 days means not seen on **any** of the past 2/3 days"))

    
  }


lost_contacts_linelist <- 
  function(contacts_df_long){

    
    non_vu_past_three_days <- 
      contacts_df_long %>%
      filter(follow_up_date >= todays_date_reactive() - 2) %>% 
      group_by(id_contact) %>% 
      mutate(etat_suivi_simple = ifelse( any(etat_suivi_simple == "Vu"),
                                         "Vu",
                                         "Non vu")) %>% 
      slice_head(n = 1) %>% 
      ungroup() %>% 
      filter(etat_suivi_simple == "Non vu")
    
    data_to_tabulate <- contacts_df_long %>%
      filter(id_contact %in% non_vu_past_three_days$id_contact) %>% 
      group_by(id_contact) %>% 
      mutate(etat_suivi_simple = ifelse( any(etat_suivi_simple == "Vu"),
                                         "Vu",
                                         "Non vu")) %>% 
      ungroup() %>% 
      filter(etat_suivi_simple == "Non vu") %>% 
      select(ID = id_contact, 
             `Sous-prefecture` = sous_prefecture, 
             Dates = follow_up_date, 
             `Follow-up States` = etat_suivi) %>% 
      group_by(ID) %>% 
      arrange(Dates) %>% 
      ungroup() 
    
    data_to_tabulate %>% 
      reactable(groupBy = "ID", 
                columns = list(`Follow-up States` = colDef(aggregate = "frequency"), 
                               `Sous-prefecture` = colDef(aggregate = "unique")
                               ),
                searchable = TRUE,
                striped = TRUE,
                highlight = TRUE,
                theme = reactableTheme(stripedColor = "#f0f1fc70",
                                       backgroundColor = "#FFFFFF00",
                                       highlightColor = "#DADEFB")
      )
  
    
  }


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~  all_contacts_tab_regional ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~ todays_date_reactive_regional ----

## does not need to be inside of a reactive atm. But it was put there initially so I can trigger it with ObserveEvent
todays_date_reactive_regional <- reactive({
  req(input$contacts_tab_select_regional)
  input$select_date_of_review_regional
})


# ~~~ read_file_regional ----

read_file_regional <- reactive({
  
  contacts_df_raw <- read_file_raw()
  
  # data in
  contacts_df <-
    contacts_df_raw %>%
    # clean vaccination column, type_de_contact, unite_age, prefecture, lien_avec_las_cas. 
    # No etat_suivi column to clean yet because I have not created it
    linelist::clean_variable_spelling(wordlists = cleaning_rules, warn = F) %>%
    # drop records that should not exist based on faux date
    filter(date_du_dernier_contact <= todays_date_reactive_regional()) %>%
    filter(date_enreg <= todays_date_reactive_regional()) %>%
    # add counter column. 1 for all records
    mutate(counter = 1) %>%
    # row numbers to match Excel spreadsheet
    mutate(row_id = row_number() + 3) %>%
    # convert dates to dates
    mutate(across(.cols = matches("date_"),
                  .fns = ~ as.Date(.x))) %>%
    # rename the follow_up_day columns
    rename_with(.cols = paste0("j", 1:21),
                .fn = ~ paste("follow_up_day", str_remove_all(.x, "j"), sep = "_")) %>%
    # duplicate the follow_up_day columns, change name to follow_up_date
    mutate(across(.cols = paste("follow_up_day", 1:21, sep = "_"),
                  .fns = ~ .,
                  .names = "follow_up_date_{str_remove_all(.col, 'follow_up_day_')}")) %>%
    # now, we convert the numbers in the follow up day columns
    # to reflect the nth day of follow up
    mutate(across(.cols = starts_with("follow_up_date_"),
                  .fn = ~ str_extract(cur_column(), paste(21:1, collapse = "|")) %>% as.numeric() )) %>% # start from 21 otherwise you'll remove the tens unit before you get to the teens
    # clean vaccination column
    mutate(across(vaccination,
                  ~ .x %>%
                    stri_trans_general("Latin-ASCII") %>%
                    str_to_lower() %>%
                    str_to_sentence())) %>%
    # clean admin levels
    mutate(across(c(prefecture, sous_prefecture, quartier), 
                  ~ .x %>%
                    str_to_sentence() %>% 
                    str_trim() %>% 
                    str_replace_all("  ", " ")))
  
  
  pivot_day <- 
    contacts_df %>% 
    select(row_id, paste0("follow_up_day_", 1:21)) %>% 
    pivot_longer(cols = c(paste0("follow_up_day_", 1:21)), 
                 names_to = "follow_up_day", 
                 values_to = "etat_suivi") %>% 
    # clean "etat de suivi" column
    linelist::clean_variable_spelling(wordlists = cleaning_rules)
  
  
  legend_df <- 
    tribble(    
      ~breaks ,                  ~colors,                   
      "Non vu",                  col2hex("orangered"),                  
      "Vu",                      col2hex("lightseagreen"),  
      "Cas confirmé",            col2hex("purple3"),     
      "Cas suspect",             col2hex("yellow"),       
      "Manquant",                col2hex("pink4"),          
      "Deplacé",                 col2hex("wheat3"),  
      "Recyclé",                 col2hex("yellow4"),          
      "Fin de suivi",            col2hex("dodgerblue3"),    
      "Suivi futur",             col2hex("goldenrod"),      
      "Données manquantes",      col2hex("black"),          
      "Doublon",                 col2hex("slategray3"),          
    ) %>% 
    arrange(breaks) %>% 
    mutate(breaks = fct_inorder(breaks)) %>% 
    mutate(legend_index = row_number())
  
  
  contacts_df_long <- 
    contacts_df %>%
    inner_join(pivot_day, by = "row_id") %>%
    # paste the day of followup
    mutate(follow_up_day = str_extract(follow_up_day, paste(21:1, collapse = "|"))) %>%
    mutate(follow_up_day = as.numeric(follow_up_day)) %>%
    # assume that follow up begins from date of last interaction
    mutate(follow_up_date = follow_up_day + date_du_dernier_contact) %>%
    mutate(etat_suivi = as.character(etat_suivi)) %>%
    # change status of records that should be still active
    mutate(etat_suivi = if_else(follow_up_date > todays_date_reactive_regional(),
                                "Suivi futur",
                                etat_suivi
    )) %>%
    mutate(etat_suivi = replace_na(etat_suivi, "Données manquantes")) %>% 
    # - if follow-up lasted the full 21 days,
    # - change last follow_up state to "Fin de suivi"
    mutate(etat_suivi = if_else(follow_up_day == 21 & etat_suivi == "Vu",
                                "Fin de suivi",
                                etat_suivi)) %>% 
    # add legend colors
    left_join(legend_df, by = c("etat_suivi" = "breaks")) %>% 
    # change records to "Suivi Futur" if ahead of today's date
    mutate(etat_suivi = if_else(follow_up_date > todays_date_reactive_regional(),
                                "Suivi futur",
                                etat_suivi
    ))  %>% 
    mutate(etat_suivi_simple = etat_suivi) %>% 
    mutate(etat_suivi_simple = recode(etat_suivi_simple,
                                      "Vu" = "Vu",
                                      "Non vu" = "Non vu",
                                      "Cas suspect" = "Vu",
                                      "Decedé" = "Vu",
                                      "Deplacé" = "Non vu",
                                      "Cas confirmé" = "Vu",
                                      "Transferé" = "Vu",
                                      "Refus" = "Non vu",
                                      "Reco. pas passé" = "Non vu",
                                      "Doublon" = "Doublon",
                                      "Recyclé" = "Vu", 
                                      "Fin de suivi" = "Vu", 
                                      "Données manquantes" = "Non vu"
    )) %>% 
    filter(etat_suivi != "Doublon")
  
  read_file_out <- list(contacts_df_raw = contacts_df_raw, 
                        contacts_df = contacts_df, 
                        contacts_df_long = contacts_df_long)
  
  
  return(read_file_out)
})


# ~~~ all_contacts_tab_row_1_regional ----

all_contacts_per_region_table_regional <- 
  function(contacts_df_long){
    
    
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(prefecture == input$contacts_tab_select_regional)
    
    # filter out dates that are past the input days date
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(follow_up_date <= todays_date_reactive_regional())
    
    data_to_plot <- 
      contacts_df_long %>%
      group_by(row_id) %>% 
      # slice long frame
      slice_head() %>% 
      ungroup() %>% 
      select(sous_prefecture, quartier) %>%
      count(sous_prefecture, quartier) %>%
      group_by(sous_prefecture, quartier) %>%
      summarise(n = sum(n)) %>%
      ungroup() %>% 
      mutate(percent = round(100 * n/sum(n), 2)) %>% 
      group_by(sous_prefecture) %>%
      mutate(sous_prefecture_sum = sum(n)) %>% 
      ungroup() %>% 
      mutate(sous_prefecture_percent = sous_prefecture_sum/sum(n)) %>% 
      ungroup() %>% 
      arrange(-sous_prefecture_percent, -n) %>% 
      select(`Sous-prefecture` = sous_prefecture, 
             `Quartier` = quartier, 
             `Total contacts` = n,
             `%` = percent)
    

    
    data_to_plot %>%
      reactable(columns = list(`Total contacts` = colDef(cell = data_bars_gradient(data_to_plot, 
                                                                                   colors = c(peach, bright_yellow_crayola),
                                                                                   background = "transparent"),
                                                         style = list(fontFamily = "Courier", whiteSpace = "pre", fontSize = 13)), 
                               `Sous-prefecture` = colDef(style = JS("function(rowInfo, colInfo, state) {
                                                          var firstSorted = state.sorted[0]
                                                          // Merge cells if unsorted or sorting by Prefecture
                                                          if (!firstSorted || firstSorted.id === 'Sous-prefecture') {
                                                          var prevRow = state.pageRows[rowInfo.viewIndex - 1]
                                                          if (prevRow && rowInfo.row['Sous-prefecture'] === prevRow['Sous-prefecture']) {
                                                          return { visibility: 'hidden' }
                                                          }
                                                          }}"))),
                striped = TRUE,
                highlight = TRUE,
                theme = reactableTheme(stripedColor = "#f0f1fc",
                                       highlightColor = "#DADEFB"),
                defaultPageSize = 15)
    
  }




all_contacts_per_region_sunburst_plot_regional <- 
  function(contacts_df_long){
    
    
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(prefecture == input$contacts_tab_select_regional)
    
    # filter out dates that are past the input days date
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(follow_up_date <= todays_date_reactive_regional())
    
    

    contact_regions <-
      contacts_df_long %>%
      group_by(row_id) %>% 
      # slice long frame
      slice_head() %>% 
      ungroup() %>%
      select(sous_prefecture, quartier) %>%
      mutate(across(
        .fns =
          ~ .x %>%
          str_to_lower() %>%
          str_to_title() %>%
          replace_na("NA") %>%
          str_trim()
      )) %>%
      mutate(sous_prefecture = fct_lump(other_level = "Autres", 
                                        sous_prefecture, prop = 0.01)) %>%
      add_count(sous_prefecture, name = "n_sous_prefecture") %>%
      mutate(pct_sous_prefecture = round(100 * n_sous_prefecture / nrow(.), 
                                         digits = 2)) %>%
      mutate(sous_prefecture = paste0(sous_prefecture, " (", pct_sous_prefecture, "%", ")")) %>%
      group_by(sous_prefecture) %>%
      mutate(quartier = fct_lump(other_level = "Autres", quartier, prop = 0.02)) %>%
      add_count(quartier, name = "n_quartier") %>%
      mutate(pct_quartier = round(100 * n_quartier / nrow(.), digits = 2)) %>%
      mutate(quartier = paste0(quartier, " (", pct_quartier, "%", ")")) %>%
      group_by(sous_prefecture, quartier) %>%
      slice_head(n = 1) %>%
      ungroup() %>%
      arrange(-n_sous_prefecture)
    
    if (nrow(contact_regions) == 0) {
      return(highchart() %>% hc_title(text  = "No data to plot"))
    }
    
    color_df <- 
      data.frame(sous_prefecture = unique(contact_regions$sous_prefecture)) %>% 
      add_column(color_lvl_1 = final_palette[1:nrow(.)])
    
    contact_regions_list <- 
      contact_regions %>% 
      data_to_hierarchical(group_vars = c(sous_prefecture, 
                                          quartier
      ), 
      size_var = n_quartier, 
      colors= color_df$color_lvl_1)
    
    
    x <- c("Type: ", "n = ")
    y <- c("{point.name}", "{point.value}")
    tltip <- tooltip_table(x, y)
    
    
    highchart() %>% 
      hc_chart(type = "sunburst") %>% 
      #hc_subtitle(text = subtitle, useHTML = TRUE) %>% 
      hc_add_series(data = contact_regions_list,
                    allowDrillToNode = TRUE,
                    levelIsConstant = FALSE,
                    #textOverflow = "clip",
                    levels = list(list(level = 1,
                                       borderWidth = 1, 
                                       dataLabels = list(enabled = TRUE,
                                                         color = "#FFFFFF",
                                                         style = list(fontSize = "14px",
                                                                      #fontWeight = "bold",
                                                                      #textOutline = "white", 
                                                                      opacity = 0.8))), 
                                  list(level = 2, 
                                       borderWidth = 0,
                                       dataLabels = list(enabled = TRUE, 
                                                         color = "#FFFFFF",
                                                         style = list(fontSize = "12px", 
                                                                      textOutline = FALSE, 
                                                                      opacity = 0.8))), 
                                  list(level = 3,
                                       borderWidth = 0,
                                       dataLabels = list(enabled = TRUE, 
                                                         color = "#FFFFFF",
                                                         style = list(fontSize = "8px", 
                                                                      textOutline = FALSE, 
                                                                      opacity = 0.8))))) %>% 
      hc_plotOptions(sunburst = list(dataLabels = list(enabled = TRUE) )) %>% 
      hc_tooltip(useHTML = TRUE, 
                 headerFormat = "", pointFormat = tltip) %>% 
      hc_exporting(enabled = TRUE)
    
    
  }




all_contacts_per_region_text_regional <- 
  function(contacts_df_long){
    
    
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(prefecture == input$contacts_tab_select_regional)
    
    # filter out dates that are past the input days date
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(follow_up_date <= todays_date_reactive_regional())
    
    data_to_plot <- 
      contacts_df_long %>%
      group_by(row_id) %>% 
      # slice long frame
      slice_head() %>% 
      ungroup() %>% 
      select(sous_prefecture, quartier) %>%
      count(sous_prefecture, quartier) %>%
      group_by(sous_prefecture, quartier) %>%
      summarise(n = sum(n)) %>%
      ungroup() %>% 
      mutate(percent = round(100 * n/sum(n), 2)) %>% 
      group_by(sous_prefecture) %>%
      mutate(sous_prefecture_sum = sum(n)) %>% 
      ungroup() %>% 
      mutate(sous_prefecture_percent = sous_prefecture_sum/sum(n)) %>% 
      ungroup() %>% 
      select(sous_prefecture, 
             sous_prefecture_sum,
             sous_prefecture_percent,
             quartier, 
             quartier_sum = n,
             quartier_percent = percent)
    
    region_w_most_contacts <- 
      data_to_plot %>% 
      arrange(-sous_prefecture_percent) %>% 
      .$sous_prefecture %>% .[1]
    
    n_contacts_in_region_w_most_contacts <- 
      data_to_plot %>% 
      arrange(-sous_prefecture_percent) %>% 
      .$sous_prefecture_sum %>% .[1]
    
    pct_contacts_in_region_w_most_contacts <- 
      data_to_plot %>% 
      arrange(-sous_prefecture_percent) %>% 
      .$sous_prefecture_percent %>% .[1] %>% 
      magrittr::multiply_by(100) %>% 
      round(1) %>% 
      paste0(., "%")
    
    
    str1 <- glue("<br>The Sous-prefecture with the most total contacts since database inception is <b> {region_w_most_contacts}</b>, 
                 with <b>{n_contacts_in_region_w_most_contacts}</b> contacts (<b>{pct_contacts_in_region_w_most_contacts}</b> of the total)" )
    
    info <- "<br>
            <span style='color: rgb(97, 189, 109);'>ℹ:</span>
            <font size='1'>The table and plot show the count of all contacts seen in each sous-prefecture, since the beginning of the epidemic. </font>"
    
    HTML(paste(info, str1, sep = '<br/>'))
    
  }




# ~~~ all_contacts_tab_row_2_regional ----


contacts_under_surveillance_per_region_over_time_bar_chart_regional <- 
  function(contacts_df_long){
    
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(prefecture == input$contacts_tab_select_regional)
    
    
    # filter out dates that are past the input days date
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(follow_up_date <= todays_date_reactive_regional())
    
    
    if (nrow(contacts_df_long) == 0) {
      return(highchart() %>% hc_title(text  = "No data to plot"))
    }
    
    data_to_plot <- 
      contacts_df_long %>%
      select(sous_prefecture, follow_up_date) %>%
      group_by(follow_up_date, sous_prefecture) %>% 
      count(sous_prefecture) %>% 
      ungroup() %>% 
      arrange(sous_prefecture, follow_up_date) %>% 
      bind_rows(tibble(follow_up_date =  seq.Date(from = min(.$follow_up_date), 
                                                  to = max(.$follow_up_date), 
                                                  by = "day"), 
                       sous_prefecture = "temporary"
      )) %>% 
      complete(follow_up_date, sous_prefecture, fill = list(n = 0)) %>% 
      filter(sous_prefecture != "temporary") %>% 
      ungroup() %>% 
      # region with the most cases should be at the top
      group_by(sous_prefecture) %>% 
      mutate(total = sum(n)) %>% 
      ungroup() %>% 
      mutate(sous_prefecture = fct_rev(fct_reorder(sous_prefecture, total)))
    
    
    if (nrow(data_to_plot) == 0) {
      return(highchart() %>% hc_title(text  = "No data to plot"))
    }
    
    data_to_plot %>% 
      hchart("column", hcaes(x = follow_up_date, y = n, group = sous_prefecture)) %>% 
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
  function(contacts_df_long){
    
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(prefecture == input$contacts_tab_select_regional) 
    
    
    # filter out dates that are past the input days date
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(follow_up_date <= todays_date_reactive_regional())
    
    if (nrow(contacts_df_long) == 0) {
      return(highchart() %>% hc_title(text  = "No data to plot"))
    }
    
    data_to_plot <- 
      contacts_df_long %>%
      select(sous_prefecture, follow_up_date) %>%
      group_by(follow_up_date, sous_prefecture) %>% 
      count(sous_prefecture) %>% 
      group_by(follow_up_date) %>% 
      mutate(prop = n/sum(n)) %>% 
      mutate(prop = round(prop, digits = 4)) %>% 
      ungroup() %>% 
      arrange(sous_prefecture, follow_up_date) %>% 
      bind_rows(tibble(follow_up_date =  seq.Date(from = min(.$follow_up_date), 
                                                  to = max(.$follow_up_date), 
                                                  by = "day"), 
                       sous_prefecture = "temporary"
      )) %>% 
      complete(follow_up_date, sous_prefecture, fill = list(n = 0, prop = 0)) %>% 
      filter(sous_prefecture != "temporary") %>% 
      ungroup() %>% 
      # region with the most cases should be at the top
      group_by(sous_prefecture) %>% 
      mutate(total = sum(n)) %>% 
      ungroup() %>% 
      mutate(sous_prefecture = fct_rev(fct_reorder(sous_prefecture, total)))
    
    
    if (nrow(data_to_plot) == 0) {
      return(highchart() %>% hc_title(text  = "No data to plot"))
    }
    
    data_to_plot %>% 
      hchart("column", hcaes(x = follow_up_date, y = prop, group = sous_prefecture)) %>% 
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
  function(contacts_df_long){
    
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(prefecture == input$contacts_tab_select_regional)
    
    # filter out dates that are past the input days date
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(follow_up_date <= todays_date_reactive_regional())
    
    
    if (nrow(contacts_df_long) == 0) {
      return(HTML("No data"))
    }
    
    
    data_to_plot <- 
      contacts_df_long %>%
      select(sous_prefecture, follow_up_date) %>%
      group_by(follow_up_date, sous_prefecture) %>% 
      count(sous_prefecture) %>% 
      ungroup() %>% 
      arrange(sous_prefecture, follow_up_date) %>% 
      bind_rows(tibble(follow_up_date =  seq.Date(from = min(.$follow_up_date), 
                                                  to = max(.$follow_up_date), 
                                                  by = "day"), 
                       sous_prefecture = "temporary"
      )) %>% 
      complete(follow_up_date, sous_prefecture, fill = list(n = 0)) %>% 
      filter(sous_prefecture != "temporary") %>% 
      ungroup() %>% 
      # region with the most cases should be at the top
      group_by(sous_prefecture) %>% 
      mutate(total = sum(n)) %>% 
      ungroup() %>% 
      mutate(sous_prefecture = fct_rev(fct_reorder(sous_prefecture, total)))
    
    
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
    str1 <- glue("<br>The day on which the highest number contacts in {input$contacts_tab_select_regional} were under surveillance was <b>{date_of_max_n_under_surveillance}</b>, 
                 with <b>{max_n_under_surveillance}</b> contacts under surveillance." )
    
    HTML(paste(info, str1, sep = '<br/>'))
    
    
  }



# ~~~ all_contacts_tab_row_3_regional ----


total_contacts_per_case_donut_plot_regional <- 
  function(contacts_df_long){
    
    
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(prefecture == input$contacts_tab_select_regional)
    
    
    if (nrow(contacts_df_long) == 0) {
      return(highchart() %>% hc_title(text  = "No data to plot"))
    }
    
    
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
    
    if (nrow(data_to_plot) == 0) {
      return(highchart() %>% hc_title(text  = "No data to plot"))
    }
    
    data_to_plot %>%
      mutate(`Case ID` = fct_lump(`Case ID`, prop = 0.05, w = `Total linked contacts` )) %>% 
      group_by(`Case ID`) %>% 
      mutate(`Total linked contacts` = sum(`Total linked contacts`) ) %>% 
      slice_head() %>% 
      ungroup() %>% 
      hchart("pie", hcaes(name = `Case ID` , y = `Total linked contacts`),
             name = "n ",
             innerSize = "40%",
             showInLegend = TRUE,
             dataLabels = list(enabled = TRUE,
                               style = list(fontSize = 12),
                               format = '{point.y}, ({point.percentage:.1f} %)')) %>% 
      hc_exporting(enabled = TRUE)
    
    
  }


total_contacts_per_case_table_regional <- 
  function(contacts_df_long){
    
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(prefecture == input$contacts_tab_select_regional)
    
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
    
    
    
    data_to_plot %>%
      reactable(columns = list(`Total linked contacts` = colDef(cell = data_bars_gradient(data_to_plot, 
                                                                                          colors = c(peach, bright_yellow_crayola),
                                                                                          background = "transparent"),
                                                                style = list(fontFamily = "Courier", whiteSpace = "pre", fontSize = 13))), 
                defaultPageSize = 15, 
                striped = TRUE,
                highlight = TRUE,
                theme = reactableTheme(stripedColor = "#f0f1fc",
                                       highlightColor = "#DADEFB"))
    
    
    
  }



total_contacts_per_case_text_regional <- 
  function(contacts_df_long){
    
    
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(prefecture == input$contacts_tab_select_regional)
    
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
    
    
    
    info <- "<br>
            <span style='color: rgb(97, 189, 109);'>ℹ:</span>
            <font size='1'>
            The plots show the number of contacts linked to each case.
            </font>"
    
    str1 <- glue("<br>The <b>mean</b> number of contacts per case is <b>{mean_number_of_contacts_per_case}</b>, (<b>SD:{sd_number_of_contacts_per_case}</b>) 
                 with a <b>minimum</b> of <b>{min_number_of_contacts_per_case}</b> and a maximum of <b>{max_number_of_contacts_per_case}</b>" )
    
    HTML(paste(info, str1, sep = '<br/>'))
    
  }




# ~~~ all_contacts_tab_row_4_regional ----


total_contacts_per_link_type_donut_plot_regional <- 
  function(contacts_df_long){
    
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(prefecture == input$contacts_tab_select_regional)
    
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
             `Number of contacts` = n
      )
    
    if (nrow(data_to_plot) == 0) {
      return(highchart() %>% hc_title(text  = "No data to plot"))
    }
    
    
    data_to_plot %>%
      hchart("pie", hcaes(name = `Link with the case`, y = `Number of contacts` ),
             innerSize = "40%",
             name = "n",
             showInLegend = TRUE,
             dataLabels = list(enabled = TRUE,
                               style = list(fontSize = 12),
                               format = '{point.name}: {point.y}, ({point.percentage:.1f} %)'))  %>%
      hc_exporting(enabled = TRUE)
    
    
  }



total_contacts_per_link_type_text_regional <-
  function(contacts_df_long) {
    
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(prefecture == input$contacts_tab_select_regional)
    
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
    
    # 
    # second_link_type_with_most_contacts <-
    #   data_to_plot %>%
    #   .$`Link with the case` %>%
    #   .[2]
    # 
    # second_number_of_contacts_link_type_with_most_contacts <-
    #   data_to_plot %>%
    #   .$`Number of contacts` %>%
    #   .[2]
    # 
    # second_percent_link_type_with_most_contacts <-
    #   data_to_plot %>%
    #   .$Percent %>%
    #   .[2]
    # 
    
    
    info <- "<br>
            <span style='color: rgb(97, 189, 109);'>ℹ:</span>
            <font size='1'>
            The plots show the number of contacts per type of type of link. The categories have been cleaned and condensed.
            Access the data in tabular form by clicking on the top-right button.
            </font>"
    
    
    ### ! removed second most common category
    # str1 <-
    #   glue(
    #     "<br>The most common link category is <b>'{link_type_with_most_contacts}'</b>,
    #              with <b>{number_of_contacts_link_type_with_most_contacts}</b> contacts (<b>{percent_link_type_with_most_contacts}</b>),
    #              followed by <b>'{second_link_type_with_most_contacts}'</b>
    #              with <b>{second_number_of_contacts_link_type_with_most_contacts}</b> contacts (<b>{second_percent_link_type_with_most_contacts}</b>)
    #              "
    #   )
    
    str1 <-
      glue(
        "<br>The most common link category is <b>'{link_type_with_most_contacts}'</b>,
                 with <b>{number_of_contacts_link_type_with_most_contacts}</b> contacts (<b>{percent_link_type_with_most_contacts}</b>)
                 "
      )
    
    HTML(paste(info, str1, sep = '<br/>'))
    
  }



# ~~~ all_contacts_tab_row_5_regional ----

total_contacts_vaccinated_bar_plot_regional <-
  function(contacts_df_long) {
    
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(prefecture == input$contacts_tab_select_regional)
    
    
    data_to_plot <-
      contacts_df_long %>%
      group_by(row_id) %>%
      # slice long frame
      slice_head() %>%
      ungroup() %>%
      count(quartier, vaccination) %>%
      group_by(quartier) %>%
      mutate(prop = n / sum(n)) %>%
      ungroup() %>%
      complete(quartier, vaccination, fill = list(n = 0, prop = 0, sum_n  = 0)) %>%
      group_by(quartier) %>%
      mutate(sum_n = sum(n)) %>%  # for arranging
      ungroup() %>%
      # region with the most cases should be at the top
      arrange(-sum_n, quartier) %>%
      mutate(vaccination = recode(
        vaccination,
        "Vaccine" =  "Vacciné",
        "Non_vaccine" = "Non vacciné"
      )) %>%
      mutate(vaccination = factor(vaccination, levels = c("Vacciné", "Non vacciné"))) %>% 
      mutate(hc_data_label = ifelse(vaccination == "Vacciné", 
                                    glue("{n} of {sum_n} vaccinated ({percent(prop)})"), 
                                    NA_character_
      ))
    
    if (nrow(data_to_plot) == 0) {
      return(highchart() %>% hc_title(text  = "No data to plot"))
    }
    
    
    data_to_plot %>%
      hchart("column", hcaes(y = n, x = quartier, group = vaccination), 
             dataLabels = list(enabled = TRUE,formatter = JS("
                                                             function(){return(this.point.hc_data_label)}") 
             ))  %>%
      hc_yAxis(visible = TRUE) %>%
      hc_plotOptions(
        column = list(
          stacking = "normal",
          groupPadding = 0.1,
          borderWidth = 0.05,
          stickyTracking = T
        )
      ) %>%
      hc_plotOptions(column = list(states = list(inactive = list(opacity = 0.7)))) %>%
      hc_xAxis(title = list(text = "Quartier")) %>%
      hc_yAxis(title = list(text = "Nombre de contacts")) %>%
      hc_exporting(enabled = TRUE) %>% 
      hc_chart(inverted = TRUE)
    
    
  }



total_contacts_vaccinated_text_regional <-
  function(contacts_df_long) {
    
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(prefecture == input$contacts_tab_select_regional)
    
    data_to_plot <-
      contacts_df_long %>%
      group_by(row_id) %>%
      # slice long frame
      slice_head() %>%
      ungroup() %>%
      count(quartier, vaccination) %>%
      group_by(quartier) %>%
      mutate(prop = n / sum(n)) %>%
      ungroup() %>%
      complete(quartier, vaccination, fill = list(n = 0, prop = 0, sum_n  = 0)) %>%
      group_by(quartier) %>%
      mutate(sum_n = sum(n)) %>%  # for arranging
      ungroup() %>%
      # region with the most cases should be at the top
      arrange(-sum_n, quartier) %>%
      mutate(vaccination = recode(
        vaccination,
        "Vaccine" =  "Vacciné",
        "Non_vaccine" = "Non vacciné"
      )) %>%
      mutate(vaccination = factor(vaccination, levels = c("Vacciné", "Non vacciné"))) %>% 
      mutate(hc_data_label = ifelse(vaccination == "Vacciné", 
                                    glue("{n} of {sum_n} vaccinated ({percent(prop)})"), 
                                    NA_character_
      )) %>% 
      pivot_wider(quartier, vaccination, values_from = c(n, prop, sum_n, hc_data_label))
    
    
    
    quartier_1<- 
      data_to_plot %>% 
      .$quartier %>% 
      .[1]
    
    quartier_1_vaccination_coverage<- 
      data_to_plot %>% 
      .$prop_Vacciné %>% 
      .[1] %>% 
      percent()
    
    # commented out the below because one or two prefectures probably have only one quartier
    
    # quartier_2<- 
    #   data_to_plot %>% 
    #   .$quartier %>% 
    #   .[2]
    # 
    # quartier_2_vaccination_coverage<- 
    #   data_to_plot %>% 
    #   .$prop_Vacciné %>% 
    #   .[2] %>% 
    #   percent()
    
    info <- c("<br>
            <span style='color: rgb(97, 189, 109);'>ℹ:</span>
            <font size='1'>
            The plot shows the number and percentage of contacts vaccinated in each region.
            Access the data in tabular form by clicking on the top-right button.
            </font>")
    
    str1 <-
      glue(
        "<br><b>{quartier_1_vaccination_coverage}</b> of all contacts have been vaccinated in <b>{quartier_1}</b> 
        "
      )
    
    HTML(paste(info, str1, sep = '<br/>'))
    
  }



# ~~~ all_contacts_tab_row_6_regional ----


active_contacts_breakdown_bar_chart_regional <- 
  function(contacts_df_long){
    
    
    
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(prefecture == input$contacts_tab_select_regional)
    
    active_contacts <-
      contacts_df_long %>%
      group_by(row_id) %>% 
      ## keep active cases. 
      ## active cases whose last day of follow-up is today any day past today
      filter(max(follow_up_date) >= todays_date_reactive_regional()) %>%
      ungroup()
    
    colors <- 
      contacts_df_long %>% 
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
                          follow_up_date <= todays_date_reactive_regional(), 
                        NA_integer_, 
                        n)) %>% 
      mutate(prop = n/sum(n)) %>% 
      mutate(hc_ttip = glue("<b>Date:</b> {format.Date(follow_up_date,  format = '%b %d' )}
                        <b>{etat_suivi}:</b> {n}
                        ")) %>% 
      arrange(etat_suivi) %>%   # arranging is necessary so that that colors are pulled in the right order for highcharter
      left_join(colors)
    
    if (nrow(data_to_plot) == 0) {
      return(highchart() %>% hc_title(text  = "No data to plot"))
    }
    
    
    data_to_plot %>% 
      hchart("column", hcaes(x = follow_up_date, y = n, 
                             color = colors,
                             group = etat_suivi), 
             fillOpacity = 0.35) %>% 
      hc_colors(unique(data_to_plot$colors)) %>% 
      hc_plotOptions(column = list(series = list(fillOpacity = 0.1),
                                   marker = list(enabled = TRUE, 
                                                 radius = 3, 
                                                 symbol = "circle"),
                                   states = list(inactive = list(opacity = 0.1)), 
                                   stacking = "normal")) %>% 
      hc_tooltip(formatter = JS("function(){return(this.point.hc_ttip)}")) %>% 
      hc_xAxis(title = list(text = "Date")) %>% 
      hc_yAxis(title = list(text = "Number of contacts"))
    
    
  }

active_contacts_breakdown_table_regional <- 
  function(contacts_df_long){
    
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(prefecture == input$contacts_tab_select_regional)
    
    active_contacts <-
      contacts_df_long %>%
      group_by(row_id) %>% 
      ## keep active cases. 
      ## active cases whose last day of follow-up is today any day past today
      filter(max(follow_up_date) >= todays_date_reactive_regional()) %>%
      ungroup()
    
    colors <- 
      contacts_df_long %>% 
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
                          follow_up_date <= todays_date_reactive_regional(), 
                        NA_integer_, 
                        n)) %>% 
      mutate(prop = n/sum(n)) %>% 
      mutate(hc_ttip = glue("<b>Date:</b> {format.Date(follow_up_date,  format = '%b %d')}
                        <b>{etat_suivi}:</b> {n}
                        ")) %>% 
      arrange(etat_suivi) %>%   # arranging is necessary so that that colors are pulled in the right order for highcharter
      left_join(colors)
    
    
    data_to_plot %>% 
      select(1:3) %>% 
      pivot_wider(names_from = etat_suivi, values_from = n) %>% 
      select(Date = follow_up_date, everything()) %>% 
      reactable(searchable = TRUE,
                striped = TRUE,
                highlight = TRUE,
                theme = reactableTheme(stripedColor = "#f0f1fc",
                                       highlightColor = "#DADEFB",
                                       cellPadding = "8px 12px",
                                       style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif")
                )
      )
    
    
  }


# ~~~ all_contacts_tab_row_7_regional ----


contacts_lost_24_to_72_hours_regional <- 
  function(contacts_df_long){
    
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(prefecture == input$contacts_tab_select_regional)
    
    
    if((contacts_df_long %>%
       filter(follow_up_date == todays_date_reactive_regional()) %>% 
       nrow()) == 0){
      return(
        "No data to show" %>% 
          data.frame() %>% 
          gt() %>% 
          fmt_markdown(columns = 1) %>%
          tab_options(column_labels.hidden = T)
      )
    }
      
    
    vu_non_vu_today <-
      contacts_df_long %>%
      filter(follow_up_date == todays_date_reactive_regional()) %>% 
      group_by(quartier) %>% 
      count(etat_suivi_simple) %>% 
      ungroup() %>% 
      pivot_wider(names_from = etat_suivi_simple, values_from = n) %>% 
      clean_names() %>% 
      mutate(total = non_vu + vu) %>% 
      mutate(pct_non_vu = percent(non_vu/ total, accuracy = 0.1)) %>% 
      select(Quartier = quartier, 
             Total = total, 
             `Not seen` = non_vu, 
             `% Not seen` = pct_non_vu) %>% 
      rename_with(.cols = c(Total, `Not seen`, `% Not seen`), 
                  .fn = ~ paste0("Today.", .x))
    
    
    vu_non_vu_past_two_days <- 
      contacts_df_long %>%
      filter(follow_up_date >= todays_date_reactive_regional() - 1) %>% 
      group_by(id_contact) %>% 
      mutate(etat_suivi_simple = ifelse( any(etat_suivi_simple == "Vu"),
                                         "Vu",
                                         "Non vu")) %>% 
      slice_head(n = 1) %>% 
      ungroup() %>% 
      group_by(quartier) %>% 
      count(etat_suivi_simple) %>% 
      ungroup() %>% 
      pivot_wider(names_from = etat_suivi_simple, values_from = n) %>% 
      clean_names() %>% 
      mutate(total = non_vu + vu) %>% 
      mutate(pct_non_vu = percent(non_vu/ total, accuracy = 0.1)) %>% 
      select(Quartier = quartier, 
             Total = total, 
             `Not seen` = non_vu, 
             `% Not seen` = pct_non_vu) %>% 
      rename_with(.cols = c(Total, `Not seen`, `% Not seen`),
                  .fn = ~ paste0("Past 2d.", .x))
    
    
    
    vu_non_vu_past_three_days <- 
      contacts_df_long %>%
      filter(follow_up_date >= todays_date_reactive_regional() - 2) %>% 
      group_by(id_contact) %>% 
      mutate(etat_suivi_simple = ifelse( any(etat_suivi_simple == "Vu"),
                                         "Vu",
                                         "Non vu")) %>% 
      slice_head(n = 1) %>% 
      ungroup() %>% 
      group_by(quartier) %>% 
      count(etat_suivi_simple) %>% 
      ungroup() %>% 
      pivot_wider(names_from = etat_suivi_simple, values_from = n) %>% 
      clean_names() %>% 
      mutate(total = non_vu + vu) %>% 
      mutate(pct_non_vu = percent(non_vu/ total, accuracy = 0.1)) %>% 
      select(Quartier = quartier, 
             Total = total, 
             `Not seen` = non_vu, 
             `% Not seen` = pct_non_vu) %>% 
      rename_with(.cols = c(Total, `Not seen`, `% Not seen`),
                  .fn = ~ paste0("Past 3d.", .x))
    
    
    # the table
    data_to_plot <- vu_non_vu_today %>% 
      left_join(vu_non_vu_past_two_days) %>% 
      left_join(vu_non_vu_past_three_days)
    
    
    if(is.null(data_to_plot) | nrow(data_to_plot) == 0){
      return(
        "No data to show" %>% 
          data.frame() %>% 
          gt() %>% 
          fmt_markdown(columns = 1) %>%
          tab_options(column_labels.hidden = T)
      )
    }
    
    
    data_to_plot %>% 
      gt() %>% 
      fmt_missing(columns = everything(), missing_text = "-") %>% 
      data_color(columns = "Past 3d.Not seen", 
                 colors = scales::col_numeric(palette = paletteer_d(palette = "ggsci::red_material") %>% as.character(), 
                                              domain = c(1, max(vu_non_vu_past_three_days$`Past 3d.Not seen`) * 3)
                 ),
                 autocolor_text = F ) %>% 
      tab_spanner_delim(delim = ".") %>%
      tab_style(locations = cells_column_labels(columns = everything()),
                style = list(cell_borders(sides = "bottom", weight = px(3)),
                             cell_text(weight = "bold"))) %>%
      tab_style(locations = cells_column_spanners(spanners = everything()),
                style = list(cell_text(weight = "bold", size = "large"))) %>%
      opt_row_striping() %>% 
      tab_source_note(source_note = md("*Not seen in past 2/3 days means not seen on **any** of the past 2/3 days"))
    
    
  }




lost_contacts_linelist_regional <- 
  function(contacts_df_long){
    
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(prefecture == input$contacts_tab_select_regional)
    
    
    non_vu_past_three_days <- 
      contacts_df_long %>%
      filter(follow_up_date >= todays_date_reactive_regional() - 2) %>% 
      group_by(id_contact) %>% 
      mutate(etat_suivi_simple = ifelse( any(etat_suivi_simple == "Vu"),
                                         "Vu",
                                         "Non vu")) %>% 
      slice_head(n = 1) %>% 
      ungroup() %>% 
      filter(etat_suivi_simple == "Non vu")
    
    data_to_tabulate <- contacts_df_long %>%
      filter(id_contact %in% non_vu_past_three_days$id_contact) %>% 
      group_by(id_contact) %>% 
      mutate(etat_suivi_simple = ifelse( any(etat_suivi_simple == "Vu"),
                                         "Vu",
                                         "Non vu")) %>% 
      ungroup() %>% 
      filter(etat_suivi_simple == "Non vu") %>% 
      select(ID = id_contact, 
             Quartier = quartier, 
             Dates = follow_up_date, 
             `Follow-up States` = etat_suivi) %>% 
      group_by(ID) %>% 
      arrange(Dates) %>% 
      ungroup() 
    
    data_to_tabulate %>% 
      reactable(groupBy = "ID", 
                columns = list(`Follow-up States` = colDef(aggregate = "frequency"), 
                               Quartier = colDef(aggregate = "unique")
                ),
                searchable = TRUE,
                striped = TRUE,
                highlight = TRUE,
                theme = reactableTheme(stripedColor = "#f0f1fc",
                                       highlightColor = "#DADEFB",
                                       cellPadding = "8px 12px",
                                       style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif")
                )
      )
    
    
  }





# ~~~ all_contacts_tab_row_8_regional ----

contacts_timeline_snake_plot_regional <- 
  function(contacts_df_long){
    
    contacts_df_long <- 
      contacts_df_long %>% 
      filter(prefecture == input$contacts_tab_select_regional)
    
    
    number_to_sample <- 50
    
    data_to_plot <-
      contacts_df_long %>%
      filter(row_id %in% c(input$snake_plot_slider_regional[1]:input$snake_plot_slider_regional[2]) ) %>% 
      group_by(row_id) %>% 
      # sample
      {if (length(unique(.$row_id)) > number_to_sample) {
        filter(., row_id %in% sample(unique(.$row_id), number_to_sample, replace = F))
      } else .} %>%
      mutate(hc_ttip = glue("<b>ID: </b> {id_contact} <br>
                         <b>Sous-prefecture: </b> {sous_prefecture}<br>
                         <b>Date: </b> {format.Date(follow_up_date, format = '%b %d')} (Jour de suivi {follow_up_day}) <br>
                         <b>Status:</b> {etat_suivi}
                         ")) %>% 
      ungroup() %>% 
      arrange(etat_suivi) %>%   # arranging is necessary so that that colors are pulled in the right order for highcharter
      mutate(follow_up_date_timestamp = datetime_to_timestamp(follow_up_date))
    
    
    if (nrow(data_to_plot) == 0) {
      return(highchart() %>% hc_title(text  = "No data to plot"))
    }
    
    data_to_plot %>% 
      hchart("scatter", 
             hcaes(x = follow_up_date, y = row_id, 
                   color = colors, group = etat_suivi)) %>% 
      hc_colors(unique(data_to_plot$colors)) %>% 
      hc_plotOptions(series = list(marker = list(radius = 3, 
                                                 symbol = "circle"))) %>% 
      hc_xAxis(title = list(text = "Date"),
               min = min(data_to_plot$follow_up_date, na.rm = T)  %>% datetime_to_timestamp(), 
               max = max(data_to_plot$follow_up_date, na.rm = T) %>% datetime_to_timestamp()
      ) %>% 
      hc_yAxis(title = list(text = "Row ID"),
               min = min(data_to_plot$row_id, na.rm = T), 
               max = max(data_to_plot$row_id, na.rm = T)) %>% 
      hc_tooltip(formatter = JS("function(){return(this.point.hc_ttip)}")) %>% 
      hc_plotOptions(scatter = list(stickyTracking = F)) %>% 
      hc_exporting(enabled = TRUE) %>% 
      hc_boost(enabled = TRUE) %>% 
      hc_size(height = 600)
    
  }


















