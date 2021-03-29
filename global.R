#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Packages ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#library(polished)
# 
# polished::global_sessions_config(
#   app_name = "CovContactR",
#   api_key = "ikHFLT2t4R1gcmXYHWjT2372QPA2v1JQd7"
# )


library(here)
library(paletteer)
library(highcharter)
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(shinycssloaders)
library(fresh)
library(reactable)
library(reactablefmtr)
library(magrittr)
library(lubridate)
library(reactable)
library(inspectdf)
library(waiter)
library(visdat)
library(here)
library(stringi)
library(anytime)
library(glue)
library(janitor)
library(scales)
library(gt)
library(linelist)
library(rio)


library(tidyverse)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Options ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

options(scipen = 999) # turn off scientific notation

set.seed(1) # fix seed

options(tibble.print_max = 35, tibble.print_min = 35)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~   FUNCTIONS ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source(here("helper_scripts/misc_functions.R"), local = T)

col2hex <- function(col, alpha) rgb(t(col2rgb(col)), 
                                    alpha=alpha, maxColorValue=255)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Read in preloaded datasets ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


cleaning_rules <- rio::import(here("data", "cleaning_rules_contacts.xlsx"))


contacts_df_raw <-
  rio::import(here("data/Base contacts_GUINEE_2021_03_14_GLOBAL.xlsx"))  %>%
  clean_names() %>%
  type_convert()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~  cleaning rules ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


cleaning_rules <-
  rio::import(here("data", "cleaning_rules_contacts.xlsx"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~ sample_contacts_df_raw ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

#! Not used at the moment March 26
sample_contacts_df_raw <-
  rio::import(here("data/sample_contacts_df_raw.xlsx"))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~  preloaded_data_options ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# called by data_input UI element on page 1 of app
preloaded_data_options <-
  list(`Guinea list 03_14` = contacts_df_raw)

# 
# todays_date <- as.Date("2021-03-14")
# 
# app_mode <- "manual"
# 
# if (app_mode == "manual"){
#   
#   todays_date_reactive <- function(){
#     
#     as.Date("2021-03-14")
#     
#   }
# 
# 
#   # data in
#   contacts_df <-
#     contacts_df_raw %>%
#     # clean vaccination column, type_de_contact, unite_age, prefecture, lien_avec_las_cas. 
#     # No etat_suivi column to clean yet because I have not created it
#     linelist::clean_variable_spelling(wordlists = cleaning_rules) %>%
#     # drop records that should not exist based on faux date
#     filter(date_du_dernier_contact <= todays_date_reactive()) %>%
#     # add counter column. 1 for all records
#     mutate(counter = 1) %>%
#     # row numbers to match Excel spreadsheet
#     mutate(row_id = row_number() + 3) %>%
#     # convert dates to dates
#     mutate(across(.cols = matches("date_"),
#                   .fns = ~ as.Date(.x))) %>%
#     # rename the follow_up_day columns
#     rename_with(.cols = paste0("j", 1:21),
#                 .fn = ~ paste("follow_up_day", str_remove_all(.x, "j"), sep = "_")) %>%
#     # duplicate the follow_up_day columns, change name to follow_up_date
#     mutate(across(.cols = paste("follow_up_day", 1:21, sep = "_"),
#                   .fns = ~ .,
#                   .names = "follow_up_date_{str_remove_all(.col, 'follow_up_day_')}")) %>%
#     # now, we convert the numbers in the follow up day columns
#     # to reflect the nth day of follow up
#     mutate(across(.cols = starts_with("follow_up_date_"),
#                   .fn = ~ str_extract(cur_column(), paste(21:1, collapse = "|")) %>% as.numeric() )) %>% # start from 21 otherwise you'll remove the tens unit before you get to the teens
#     # clean vaccination column
#     mutate(across(vaccination,
#                   ~ .x %>%
#                     stri_trans_general("Latin-ASCII") %>%
#                     str_to_lower() %>%
#                     str_to_sentence())) %>%
#     # clean admin levels
#     mutate(across(c(prefecture, sous_prefecture, quartier), 
#                   ~ .x %>%
#                     str_to_sentence() %>% 
#                     str_trim() %>% 
#                     str_replace_all("  ", " ")))
# 
# 
#   
#   pivot_day <- 
#     contacts_df %>% 
#     select(row_id, paste0("follow_up_day_", 1:21)) %>% 
#     pivot_longer(cols = c(paste0("follow_up_day_", 1:21)), 
#                  names_to = "follow_up_day", 
#                  values_to = "etat_suivi") %>% 
#     # clean "etat de suivi" column
#     linelist::clean_variable_spelling(wordlists = cleaning_rules)
#   
#   
#   legend_df <- 
#     tribble(    
#       ~breaks ,                  ~colors,                   
#       "Non vu",                  col2hex("orangered"),                  
#       "Vu",                      col2hex("lightseagreen"),  
#       "Cas confirmé",            col2hex("purple3"),     
#       "Cas suspect",             col2hex("yellow"),       
#       "Manquant",                col2hex("pink4"),          
#       "Deplacé",                 col2hex("wheat3"),  
#       "Recyclé",                 col2hex("yellow4"),          
#       "Fin de suivi",            col2hex("dodgerblue3"),    
#       "Suivi futur",             col2hex("goldenrod"),      
#       "Données manquantes",      col2hex("black"),          
#       "Doublon",                 col2hex("slategray3"),          
#     ) %>% 
#     arrange(breaks) %>% 
#     mutate(breaks = fct_inorder(breaks)) %>% 
#     mutate(legend_index = row_number())
# 
# 
#   
#   contacts_df_long <- 
#     contacts_df %>%
#     inner_join(pivot_day, by = "row_id") %>%
#     # paste the day of followup
#     mutate(follow_up_day = str_extract(follow_up_day, paste(21:1, collapse = "|"))) %>%
#     mutate(follow_up_day = as.numeric(follow_up_day)) %>%
#     # assume that follow up begins from date of last interaction
#     mutate(follow_up_date = follow_up_day + date_du_dernier_contact) %>%
#     mutate(etat_suivi = as.character(etat_suivi)) %>%
#     # change status of records that should be still active
#     mutate(etat_suivi = if_else(follow_up_date > todays_date_reactive(),
#                                 "Suivi futur",
#                                 etat_suivi
#     )) %>%
#     mutate(etat_suivi = replace_na(etat_suivi, "Données manquantes")) %>% 
#     # - if follow-up lasted the full 21 days,
#     # - change last follow_up state to "Fin de suivi"
#     mutate(etat_suivi = if_else(follow_up_day == 21 & etat_suivi == "Vu",
#                                 "Fin de suivi",
#                                 etat_suivi)) %>% 
#     # add legend colors
#     left_join(legend_df, by = c("etat_suivi" = "breaks")) %>% 
#     # change records to "Suivi Futur" if ahead of today's date
#     mutate(etat_suivi = if_else(follow_up_date > todays_date_reactive(),
#                                 "Suivi futur",
#                                 etat_suivi
#     ))  %>% 
#     mutate(etat_suivi_simple = etat_suivi) %>% 
#     mutate(etat_suivi_simple = recode(etat_suivi_simple,
#                                       "Vu" = "Vu",
#                                       "Non vu" = "Non vu",
#                                       "Cas suspect" = "Vu",
#                                       "Decedé" = "Vu",
#                                       "Deplacé" = "Non vu",
#                                       "Cas confirmé" = "Vu",
#                                       "Transferé" = "Vu",
#                                       "Refus" = "Non vu",
#                                       "Reco. pas passé" = "Non vu",
#                                       "Doublon" = "Doublon",
#                                       "Recyclé" = "Vu", 
#                                       "Fin de suivi" = "Vu", 
#                                       "Données manquantes" = "Non vu"
#     )) %>% 
#     filter(etat_suivi != "Doublon")
#   
# 
#   # send function of output to global environment.
#   # All data-plotting reactives wait for this object to appear in the global environment before executing
#   read_file_out <- list(contacts_df_raw = contacts_df_raw,
#                          contacts_df = contacts_df,
#                          contacts_df_long = contacts_df_long)
# 
# 
# }




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~ Colors ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


space_cadet <- "#1C2541"
burnt_sienna <- "#EE6C4D"
languid_lavender <- "#E8D7F1"
ghost_white <- "#f0f2fa"
magnolia <- "#faf5fc"
beau_blue <- "#CFE9FF"
bittersweet <- "#F07167"
white <- "#FFFFFF"
alice_blue <- "#D9F0FE"
yale_blue <- "#054a91"
who_blue <- "#158BC6"
light_gray <- "#ebebeb"
russian_violet <- "#332859"
cinnabar <- "#E64B35"
coral <- "#ff7a4a"
peach <- "#FCDC9C"
bright_yellow_crayola <- "#FFB238"


final_palette <-  c(
  "#332859",
  (
    paletteer_d("ggsci::nrc_npg") %>%
      as.character() %>%
      str_sub(1, 7)
  )
)


my_fresh_theme <- 
  fresh::create_theme(
    fresh::adminlte_color(
      light_blue = white
    ),
    fresh::adminlte_sidebar(
      dark_bg = space_cadet,
      dark_hover_bg = cinnabar,
      dark_color = alice_blue
    ),
    fresh::adminlte_global(
      content_bg = white,
      box_bg = white, 
      info_box_bg = light_gray
    )
  )

spinner_color <- burnt_sienna <- "#EE6C4D" 



legend_df <- 
  tribble(     
    ~breaks ,                  ~colors,                        ~shapes,
    "Not followed",            cinnabar,                        20,
    "Followed",                col2hex("lightseagreen"),         20,
    "Has tested positive",     col2hex("chocolate3"),            25,
    "Has tested negative",     col2hex("greenyellow"),          25,
    "Developed symptoms",      col2hex("magenta4"),             22,
    "Lost to follow-up",       col2hex("pink4"),               22,
    "Completed",               col2hex("dodgerblue3"),         19,
    "Upcoming follow-up",      col2hex("goldenrod"),           1,
    "Missing status",          col2hex("black"),               63
  ) %>% 
  arrange(breaks) %>% 
  mutate(breaks = fct_inorder(breaks)) %>% 
  mutate(legend_index = row_number())


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~ highcharter themes ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


options(highcharter.theme = hc_theme_hcrt())


myMenuItems <-
  c(
    "printChart",
    "separator",
    "downloadPNG",
    "downloadJPEG",
    "downloadPDF",
    "downloadSVG",
    "separator",
    "viewData",
    "downloadCSV",
    "downloadXLS"
  )

newtheme <-
  hc_theme_merge(
    getOption("highcharter.theme"),
    hc_theme(
      chart = list(
        backgroundColor = "transparent",
        zoomType = "xy",
        panning = list(enabled = TRUE, type = "xy"),
        panKey = "shift"
      ),
      colors = final_palette,
      exporting = list(buttons = list(contextButton = list(menuItems = myMenuItems)))
    )
  )

options(highcharter.theme = newtheme)

options(highcharter.color_palette = final_palette)

# ~~~ highcharter sparkline theme ----

hc_theme_sparkline_vb <- function(...) {
  
  theme <- list(
    chart = list(
      backgroundColor = NULL,
      margins = c(0, 0, 0, 0),
      spacingTop = 0,
      spacingRight = 0,
      spacingBottom = 0,
      spacingLeft = 0,
      plotBorderWidth = 0,
      borderWidth = 0,
      style = list(overflow = "visible")
    ),
    xAxis = list(
      visible = FALSE, 
      endOnTick = FALSE, 
      startOnTick = FALSE
    ),
    yAxis = list(
      visible = FALSE,
      endOnTick = FALSE, 
      startOnTick = FALSE
    ),
    tooltip = list(
      outside = FALSE,
      shadow = FALSE,
      borderColor = "transparent",
      botderWidth = 0,
      backgroundColor = "transparent",
      style = list(textOutline = "5px white")
    ),
    plotOptions = list(
      series = list(
        marker = list(enabled = FALSE),
        lineWidth = 2,
        shadow = FALSE,
        fillOpacity = 0.25,
        color = "#FFFFFFBF",
        fillColor = list(
          linearGradient = list(x1 = 0, y1 = 1, x2 = 0, y2 = 0),
          stops = list(
            list(0.00, "#FFFFFF00"),
            list(0.50, "#FFFFFF7F"),
            list(1.00, "#FFFFFFFF")
          )
        )
      )
    ),
    credits = list(
      enabled = FALSE,
      text = ""
    )
  )
  
  theme <- structure(theme, class = "hc_theme")
  
  if (length(list(...)) > 0) {
    theme <- hc_theme_merge(
      theme,
      hc_theme(...)
    )
  }
  
  theme
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~ ggplot2 theme ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

my_theme <- theme_classic() + 
  theme(
    plot.title = element_text(face = "bold"),
    #plot.background = element_rect(fill = "gray93"),
    panel.grid.major = element_line(color = "gray95", size = 0.2),
    strip.background = element_blank(),
    # element textbox is from ggtext
    strip.text = ggtext::element_textbox(
      size = 11, face = "bold",
      color = "white", fill = "steelblue3", halign = 0.5, 
      r = unit(5, "pt"), width = unit(1, "npc"),
      padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)
    )
  )

theme_set(my_theme)
