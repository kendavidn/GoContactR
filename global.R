
PARAMS <- list()
PARAMS$testing_mode <- FALSE
PARAMS$fake_data <- TRUE
#PARAMS$country_code <- "UGA"
PARAMS$country_code <- "UGA"
## App currently built for CIV and UGA
## CIV & UGA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~  Packages ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# library(polished)
#
# polished::global_sessions_config(
#   app_name = "CovContactR",
#   api_key = "ikHFLT2t4R1gcmXYHWjT2372QPA2v1JQd7"
# )




library("remotes")

library(webshot)

if (!webshot::is_phantomjs_installed()) webshot::install_phantomjs()
library(here)
library(paletteer)
library(highcharter) ## remotes::install_github("jbkunst/highcharter")
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
library(inspectdf)
library(visdat)
library(here)
library(echarts4r)
library(stringi)
library(anytime)
library(glue)
library(janitor)
library(scales)
library(gt)
library(gtools)
library(linelist) ##  devtools::install_github("reconhub/linelist")
library(rvest) ## devtools::install_github("tidyverse/rvest")
library(pander)
library(rio)
library(huxtable)
library(flextable) # called by huxtable in some functions
library(rmarkdown)
library(officedown) # remotes::install_github("davidgohel/officedown", force = TRUE)
library(pagedreport)
library(promises)
library(clock)
library(plotly)
library(charlatan)

## for godata
library(httr)
library(jsonlite)

## last to avoid masking
library(tidyverse)




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~  Options ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

options(scipen = 999) # turn off scientific notation

set.seed(1) # fix seed

options(tibble.print_max = 35, tibble.print_min = 35)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~   FUNCTIONS ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source(here("helper_scripts/misc_functions.R"), local = T)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~  Read in preloaded datasets ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# cleaning_rules <- rio::import(here("data", "cleaning_rules_contacts.xlsx"))

# contacts_df_raw <-
#   rio::import(here("data/Base contacts_GUINEE_2021_03_14_GLOBAL.xlsx"))  %>%
#   clean_names() %>%
#   type_convert()

# ! Not used at the moment March 26
contacts_list_sample <-
  rio::import(here("data/liste_contacts_sample.xlsx"))

follow_up_list_sample <-
  rio::import(here("data/suivi_contacts_sample.xlsx"))

tracing_data_sample <- list(
  contacts_list = contacts_list_sample,
  follow_up_list = follow_up_list_sample
)

# called by data_input UI element on page 1 of app
preloaded_data_options <-
  list(`Sample tracing data` = tracing_data_sample)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~ Colors ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


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
spinner_color <- burnt_sienna <- "#EE6C4D"




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




legend_df <-
  tribble(
    ~breaks, ~colors,
    "Manquant", col2hex("black"),
    "Poursuite du suivi", col2hex("lightseagreen"),
    "Symptomatique, resultats attendus", col2hex("lightpink2"),
    "Devenu cas confirme", col2hex("orangered"),
    "Sorti sain", col2hex("darkolivegreen4"),
    "Deplacé", col2hex("wheat3"),
    "Not generated", col2hex("wheat4"),
    "Fin du suivi", col2hex("dodgerblue3"),
    "Suivi futur", col2hex("goldenrod"),
    "Decede", col2hex("purple3")
  ) %>%
  arrange(breaks) %>%
  mutate(breaks = fct_inorder(breaks)) %>%
  mutate(legend_index = row_number())


## Uganda version
legend_df <-
  tribble(
    ~breaks, ~colors,
    "Missed", col2hex("gray50"),
    "Missing", col2hex("black"),
    "Seen, Ok", col2hex("lightseagreen"),
    "Seen, Not Ok", col2hex("orangered"),
    "Not attempted", col2hex("wheat3"),
    "Not performed", col2hex("blueviolet"),
    "Not generated", col2hex("purple3"),
    "End of follow-up", col2hex("dodgerblue3"),
    "Future follow-up", col2hex("goldenrod")
  ) %>%
  arrange(breaks) %>%
  mutate(breaks = fct_inorder(breaks)) %>%
  mutate(legend_index = row_number())


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~ highcharter themes ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


highcharter_palette_initial <- c(
  "#332859",
  (
    paletteer_d("ggsci::nrc_npg") %>%
      as.character() %>%
      str_sub(1, 7)
  )
)
# for graphs with many categories
ramped_colors <- colorRampPalette(highcharter_palette_initial)(10)

# remove first and last colors from ramp, because these are the initial colors

new_colors <-
  ramped_colors[-c(1, length(ramped_colors))] %>%
  rev()

highcharter_palette <- c(highcharter_palette_initial, new_colors)


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
      colors = highcharter_palette,
      labels = list(style = list(lineHeight = "100px")),
      plotOptions = list(series = list(label = list(style = list(lineHeight = "100px")))),
      exporting = list(buttons = list(contextButton = list(menuItems = myMenuItems)))
    )
  )

options(highcharter.theme = newtheme)


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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~  Plotly theme ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

font <- list(
  family = "Avenir",
  size = 15,
  color = "white"
)

label <- list(
  bordercolor = "transparent",
  font = font
)

## to be passed to plotly object like so:
# object %>%
#   gplotly() %>%
#   style(hoverlabel = label) %>%
#   layout(font = font)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~ ggplot2 theme ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

my_theme <- theme_classic() +
  theme(
    plot.title = element_text(face = "bold"),
    # plot.background = element_rect(fill = "gray93"),
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
