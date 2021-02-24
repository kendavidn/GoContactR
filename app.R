#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Packages ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(jsonlite)
library(here)
library(tidytext)
library(paletteer)
library(highcharter)
library(shiny)
library(shinyWidgets)
library(styler)
library(shinydashboard)
library(shinydashboardPlus)
library(here)
library(shinycssloaders)
library(DT)
library(fresh)
library(shinyr)
library(ggtext)


library(data.table)
library(leaflet)
library(leaflet.minicharts)
library(echarts4r)
library(sparkline)
library(shinyBS)




#
#library(shinydashboardPlus)
#
library(rwhatsapp)
library(highcharter)
library(waiter)
library(tidyverse)
library(paletteer)
library(visdat)
library(charlatan)
library(randomNames)

options(scipen=999) # turn off scientific notation

set.seed(1) # fix seed

options(tibble.print_max = 35, tibble.print_min = 35)







# options(highcharter.theme = hc_theme_hcrt())
# 
# newtheme <- hc_theme_merge(
#   getOption("highcharter.theme"), 
#   hc_theme(chart = list(backgroundColor = "transparent"), 
#            colors = 
#              c("#332859",
#                (paletteer_d("ggsci::nrc_npg") %>% 
#                   as.character() %>% 
#                   str_sub(1,7))
#              ))
# )
# options(highcharter.theme = newtheme)

# hchart(cars, "scatter", hcaes(speed, dist))


PARS <- list(
  debug = FALSE,
  classcol = "col-lg-offset-1 col-lg-10 col-md-offset-0 col-md-12 col-sm-offset-0 col-sm-12",
  sparkline_color = "#333333",
  font = '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol"'
)
options(
  highcharter.google_fonts = FALSE,
  highcharter.debug = PARS$debug,
  # shiny.launch.browser = PARS$debug,
  highcharter.theme = 
    hc_theme_smpl(
      title = list(style = list(fontSize = "1.2em", fontFamily = PARS$font)),
      subtitle = list(style = list(fontFamily = PARS$font, fontSize = "0.95em")),
      chart = list(
        backgroundColor = "transparent",
        style = list(fontFamily = PARS$font, fontSize = "1.0em")
      ),
      plotOptions = list(
        series = list(
          dataLabels = list(color = "#222d32", style = list(fontWeight = "normal", textShadow = FALSE, textOutline = FALSE)),
          animation = list(duration = 800)
        )
      ),
      legend = list(
        itemStyle =  list(
          fontWeight = "normal"
        )
      )
    )
)

newtheme <- hc_theme_merge(
  getOption("highcharter.theme"),
  hc_theme(chart = list(backgroundColor = "transparent"),
           colors =
             c("#332859",
               (paletteer_d("ggsci::nrc_npg") %>%
                  as.character() %>%
                  str_sub(1,7))
             ))
)
options(highcharter.theme = newtheme)



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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Server ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(here)
source(here("ui.R"))
source(here("server.R"))

# Run the application
shinyApp(ui = ui, server = server)



