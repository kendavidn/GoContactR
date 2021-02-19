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
library(here)
library(shinycssloaders)
library(DT)
library(fresh)
library(shinydashboardPlus)
library(shinyr)




#
#library(shinydashboardPlus)
#
library(rwhatsapp)
library(highcharter)
library(waiter)
library(tidyverse)
library(paletteer)
library(visdat)



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



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Server ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(here)
source(here("ui.R"))
source(here("server.R"))

# Run the application
shinyApp(ui = ui, server = server)



