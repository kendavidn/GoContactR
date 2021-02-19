space_cadet <- "#1C2541"
burnt_sienna <- "#EE6C4D"
languid_lavender <- "E8D7F1"

my_fresh_theme <- create_theme(
  adminlte_color(
    light_blue = space_cadet
  ),
  adminlte_sidebar(
    dark_bg = space_cadet,
    dark_hover_bg = burnt_sienna,
    dark_color = languid_lavender
  ),
  adminlte_global(
    content_bg = space_cadet,
    box_bg = languid_lavender, 
    info_box_bg = languid_lavender
  )
)

spinner_color <- burnt_sienna <- "#EE6C4D"



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~ ui ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


header <- 
  dashboardHeader(title = "WHO Logo here")

sidebar <- 
  dashboardSidebar(sidebarUserPanel("User Name",
                                    subtitle = a(href = "#", icon("circle", class = "text-success"), "Online"),
                                    # Image file should be in www/ subdir
                                    #image = "userimage.png"
                                    ),
                   sidebarMenu(id = "tabs", # Setting id makes input$tabs give the tabName of currently-selected tab
                               menuItem("Load data", tabName = "load_data", icon = icon("table")),
                               menuItem("Overview",  tabName = "overview", icon = icon("dashboard")),
                               menuItem("Charts", icon = icon("bar-chart-o"),
                                        menuSubItem("Sub-item 1", tabName = "subitem1"),
                                        menuSubItem("Sub-item 2", tabName = "subitem2")
                                        )
                               )
                   )
# sidebar <- 
#   dashboardSidebar(
#   uiOutput("data_to_use"))

#~~ Body ---------------------------

load_data_tab_row_1 <- 
  fluidRow(column(width = 3,
                  box(width = NULL, 
                      div(style = "overflow-x: scroll; ",
                          HTML("Prototype R Shiny App to analyse contact tracing data for the Niger dataset: Contact the WHO team <a href='https://www.afro.who.int/about-us/contact-us' target= '_blank' >here</a> or email us at <a href='mailto:afrgocom@who.int' target= '_blank' >afrgocom@who.int</a> for enquiries.")))),
           column(width = 1,
                  dropdownButton(tags$h4("Control Outputs"),
                                 "Will add options to subset by sender, dates",
                                 icon = icon("gear"), status = "info"),
                  dropdownButton(tags$h4("Control Outputs"),
                                 "will add information about non-obvious text analysis decisions",
                                 icon = icon("info"), status = "info")))


load_data_tab_row_2 <- 
  fluidRow(column(width = 12, 
                  withSpinner(type = 6, color = burnt_sienna,
                              plotOutput("data_completeness_plot")
                  )
  )
  )

overview_tab_row_1 <- 
  fluidRow(column(width = 4, 
                  withSpinner(type = 6, color = burnt_sienna,
                              highchartOutput("sex_count_donut_plot1"))),
           column(width = 4, 
                  withSpinner(type = 6, color = burnt_sienna,
                              highchartOutput("sex_count_donut_plot2"))),
           column(width = 4, 
                  withSpinner(type = 6, color = burnt_sienna,
                              highchartOutput("sex_count_donut_plot3")))
  )

overview_tab_row_2 <-
  fluidRow(column(width = 8,
                  style = "height:600px",
                  withSpinner(type = 6, color = burnt_sienna,
                              highchartOutput("region_count_sunburst_plot", height = "600px"))), 
           column(width = 4,
                  style = "height:600px",
                  withSpinner(type = 6, color = burnt_sienna,
                              dataTableOutput("contacts_df_table")))
  )

body <-  
  dashboardBody(use_waiter(), # include dependencies
                use_theme(my_fresh_theme), # <-- use the theme
                setBackgroundImage(src = "https://www.fonewalls.com/wp-content/uploads/Lavender-Gradient-Wallpaper.jpg",
                                   shinydashboard = TRUE),
                tabItems(tabItem("load_data", 
                                 load_data_tab_row_1, 
                                 load_data_tab_row_2), 
                         tabItem("overview", 
                                 overview_tab_row_1,
                                 overview_tab_row_2
                                 
                                 
                         )
                )
  )

# controlbar <-
#   dashboardControlbar(id = "controlbar",
#                       .list = list(
#                       uiOutput("data_to_use"), 
#                       uiOutput("input_data"), 
#                       uiOutput("columns_to_analyze"), 
#                       uiOutput("analyze_action_bttn"), 
#                       uiOutput("filters")
#                       )
#                       
#                       )

controlbar <-
  dashboardControlbar(id = "controlbar",
                      controlbarMenu(
                        controlbarItem(title = "Controls",
                                       uiOutput("data_to_use"), 
                                       uiOutput("input_data"), 
                                       uiOutput("columns_to_analyze"), 
                                       uiOutput("analyze_action_bttn"), 
                                       uiOutput("filters")
                          
                        )
                      )
                      
                      
  )

footer <- 
  dashboardFooter()
  

#~~ Combine ---------------------------

ui <- 
   dashboardPage(header = header, 
                sidebar = sidebar, 
                body= body, 
                controlbar = controlbar, 
                footer = footer)
