space_cadet <- "#1C2541"
burnt_sienna <- "#EE6C4D"
languid_lavender <- "E8D7F1"
beau_blue <- "#CFE9FF"
bittersweet <- "#F07167"
white <- "#FFFFFF"
alice_blue <- "#D9F0FE"
who_blue <- "#158BC6"
light_gray <- "#ebebeb"

my_fresh_theme <- create_theme(
  adminlte_color(
    light_blue = white
  ),
  adminlte_sidebar(
    dark_bg = space_cadet,
    dark_hover_bg = who_blue,
    dark_color = alice_blue
  ),
  adminlte_global(
    content_bg = white,
    box_bg = white, 
    info_box_bg = light_gray
  )
)

spinner_color <- burnt_sienna <- "#EE6C4D" 



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~ ui ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

header <- dashboardHeader()

anchor <- tags$a(href='http://www.example.com',
                 tags$img(src='logo.png', height='50', width='42'),
                 HTML('<span style="color: rgb(21, 139, 198); font-size: 18px;">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Cov</span><span style="font-size: 18px;"><span style="color: rgb(209, 72, 65);">Contact</span></span><span style="color: rgb(21, 139, 198); font-size: 18px;">R</span>'))

header$children[[2]]$children <- tags$div(
  tags$head(tags$style(HTML(".name { background-color: '#1C2541' }"))),
  anchor,
  class = 'name')




sidebar <- 
  dashboardSidebar(sidebarUserPanel("User Name",
                                    subtitle = a(href = "#", icon("circle", class = "text-success"), "Online"),
                                    # Image file should be in www/ subdir
                                    #image = "userimage.png"
                                    ),
                   sidebarMenu(id = "tabs", # Setting id makes input$tabs give the tabName of currently-selected tab
                               menuItem("Load data", tabName = "load_data", icon = icon("table")),
                               menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
                               menuItem("Charts", tabName = "charts", icon = icon("bar-chart-o"),
                                        menuSubItem("Sub-item 1", tabName = "subitem1"),
                                        menuSubItem("Sub-item 2", tabName = "subitem2")
                                        )
                               )
                   )
# sidebar <- 
#   dashboardSidebar(
#   uiOutput("data_to_use"))

#~~ Body ---------------------------

# load_data_tab_row_1 <- 
#   fluidRow(column(width = 3,
#                   box(width = NULL, 
#                       div(style = "overflow-x: scroll; ",
#                           HTML("Prototype R Shiny App to analyse contact tracing data for the Niger dataset: Contact the WHO team <a href='https://www.afro.who.int/about-us/contact-us' target= '_blank' >here</a> or email us at <a href='mailto:afrgocom@who.int' target= '_blank' >afrgocom@who.int</a> for enquiries.")))),
#            column(width = 1,
#                   dropdownButton(tags$h4("Control Outputs"),
#                                  "Will add options to subset by sender, dates",
#                                  icon = icon("gear"), status = "info"),
#                   dropdownButton(tags$h4("Control Outputs"),
#                                  "will add information about non-obvious text analysis decisions",
#                                  icon = icon("info"), status = "info")))
# 
# 
# load_data_tab_row_2 <- 
#   fluidRow(box(width = 12,
#                title = tagList(icon("dashboard"), 
#                                "Box 2"), 
#                closable = F, 
#                collapsible = T,
#                label = boxLabel("Live", status = "success"),
#                tabsetPanel(tabPanel(title = tagList(icon("Completeness",
#                                                          "Tab 1")),
#                                     fluidRow(style = "margin-top:10px;",
#                                              column(width = 12, 
#                                                     plotOutput("data_completeness_plot") %>% 
#                                                       withSpinner()
#                                                   )
#                                   )),
#                            tabPanel(title = "Missingness", 
#                                     fluidRow(style = "margin-top:10px;",
#                                              column(width = 12, 
#                                                     plotOutput("data_completeness_plot2") %>% 
#                                                       withSpinner()
#                                                     )
#                                )
#                       )
#              )
#              
#          )
# )


load_data_tab_row_1 <- 
  fluidRow(box(width = 12,
               title = tagList(icon("hand-pointer"), 
                               "Choose data and columns to analyse"), 
               closable = F, 
               collapsible = T,
               label = boxLabel("Live", status = "info"),
               fluidRow(style = "margin-top:10px;",
                        column(width = 3, 
                               uiOutput("data_to_use")),
                        column(width = 3, 
                               uiOutput("input_data")),
                        column(width = 3, 
                               uiOutput("columns_to_analyze")),
                        column(width = 3, 
                               uiOutput("analyze_action_bttn"))
                        )
               ))

load_data_tab_row_2 <- 
  fluidRow(box(width = 5,
               title = tagList(icon("filter"), 
                               "Apply filters"), 
               closable = F, 
               collapsible = T,
               div(style = "overflow-y:scroll; 
                            height: 60vh",
                   HTML("It is important that you have the following variables:"),
                   uiOutput("filters") %>% 
                     withSpinner(type = 6, color = burnt_sienna)
                   )
               ),
           box(width = 7, 
               title = tagList(icon("microscope"), 
                               "Assess data completeness and quality"), 
               closable = F, 
               collapsible = T,
               tabsetPanel(tabPanel(title = tagList(icon("question-circle"),
                                                         "Missingness"),
                                    HTML("The plot below shows..."),
                                    plotOutput("data_completeness_plot") %>% 
                                      withSpinner(type = 6, color = burnt_sienna)
                                    ), 
                           tabPanel(title = tagList(icon("chart-line"),
                                                         "Cardinality"),
                                    HTML("The plot below shows..."),
                                    plotOutput("data_completeness_plot2") %>% 
                                      withSpinner(type = 6, color = burnt_sienna)
                                    )
                           )
               )
           )



overview_tab_row_1 <- 
  fluidRow(box(width = 4, 
                  withSpinner(type = 6, color = burnt_sienna,
                              highchartOutput("sex_count_donut_plot1"))),
           box(width = 4, 
                  withSpinner(type = 6, color = burnt_sienna,
                              highchartOutput("sex_count_donut_plot2"))),
           box(width = 4, 
                  withSpinner(type = 6, color = burnt_sienna,
                              highchartOutput("sex_count_donut_plot3")))
  )

overview_tab_row_2 <-
  fluidRow(box(width = 8,
                  style = "height:600px",
                  withSpinner(type = 6, color = burnt_sienna,
                              highchartOutput("region_count_sunburst_plot", height = "600px"))), 
           box(width = 4,
                  style = "height:600px",
                  withSpinner(type = 6, color = burnt_sienna,
                              dataTableOutput("contacts_df_table")))
  )

body <-  
  dashboardBody(tags$head(tags$style(HTML(".sidebar-toggle {color:#158BC6 !important}"))),
                tags$head(tags$style(HTML(".fa-gears {color:#158BC6 !important}"))),
                tags$head(tags$style(HTML(".main-header .logo {
                                          text-align:left !important;
                                          padding: 2px 0px 8px 6px !important;}"))),
    
                use_waiter(), # include dependencies
                use_theme(my_fresh_theme), # <-- use the theme
                setBackgroundImage(src = "light_blue_gradient.jpg",
                                   shinydashboard = TRUE),
                tabItems(tabItem(tabName = "load_data",
                                 load_data_tab_row_1,
                                 load_data_tab_row_2
                                 ),
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

# 
# controlbar <-
#   dashboardControlbar(id = "controlbar", collapsed = FALSE, overlay = TRUE,
#                       controlbarMenu(
#                         controlbarItem(title = 'Load data',
#                                        uiOutput("data_to_use"), 
#                                        uiOutput("input_data"), 
#                                        uiOutput("columns_to_analyze"), 
#                                        uiOutput("analyze_action_bttn"), 
#                                        #uiOutput("filters")
#                                        ), 
#                         controlbarItem(title = 'Overview',
#                                        #uiOutput("data_to_use"), 
#                                        #uiOutput("input_data"), 
#                                        #uiOutput("columns_to_analyze"), 
#                                        #uiOutput("analyze_action_bttn"), 
#                                        uiOutput("filters")
#                         )
#                       )
#                       
#                       
#   )

controlbar <- dashboardControlbar(collapsed = TRUE)


footer <-
  dashboardFooter(left = HTML('<span style="font-size: 9px; color: rgb(21, 139, 198);">World Health Organization Regional Office for Africa</span>'),
                  right = "")


#~~ Combine ---------------------------

ui <- 
   dashboardPage(header = header, 
                sidebar = sidebar, 
                body= body, 
                controlbar = controlbar, 
                footer = footer
                )


