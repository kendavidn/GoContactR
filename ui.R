
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~  HEADER ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

header <- dashboardHeader()

anchor <- tags$a(href='https://www.afro.who.int/',
                 # WHO logo
                 tags$img(src='logo.png', height='50', width='42'),
                 # CovContactR title
                 HTML('<span style="color: rgb(21, 139, 198); font-size: 18px;">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Go</span><span style="font-size: 18px;"><span style="color: rgb(209, 72, 65);">Contact</span></span><span style="color: rgb(21, 139, 198); font-size: 18px;">R</span>'))

# Change title background color
header$children[[2]]$children <- tags$div(
  tags$head(tags$style(HTML(".name { background-color: '#1C2541' }"))),
  anchor,
  class = 'name')


# Change right sidebar icon from gear to info 
header$children[[3]]$children[[4]]$children[[1]]$children[[2]]$children[[1]] <- 
  HTML('<a href="#" data-toggle="control-sidebar">
       <i class="fa fa-info" role="presentation" aria-label="info icon"></i>
         </a>')


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~  SIDEBAR ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

sidebar <- 
  dashboardSidebar(collapsed = TRUE,
                   sidebarUserPanel(name = HTML("<br> <span style = 'font-size: 12px'> Côte d'Ivoire, COVID-19 </span>"),
                                    image = "flag.png"),
  sidebarMenu(id = "tabs", # Setting id makes input$tabs give the tabName of currently-selected tab
              menuItem("Load data", tabName = "load_data_tab", icon = icon("file-upload")),
              menuItem("All contacts", tabName = "main_tab", icon = icon("globe")),
              menuItem("Active contacts", tabName = "active_contacts_tab", icon = icon("calendar-week")), 
              menuItem("Help", tabName = "help_tab", icon = icon("info-circle")), 
              menuItem("Click to logout", href = "https://gocontactr.shinyapps.io/cotedivoire/__logout__/",newtab = FALSE,
                       icon = icon("sign-out-alt"))
              )
  )

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~  BODY ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~ load_data_tab ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~ load_data_tab_row_1 ----

load_data_tab_row_1 <- 
  fluidRow(box(width = 12,
               title = tagList(icon("hand-pointer"), 
                               "Choose dataset to analyse"), 
               closable = F, 
               collapsible = T,
               fluidRow(column(width = 3, 
                               uiOutput("data_to_use_picker")),
                        column(width = 6, 
                               uiOutput("input_data_preloaded_or_uploaded")), 
                        column(width = 3, 
                               uiOutput("analyze_action_bttn")))

               )
  )


# ~~~ load_data_tab_row_2 ----

load_data_tab_row_2 <-
  fluidRow(box(width = 12,
               title = tagList(icon("microscope"),
                               "Assess data completeness and quality"),
               closable = F,
               collapsible = T,
               column(width = 9,
                   tabsetPanel(tabPanel(title = tagList(icon("question-circle"),
                                                        "Missingness"),
                                        HTML("<br>
                                             The plot below shows the data types and missingess for the loaded dataset.
                                             If the dataset has more than 1000 rows, then it was downsampled to 1000 rows to hasten plotting speed. 
                                             Repetitive variables (e.g. follow-up days), are not shown. <br> <br>
                                             "),
                                        plotOutput("data_completeness_plot") %>%
                                          withSpinner(type = 6, color = burnt_sienna)
                                        ),
                               tabPanel(title = tagList(icon("chart-line"),
                                                        "Cardinality"),
                                        HTML("<br>
                                             The plot below shows the frequency of categorical levels in the loaded dataset.
                                             If the dataset has more than 1000 rows, then it was downsampled to 1000 rows to hasten plotting speed. 
                                             Please check that key columns are coded properly.  <br> <br>
                                             "),
                                        plotOutput("data_cardinality_plot") %>%
                                          withSpinner(type = 6, color = burnt_sienna)
                                    )
                           )
                   ),
               column(width = 3,
                      tabsetPanel(tabPanel(title = tagList(icon("info-circle"),
                                                           "Description of column requirements"),
                                           style = "overflow-y:auto; height:600px",
                                           includeHTML(here("data/user_guide/column_descriptions.html"))
                                           )
                                  )
                      )
               )
  )

# ~~~ load_data_tab_row_3 ----


load_data_tab_row_3 <- 
  fluidRow(box(width = 12,
               #style = "height:600px",
               title = tagList(icon("border-all"), 
                               "View data frame"), 
               closable = F, 
               collapsible = T,
               column(width = 12,
                      reactableOutput("reactable_table") %>% 
                        withSpinner(type = 6, color = burnt_sienna)
               )
  )
  )

# ~~~ load_data_tab_combine_rows----


load_data_tab <- tabItem(tabName = "load_data_tab",
                         load_data_tab_row_1,
                         load_data_tab_row_2, 
                         load_data_tab_row_3)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~ main_tab ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# ~~~ main_tab_row_00_admin_1---- 
main_tab_row_00 <- 
  fluidRow(box(width = 4,
               title = tagList(icon("hand-pointer"), 
                               "Date of review"), 
               closable = FALSE, 
               collapsible = TRUE,
               fluidRow(column(width = 12, 
                               uiOutput("select_date_of_review", style = "height: 4.7em;"))
                        )
               ),
           box(width = 8,
               title = tagList(icon("file-download"), 
                               "Download report"),
               closable = FALSE,
               collapsible = TRUE,
               fluidRow(column(width = 5,
                               uiOutput("select_format", style = "height: 4.7em;")
                               ),
                        column(width = 7, 
                               uiOutput("download_report_button"))
                        )
               )
           )


# ~~~ main_tab_select_columns---- 

main_tab_select_columns <- 
  fluidRow(box(width = 12,
               title = tagList(icon("filter"), 
                               "Additional filters"), 
               closable = FALSE, 
               collapsible = TRUE,
               collapsed = TRUE,
               fluidRow(style = "height: 30vh; overflow-y:auto",
                        column(width = 4,
                               radioButtons("filter_or_not", 
                                            label = "Load additional filters?", 
                                            choices = c("Yes", "No"), 
                                            selected = "No"),
                               htmlOutput("additional_filters_text")
                               ),
                        column(width = 8, 
                               uiOutput("filters")
                               )
                        )
               )
           # ,
           # box(width = 8,
           #     title = tagList(icon("file-download"), 
           #                    "Download report"),
           #     closable = FALSE,
           #     collapsible = TRUE,
           #     fluidRow(column(width = 5,
           #                    uiOutput("select_format", style = "height: 4.7em;")
           #     ),
           #     column(width = 7, 
           #           uiOutput("download_report_button"))
           #     )
  #)
  )


# ~~~ main_tab_row_0 ----

main_tab_row_0 <-
  fluidRow(valueBoxOutput("contacts_per_day_value_box", width = 3),
           valueBoxOutput("cumulative_contacts_value_box", width = 3),
           valueBoxOutput("contacts_under_surveillance_value_box", width = 3),
           valueBoxOutput("pct_contacts_followed_value_box", width = 3)
           )




# ~~~ main_tab_row_1 ----


main_tab_row_1 <-
  tagList(
  h1("  "), 
  icon("map", "fa-2x"),
  h3("Total contacts per admin level 1", 
     style = "display: inline; line-height: 50px;"),
  fluidRow(column(width = 12,
               column(width = 9,
                      tabsetPanel(tabPanel(title = "Bar plot",
                                           highchartOutput("all_contacts_per_admin_1_bar_chart" ) %>%
                                             withSpinner(type = 6, color = burnt_sienna)),
                                  tabPanel(title = "Sunburst plot",
                                           highchartOutput("all_contacts_per_admin_1_sunburst_plot", 
                                                           height = "350px") %>%
                                             withSpinner(type = 6, color = burnt_sienna)),
                                  tabPanel(title = "Table",
                                           reactableOutput("all_contacts_per_admin_1_table") %>%
                                             withSpinner(type = 6, color = burnt_sienna))
                      )
               ),
               column(width = 3,
                      tabsetPanel(tabPanel(title = tagList(icon("info-circle"),
                                                           "Description"),
                                           style = "overflow-y:auto",
                                           htmlOutput("all_contacts_per_admin_1_text") %>% 
                                             withSpinner(type = 1, color = burnt_sienna))
                      )
                      )
               )
  ), 
  hr()
  )



# ~~~ main_tab_row_2 ----

main_tab_row_2 <-
  tagList(
    h1("  "), 
    icon("calendar-alt", "fa-2x"),
    h3("Contacts under surveillance per admin level 1 over time", 
       style = "display: inline; line-height: 50px;"),
    fluidRow(column(width = 12,
               column(width = 9,
                      tabsetPanel(tabPanel(title = "Absolute numbers",
                                           highchartOutput("contacts_under_surveillance_per_admin_1_over_time_bar_chart") %>%
                                             withSpinner(type = 6, color = burnt_sienna)),
                                  tabPanel(title = "Relative proportions",
                                           highchartOutput("contacts_under_surveillance_per_admin_1_over_time_bar_chart_relative") %>%
                                             withSpinner(type = 6, color = burnt_sienna)) 
                                  
                      )
               ),
               column(width = 3,
                      tabsetPanel(tabPanel(title = tagList(icon("info-circle"),
                                                           "Description"),
                                           style = "overflow-y:auto",
                                           htmlOutput("contacts_under_surveillance_per_admin_1_over_time_text") %>% 
                                             withSpinner(type = 1, color = burnt_sienna))
                      )
               )
               )
  ), 
  hr()
  )


# ~~~ main_tab_row_3 ----

main_tab_row_3 <-
  tagList(
    h1("  "), 
    icon("users", "fa-2x"),
    h3("Number of contacts per case", 
       style = "display: inline; line-height: 50px;"),
  fluidRow(column(width = 12,
               column(width = 9,
                      tabsetPanel(tabPanel(title = "Donut plot",
                                           highchartOutput("total_contacts_per_case_donut_plot") %>%
                                             withSpinner(type = 6, color = burnt_sienna)), 
                                  tabPanel(title = "Bar chart",
                                           highchartOutput("total_contacts_per_case_bar_chart") %>%
                                             withSpinner(type = 6, color = burnt_sienna)) 
                                  
                                  # ,
                                  # tabPanel(title = "Table",
                                  #          reactableOutput("total_contacts_per_case_table") %>%
                                  #            withSpinner(type = 6, color = burnt_sienna)) 
                                  
                      )
               ),
               column(width = 3,
                      tabsetPanel(tabPanel(title = tagList(icon("info-circle"),
                                                           "Description"),
                                           style = "overflow-y:auto",
                                           htmlOutput("total_contacts_per_case_text") %>% 
                                             withSpinner(type = 1, color = burnt_sienna))
                      )
               )
  )
  ), 
  hr()
  )

# ~~~ main_tab_row_4 ----

main_tab_row_4 <-
  tagList(
    h1("  "), 
    icon("link", "fa-2x"),
    h3("Case-contact relationships", 
       style = "display: inline; line-height: 50px;"),
  fluidRow(column(width = 12,
               column(width = 9,
                      tabsetPanel(tabPanel(title = "Donut plot",
                                           highchartOutput("total_contacts_per_link_type_donut_plot") %>%
                                             withSpinner(type = 6, color = burnt_sienna)), 
                                  tabPanel(title = "Bar chart",
                                           highchartOutput("total_contacts_per_link_type_bar_chart") %>%
                                             withSpinner(type = 6, color = burnt_sienna))
                                  
                      )
               ),
               column(width = 3,
                      tabsetPanel(tabPanel(title = tagList(icon("info-circle"),
                                                           "Description"),
                                           style = "overflow-y:auto",
                                           htmlOutput("total_contacts_per_link_type_text") %>% 
                                             withSpinner(type = 1, color = burnt_sienna))
                      )
               )
  )
  ), 
  hr()
  )

# ~~~ main_tab_combine_rows----


main_tab <- tabItem("main_tab",
                    main_tab_row_00,
                    main_tab_select_columns,
                    main_tab_row_0,
                    h1("All contacts", align = "center"), 
                    hr(),
                    main_tab_row_1, 
                    main_tab_row_2,
                    main_tab_row_3, 
                    main_tab_row_4
                    )



# 
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # ~~ main_tab_admin_1  ----
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# 
# # ~~~ main_tab_row_00_admin_1---- 
# main_tab_row_00_admin_1 <- 
#   fluidRow(box(width = 12,
#                title = tagList(icon("hand-pointer"), 
#                                "Select admin level 1 and date"), 
#                closable = F, 
#                collapsible = T,
#                fluidRow(column(width = 6, 
#                                uiOutput("select_admin_1"), 
#                                uiOutput("select_date_of_review_admin_1"))
#                         )
#   ))
# 
# 
# main_tab_row_0_admin_1 <-
#   fluidRow(
#     valueBoxOutput("contacts_per_day_value_box_admin_1", width = 3),
#            valueBoxOutput("cumulative_contacts_value_box_admin_1", width = 3),
#            valueBoxOutput("contacts_under_surveillance_value_box_admin_1", width = 3),
#            valueBoxOutput("pct_contacts_followed_value_box_admin_1", width = 3)
#   )
# 
# 
# # ~~~ main_tab_row_1_admin_1 ----
# 
# 
# main_tab_row_1_admin_1 <-
#   tagList(
#     h1("  "), 
#     icon("map", "fa-2x"),
#     h3("Total contacts per admin level 2", 
#        style = "display: inline; line-height: 50px;"),
#   fluidRow(column(width = 12,
#                column(width = 9,
#                       tabsetPanel(tabPanel(title = "Table",
#                                            reactableOutput("all_contacts_per_admin_1_table_admin_1") %>%
#                                              withSpinner(type = 6, color = burnt_sienna)),
#                                   tabPanel(title = "Plot",
#                                            highchartOutput("all_contacts_per_admin_1_sunburst_plot_admin_1", 
#                                                            height = "350px"
#                                            ) %>%
#                                              withSpinner(type = 6, color = burnt_sienna))
#                       )
#                ),
#                column(width = 3,
#                       tabsetPanel(tabPanel(title = tagList(icon("info-circle"),
#                                                            "Description"),
#                                            style = "overflow-y:auto",
#                                            htmlOutput("all_contacts_per_admin_1_text_admin_1") %>% 
#                                              withSpinner(type = 1, color = burnt_sienna))
#                       )
#                )
#   )
#   ), 
#   hr()
#   )
# 
# 
# 
# 
# 
# 
# # ~~~ main_tab_row_2_admin_1 ----
# 
# main_tab_row_2_admin_1 <-
#   tagList(
#     h1("  "), 
#     icon("calendar-alt", "fa-2x"),
#     h3("Contacts under surveillance per admin level 2 over time", 
#        style = "display: inline; line-height: 50px;"),
#   fluidRow(column(width = 12,
#                column(width = 9,
#                       tabsetPanel(tabPanel(title = "Absolute numbers",
#                                            highchartOutput("contacts_under_surveillance_per_admin_1_over_time_bar_chart_admin_1") %>%
#                                              withSpinner(type = 6, color = burnt_sienna)),
#                                   tabPanel(title = "Relative proportions",
#                                            highchartOutput("contacts_under_surveillance_per_admin_1_over_time_bar_chart_relative_admin_1") %>%
#                                              withSpinner(type = 6, color = burnt_sienna)) 
#                                   
#                       )
#                ),
#                column(width = 3,
#                       tabsetPanel(tabPanel(title = tagList(icon("info-circle"),
#                                                            "Description"),
#                                            style = "overflow-y:auto",
#                                            htmlOutput("contacts_under_surveillance_per_admin_1_over_time_text_admin_1") %>% 
#                                              withSpinner(type = 1, color = burnt_sienna))
#                       )
#                )
#   )
#   ), 
#   hr()
#   )
# 
# 
# 
# 
# # ~~~ main_tab_row_3_admin_1 ----
# 
# main_tab_row_3_admin_1 <-
#   tagList(
#     h1("  "), 
#     icon("users", "fa-2x"),
#     h3("Number of contacts per case", 
#        style = "display: inline; line-height: 50px;"),
#   fluidRow(column(width = 12,
#                column(width = 9,
#                       tabsetPanel(tabPanel(title = "Plot",
#                                            highchartOutput("total_contacts_per_case_donut_plot_admin_1") %>%
#                                              withSpinner(type = 6, color = burnt_sienna))
#                                   
#                       )
#                ),
#                column(width = 3,
#                       tabsetPanel(tabPanel(title = tagList(icon("info-circle"),
#                                                            "Description"),
#                                            style = "overflow-y:auto",
#                                            htmlOutput("total_contacts_per_case_text_admin_1") %>% 
#                                              withSpinner(type = 1, color = burnt_sienna))
#                       )
#                )
#   )
#   ), 
#   hr()
#   )
# 
# # ~~~ main_tab_row_4_admin_1 ----
# 
# main_tab_row_4_admin_1 <-
#   tagList(
#     h1("  "), 
#     icon("link", "fa-2x"),
#     h3("Case-contact relationships", 
#        style = "display: inline; line-height: 50px;"),
#   fluidRow(column(width = 12,
#                column(width = 9,
#                       tabsetPanel(tabPanel(title = "Plot",
#                                            highchartOutput("total_contacts_per_link_type_donut_plot_admin_1") %>%
#                                              withSpinner(type = 6, color = burnt_sienna))
#                                   
#                       )
#                ),
#                column(width = 3,
#                       tabsetPanel(tabPanel(title = tagList(icon("info-circle"),
#                                                            "Description"),
#                                            style = "overflow-y:auto",
#                                            htmlOutput("total_contacts_per_link_type_text_admin_1") %>% 
#                                              withSpinner(type = 1, color = burnt_sienna))
#                       )
#                )
#   )
#   ), 
#   hr()
#   )
# 
# 
# # ~~~ main_tab_row_6_admin_1 ----
# 
# 
# main_tab_row_6_admin_1 <-
#   tagList(
#     h1("  "),
#     icon("user-clock", "fa-2x"),
#     h3("Follow-up state",
#        style = "display: inline; line-height: 50px;"),
#     fluidRow(column(width = 12,
#                     column(width = 9,
#                            tabsetPanel(tabPanel(title = "Plots",
#                                                 plotlyOutput("active_contacts_timeline_snake_plot_admin_1",height = 700) %>%
#                                                   withSpinner(type = 6, color = burnt_sienna),
#                                                 plotlyOutput("active_contacts_breakdown_bar_chart_admin_1") %>%
#                                                   withSpinner(type = 6, color = burnt_sienna)
#                            ),
#                            tabPanel(title = "Table",
#                                     reactableOutput("active_contacts_breakdown_table_admin_1") %>%
#                                       withSpinner(type = 6, color = burnt_sienna))
# 
#                            )
#                     ),
#                     column(width = 3,
#                            tabsetPanel(tabPanel(title = tagList(icon("info-circle"),
#                                                                 "Description"),
#                                                 tagList(HTML("<br>
#                                                         <span style='color: rgb(97, 189, 109);'>ℹ:</span>
#                                                         <font size='1'>
#                                                         The bar chart and snake plot show the follow-up states of currently active contacts,
#                                                         (contacts that should be currently under surveillance).
#                                                         <br>
#                                                         </font>")),
#                                                 htmlOutput("active_contacts_timeline_text_admin_1")
# 
# 
#                            )
#                            )
#                     )
#     )
#     ),
#     hr()
#   )
# 
# 
# 
# 
# # ~~~ main_tab_row_7_admin_1 ----
# 
# main_tab_row_7_admin_1 <-
#   tagList(
#     h1("  "),
#     icon("low-vision", "fa-2x"),
#     h3("Contacts not seen recently",
#        style = "display: inline; line-height: 50px;"),
#     fluidRow(column(width = 12,
#                     column(width = 9,
#                            tabsetPanel(tabPanel(title = "Summary",
#                                                 gt_output("contacts_lost_24_to_72_hours_table_admin_1") %>%
#                                                   withSpinner(type = 6, color = burnt_sienna)),
#                                        tabPanel(title = "List of contacts not seen",
#                                                 htmlOutput("lost_contacts_linelist_table_title_admin_1"),
#                                                 reactableOutput("lost_contacts_linelist_table_admin_1") %>%
#                                                   withSpinner(type = 6, color = burnt_sienna))
# 
#                            )
#                     ),
#                     column(width = 3,
#                            tabsetPanel(tabPanel(title = tagList(icon("info-circle"),
#                                                                 "Description"),
#                                                 style = "overflow-y:auto",
#                                                 HTML("<br>
#                                                  <span style='color: rgb(97, 189, 109);'>ℹ:</span>
#                                                  <font size='1'>
#                                                  The tables track the contacts who should be under surveillance but have not been followed for an extended period.
#                                                             </font>"
#                                                 )
#                            )
#                            )
#                     )
#     )
#     ),
#     hr()
#   )



# ~~~ main_tab_admin_1_combine_rows----

# 
# 
# main_tab_admin_1 <- tabItem("main_tab_admin_1",
#                                      main_tab_row_00_admin_1, 
#                                      main_tab_row_0_admin_1, 
#                                      h1("All contacts", align = "center"), 
#                                      hr(),
#                                      main_tab_row_1_admin_1, 
#                                      main_tab_row_2_admin_1, 
#                                      main_tab_row_3_admin_1,
#                                      main_tab_row_4_admin_1, 
#                                      h1("Active contacts", align = "center"), 
#                                      hr(),
#                                      main_tab_row_6_admin_1, 
#                                      main_tab_row_7_admin_1
#                                      )

# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # ~~ active_contacts_tab  ----
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~ active_contacts_row ----


active_contacts_row <- 
  tagList(
    h1("  "), 
    icon("user-clock", "fa-2x"),
    h3("Follow-up timeline", 
       style = "display: inline; line-height: 50px;"),
    fluidRow(column(width = 12,
                    column(width = 9,
                           tabsetPanel(tabPanel(title = "Status bar chart and snake plot",
                                                style = "overflow-y:auto",
                                                # echarts4rOutput("active_contacts_timeline_snake_plot", height = 700) %>%
                                                #   withSpinner(type = 6, color = burnt_sienna)
                                                plotlyOutput("active_contacts_breakdown_bar_chart", height = 300) %>%
                                                  withSpinner(type = 6, color = burnt_sienna),
                                                h4(HTML("For snake plot below:")),
                                                h6(HTML("• <b>Drag</b> to select contacts on snake plot. This filters the table beneath. Double-click anywhere to reset selection")),
                                                h6(HTML("• <b>Double click</b> on legend to isolate group.")),
                                                plotlyOutput("active_contacts_timeline_snake_plot", height = 500) %>%
                                                  withSpinner(type = 6, color = burnt_sienna),
                                                downloadButton("active_contacts_snake_plot_selected_table_download", 
                                                               "Download selected contacts"),
                                                reactableOutput("active_contacts_snake_plot_selected_table", height = 400) %>%
                                                  withSpinner(type = 6, color = burnt_sienna)
                           ),
                           tabPanel(title = "Timeline table",
                                    downloadButton("active_contacts_timeline_table_download", 
                                                   "Download timeline data"),
                                    reactableOutput("active_contacts_timeline_table") %>%
                                      withSpinner(type = 6, color = burnt_sienna)),
                           tabPanel(title = "Summary table",
                                    downloadButton("active_contacts_breakdown_table_download", 
                                                   "Download summary table"),
                                    reactableOutput("active_contacts_breakdown_table") %>%
                                      withSpinner(type = 6, color = burnt_sienna)) 
                           
                           )
                    ),
                    column(width = 3,
                           tabsetPanel(tabPanel(title = tagList(icon("info-circle"), 
                                                                "Description"),
                                                htmlOutput("active_contacts_timeline_text")
                                                
                                                
                           )
                           )
                    )
    )
    ), 
    hr()
  )


# ~~~ lost_contacts_row ----

lost_contacts_row <- 
  tagList(
    h1("  "), 
    icon("low-vision", "fa-2x"),
    h3("Contacts not seen recently", 
       style = "display: inline; line-height: 50px;"),
    fluidRow(column(width = 12,
                    column(width = 9,
                           tabsetPanel(tabPanel(title = "Loss to follow-up, past 3 days",
                                                downloadButton("contacts_lost_24_to_72_hours_table_download", 
                                                               "Download summary table"),
                                                gt_output("contacts_lost_24_to_72_hours_table") %>%
                                                  withSpinner(type = 6, color = burnt_sienna)), 
                                       tabPanel(title = "List of contacts not seen",
                                                htmlOutput("lost_contacts_linelist_table_title"),
                                                downloadButton("lost_contacts_linelist_table_download", 
                                                               "Download lost contacts linelist"),
                                                reactableOutput("lost_contacts_linelist_table") %>%
                                                  withSpinner(type = 6, color = burnt_sienna))
                                       
                           )
                    ),
                    column(width = 3,
                           tabsetPanel(tabPanel(title = tagList(icon("info-circle"),
                                                                "Description"),
                                                style = "overflow-y:auto",
                                                htmlOutput("lost_contacts_linelist_text") %>% 
                                                  withSpinner(type = 1, color = burnt_sienna)
                                                
                           )
                           )
                    )
    )
    ), 
    hr()
  )



active_contacts_tab <- tabItem(tabName = "active_contacts_tab",
                               h1("Active contacts", align = "center"), 
                               hr(),
                               active_contacts_row, 
                               lost_contacts_row
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~ help_tab  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


help_tab_row_1 <-
  tagList(
    h1("  "),
    icon("info-circle", "fa-2x"),
    h3("WHo Afro technical guidance on contact tracing",
       style = "display: inline; line-height: 50px;"),
  fluidRow(column(width = 12,
                  tags$iframe(style="height:80vh; width:80%; text-align: center, scrolling=yes", 
                              src= "WHO_Afro_technical_guidance_for_contact_tracing.pdf#zoom=90")
                  )
           )
  )

help_tab <- tabItem("help_tab", 
                    h1("Resources", align = "center"), 
                    hr(),
                    help_tab_row_1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~ Combine into body object  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


body <-  
  dashboardBody(tags$head(tags$style(HTML('* {font-family: "Avenir" };'))), # change font
                # wide button
                tags$head(tags$style(HTML(".bttn-jelly {width:100%; !important}"))),
                # sidebar toggle icon color
                tags$head(tags$style(HTML(".sidebar-toggle {color:#158BC6 !important}"))),
                # right info icon color
                tags$head(tags$style(HTML(".fa-info {color:#158BC6 !important}"))),
                tags$head(tags$style(HTML(".control-sidebar {height: 95vh; overflow-y:auto !important}"))),
                tags$head(tags$style(HTML(".main-header .logo {
                                          text-align:left !important;
                                          padding: 2px 0px 8px 6px !important;}"))),
                # reduce box padding
                #tags$head(tags$style(HTML('.box {padding: 4px !important;}'))),
                #tags$head(tags$style(HTML('.box {margin: 4px !important;}'))),
                # value boxes
                #tags$head(tags$style(HTML('.col-sm-3 {padding: 5 5 5 10px !important;}'))),
                
                # error color
                tags$head(tags$style(".shiny-output-error{color: grey;}")),
                # for FAQs
                tags$head(tags$style(HTML(".box.box-solid.box-success>.box-header {
                                          padding: 2px 2px 2px 2px !important;}"))),
                use_theme(my_fresh_theme), # <-- use the theme
                setBackgroundImage(src = "background.jpg",
                                   shinydashboard = TRUE),
                tabItems(load_data_tab,
                         main_tab,
                         active_contacts_tab
                         #, 
                         #help_tab
                         )
  )



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~ RIGHT SIDEBAR/CONTROLBAR ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# read in FAQ data frame
faq_df <- 
  read_csv(here("data/FAQs.csv")) %>% 
  mutate(row = row_number())

# Create faq accordion
faq_accordion <- 
  faq_function(faq_df$Questions, faq_df$Answers, n = faq_df$row) %>% 
  paste0(collapse = " ") %>% 
  str_sub(end = -2) # remove final comma

# paste controlbar syntax and faq_accordion text together
controlbar_str <-
  paste0(
    'dashboardControlbar(
    id = "controlbar",
    collapsed = TRUE,
    overlay = TRUE,
    controlbarMenu(
      controlbarItem(
        title = "FAQs",',
    faq_accordion,
    '),
      controlbarItem(title = "Support sources")
    )
  )',  collapse = " ")

# store in controlbar object. There should be a neater way to achieve all of this
controlbar <- eval(str2expression(controlbar_str))

# overwrite 
# controlbar <- 
#   dashboardControlbar(id = "controlbar",
#                       collapsed = TRUE,
#                       overlay = TRUE,
#                       controlbarMenu(
#                         controlbarItem(
#                           fluidRow(uiOutput("filters1"))
#     )
#   )
#   )
#   

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~ FOOTER ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

footer <-
  dashboardFooter(left = HTML('<span style="font-size: 9px; color: rgb(21, 139, 198);">World Health Organization Regional Office for Africa</span>'),
                  right = "")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~ BUILD FINAL UI OBJECT----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ui <- 
  dashboardPage(header = header, 
                sidebar = sidebar, 
                body= body, 
                controlbar = controlbar, 
                footer = footer
  )

#polished::secure_ui(ui)


