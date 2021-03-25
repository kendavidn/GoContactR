
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
                   sidebarUserPanel(name = HTML("<br>Guinée, EVD 2021"),
                                    image = "flag.png"),
  sidebarMenu(id = "tabs", # Setting id makes input$tabs give the tabName of currently-selected tab
              menuItem("Load data", tabName = "load_data_tab", icon = icon("file-import")),
              menuItem("All contacts", tabName = "all_contacts_tab", icon = icon("globe")),
              menuItem("Contacts per region", tabName = "regional_contacts_tab", icon = icon("map-marked"))
              
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

load_data_tab_row_1 <- 
  fluidRow(box(width = 12,
               title = tagList(icon("hand-pointer"), 
                               "Choose dataset to analyse"), 
               closable = F, 
               collapsible = T,
               fluidRow(column(width = 4, 
                               uiOutput("data_to_use")),
                        column(width = 4, 
                               uiOutput("input_data")),
                        column(width = 4, 
                               uiOutput("select_date_of_review"))
                        #,
                        #column(width = 3, 
                        #       uiOutput("analyze_action_bttn"))

               )
  ))


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


load_data_tab <- tabItem(tabName = "load_data_tab",
                         load_data_tab_row_1,
                         load_data_tab_row_2, 
                         load_data_tab_row_3)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~ all_contacts_tab ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



all_contacts_tab_row_0 <-
  fluidRow(valueBoxOutput("contacts_registered_per_day_value_box", width = 3),
           valueBoxOutput("cumulative_contacts_registered_value_box", width = 3),
           valueBoxOutput("contacts_under_surveillance_value_box", width = 3),
           valueBoxOutput("pct_contacts_followed_value_box", width = 3)
           )






all_contacts_tab_row_1 <-
  fluidRow(box(width = 12,
               title = tagList(icon("map"),
                               "All contacts per region"),
               closable = F,
               collapsible = T,
               column(width = 9,
                      tabsetPanel(tabPanel(title = "Contacts by prefecture, table",
                                           reactableOutput("all_contacts_per_region_table") %>%
                                             withSpinner(type = 6, color = burnt_sienna)),
                                  tabPanel(title = "Contacts by prefecture, sunburst plot",
                                           highchartOutput("all_contacts_per_region_sunburst_plot", 
                                                           height = "350px"
                                                           ) %>%
                                             withSpinner(type = 6, color = burnt_sienna))
                      )
               ),
               column(width = 3,
                      tabsetPanel(tabPanel(title = tagList(icon("info-circle"),
                                                           "Description"),
                                           style = "overflow-y:auto",
                                           htmlOutput("all_contacts_per_region_text") %>% 
                                             withSpinner(type = 1, color = burnt_sienna))
                      )
                      )
               )
  )




all_contacts_tab_row_2 <-
  fluidRow(box(width = 12,
               title = tagList(icon("calendar-alt"),
                               "Contacts under surveillance over time"),
               closable = F,
               collapsible = T,
               column(width = 9,
                      tabsetPanel(tabPanel(title = "N. under surveillance over time",
                                           highchartOutput("contacts_under_surveillance_per_region_over_time_bar_chart") %>%
                                             withSpinner(type = 6, color = burnt_sienna)),
                                  tabPanel(title = "N. under surveillance over time (relative)",
                                           highchartOutput("contacts_under_surveillance_per_region_over_time_bar_chart_relative") %>%
                                             withSpinner(type = 6, color = burnt_sienna)) 
                                  
                      )
               ),
               column(width = 3,
                      tabsetPanel(tabPanel(title = tagList(icon("info-circle"),
                                                           "Description"),
                                           style = "overflow-y:auto",
                                           htmlOutput("contacts_under_surveillance_per_region_over_time_text") %>% 
                                             withSpinner(type = 1, color = burnt_sienna))
                      )
               )
               )
  )


# ~~~ all_contacts_tab_row_3 ----

all_contacts_tab_row_3 <-
  fluidRow(box(width = 12,
               title = tagList(icon("users"),
                               "Total contacts per case"),
               closable = F,
               collapsible = T,
               column(width = 9,
                      tabsetPanel(tabPanel(title = "Contacts per case, plot",
                                           highchartOutput("total_contacts_per_case_donut_plot") %>%
                                             withSpinner(type = 6, color = burnt_sienna)),
                                  tabPanel(title = "Contacts per case, table",
                                           reactableOutput("total_contacts_per_case_table") %>%
                                             withSpinner(type = 6, color = burnt_sienna)) 
                                  
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
  )

# ~~~ all_contacts_tab_row_4 ----

all_contacts_tab_row_4 <-
  fluidRow(box(width = 12,
               title = tagList(icon("link"),
                               "Link type between contacts and cases"),
               closable = F,
               collapsible = T,
               column(width = 9,
                      tabsetPanel(tabPanel(title = "Contacts per link type, plot",
                                           highchartOutput("total_contacts_per_link_type_donut_plot") %>%
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
  )

# ~~~ all_contacts_tab_row_5 ----

all_contacts_tab_row_5 <-
  fluidRow(box(width = 12,
               title = tagList(icon("syringe"),
                               "Vaccinations per sub-prefecture"),
               closable = F,
               collapsible = T,
               column(width = 9,
                      tabsetPanel(tabPanel(title = "Contacts vaccinated per sub-prefecture, plot",
                                           highchartOutput("total_contacts_vaccinated_bar_plot") %>%
                                             withSpinner(type = 6, color = burnt_sienna))
                                  
                      )
               ),
               column(width = 3,
                      tabsetPanel(tabPanel(title = tagList(icon("info-circle"),
                                                           "Description"),
                                           style = "overflow-y:auto",
                                           htmlOutput("total_contacts_vaccinated_text") %>% 
                                             withSpinner(type = 1, color = burnt_sienna))
                      )
               )
  )
  )




all_contacts_tab_row_6 <- 
  fluidRow(box(width = 12,
               title = tagList(icon("search"),
                               "Active contacts timeline"),
               closable = F,
               collapsible = T,
               column(width = 9,
                      tabsetPanel(tabPanel(title = "Active contacts, snake plot",
                                           highchartOutput("active_contacts_timeline_snake_plot") %>%
                                             withSpinner(type = 6, color = burnt_sienna))
                                  
                      )
               ),
               column(width = 3,
                      tabsetPanel(tabPanel(title = tagList(icon("mouse"), 
                                                           "Generate snake plot"),
                                           tagList(HTML("<br>At the moment, only 50 contacts can be shown on the timeline plot at a time, 
                                                as plot rendering is too slow otherwise. If more than 50 contacts are selected, a sample of 50 is drawn.<br>"), 
                                                uiOutput("snake_plot_slider"),
                                                uiOutput("generate_snake_plot_bttn")
                                                
                                                
                                                ))
                                  )
                      )
               )
  )
  




all_contacts_tab_row_7 <- 
  fluidRow(box(width = 12,
               title = tagList(icon("question-circle"),
                               "Contacts lost to follow-up"),
               closable = F,
               collapsible = T,
               column(width = 9,
                      tabsetPanel(tabPanel(title = "Contacts not seen in past day, 2d., 3d.",
                                           gt_output("contacts_lost_24_to_72_hours") %>%
                                             withSpinner(type = 6, color = burnt_sienna)), 
                                  tabPanel(title = "List of contacts not seen in past 3d",
                                           reactableOutput("lost_contacts_linelist") %>%
                                             withSpinner(type = 6, color = burnt_sienna))
                                  
                      )
               ),
               column(width = 3,
                      tabsetPanel(tabPanel(title = tagList(icon("info-circle"),
                                                           "Description"),
                                           style = "overflow-y:auto",
                                           HTML("<br>
                                                 <span style='color: rgb(97, 189, 109);'>ℹ:</span>
                                                 <font size='1'>
                                                 The tables track the contacts who should be under surveillance but have not been followed for an extended period.
                                                            </font>"
                                                )
                      )
               )
  )
  )
  )



all_contacts_tab <- tabItem("all_contacts_tab",
                            all_contacts_tab_row_0,
                            all_contacts_tab_row_1, 
                            all_contacts_tab_row_2, 
                            all_contacts_tab_row_3, 
                            all_contacts_tab_row_4, 
                            all_contacts_tab_row_5, 
                            all_contacts_tab_row_6, 
                            all_contacts_tab_row_7)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~ regional_contacts_tab  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


regional_contacts_tab_select <- 
  fluidRow(box(width = 12,
               title = tagList(icon("hand-pointer"), 
                               "Select region to analyse"), 
               closable = F, 
               collapsible = T,
               fluidRow(column(width = 6, 
                               uiOutput("regional_contacts_tab_select")),
                        column(width = 6, 
                               uiOutput("regional_select_date_of_review"))
                        )
  ))

  


regional_contacts_tab <- tabItem("regional_summary_tab",
                                 regional_contacts_tab_select)



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
                tags$head(tags$style(HTML('.box {margin: 5px;}'))),
                tags$head(tags$style(".shiny-output-error{color: grey;}")),
                # for FAQs
                tags$head(tags$style(HTML(".box.box-solid.box-success>.box-header {
                                          padding: 2px 2px 2px 2px !important;}"))),
                use_waiter(), # include dependencies
                use_theme(my_fresh_theme), # <-- use the theme
                setBackgroundImage(src = "background.jpg",
                                   shinydashboard = TRUE),
                tabItems(load_data_tab,
                         all_contacts_tab,
                         regional_contacts_tab)
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

# store in controlbar object. There should be a neater way to achieve this
controlbar <- eval(str2expression(controlbar_str))

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


