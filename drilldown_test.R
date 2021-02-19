library (shinyjs)
library (tidyr)
library (data.table)
library (highcharter)
library (dplyr)
library (shinydashboard)
library (shiny)

x <- c("Farm","Farm","Farm","City","City","City","Ocean","Ocean")
y <- c("Sheep","Sheep","Cow","Car","Bus","Bus","Boat","Boat")
z <- c("Bill","Tracy","Sandy","Bob","Carl","Newt","Fig","Tony")
a <- c(1,1,1,1,1,1,1,1)

dat <- data.frame(x,y,z,a)


ui <- dashboardPage(header = dashboardHeader(), 
                    sidebar = dashboardSidebar(), 
                    body = dashboardBody(
                      #shiny::textInput(inputId = "text_to_print", label = "enter text"),
                      highchartOutput(outputId = "Working"),
                      highchartOutput(outputId = "Working_control_group"),
                      textOutput(outputId = "print_text"), 
                      textOutput(outputId = "print_text2_control_group")
                      
                      
                      
                    ))


server <- function(input, output, session) {
  
  # To hold the current drilldown status as list, i.e. list("Farm", "Sheep")
  #state <- reactiveValues(drills = list())
  
  # Since Drilldown from Highcharts is not used: Install own click handler that builds up the drill list.
#  observeEvent(eventExpr = input$ClickedInput)
  
  state <- reactiveValues(drills = list())
  
  observeEvent(eventExpr = input$ClickedInput,
               handlerExpr = {state$drills <<- input$ClickedInput })
          
  # output$print_text1 <- 
  #   renderText({
  #     print(state$drills)
  # 
  #   })
  # 
  output$print_text1 <- 
    renderText({
      print(input$ClickedInput)
      
    })
  
  output$print_text2_control_group <- 
        renderText({
        print(input$pieclick)
          })

  
  # output$print_text <- 
  #   renderText({
  #     
  #     state$drills <<- input$text_to_print
  #     
  #   print(state$drills)
  #   #print(input$text_to_print)
  #     
  #     })
  # 
  
  output$Working <- renderHighchart({
  
  # This time, click handler is needed.
  # pointClickFunction <- JS("function(event) {Shiny.onInputChange('treemapclick', event.point.name);}")
  # 
  # gapminder_2007 <-
  #   gapminder::gapminder %>%
  #   filter(year  == max(year)) %>%
  #   mutate(pop_mm = round(pop/1e6))
  # 
  # dout <- data_to_hierarchical(gapminder_2007, c(continent, country), pop_mm)
  # 
  # hchart(dout, type = "treemap") %>%
  #   hc_plotOptions(column = list(stacking = "normal", events = list(click = pointClickFunction)))
  # 

  pointClickFunction <- JS("function(event) {Shiny.onInputChange('pieclick', event.point.name);}")
  
  highchart() %>%
  hc_chart(type = "pie") %>%
  hc_add_series(data = list(
    list(y = 3, name = "cat 1"),
    list(y = 4, name = "dog 11"),
    list(y = 6, name = "cow 55"))) %>%
  hc_plotOptions(series = list(events = list(click = pointClickFunction)))
  
  })
  
  
  output$Working_control_group <- renderHighchart({
    
    # This time, click handler is needed.
    pointClickFunction <- JS("function(event) {Shiny.onInputChange('ClickedInput_control', event.point.name);}")
    
      x_level <- "Farm"

      sub <- dat[dat$x == x_level, ]


      tibbled <- 
        data.frame(category = sub$y, amount = sub$a) %>%
        group_by(category) %>%
        summarize(Quantity = sum(amount))

      dat %>% 
        highcharter::data_to_hierarchical(group_vars = c(a, x), size_var = pop) %>%
        hchart(type = "treemap") %>%
        hc_plotOptions(treemap = list(events = list(click = click_js)), allowDrillToNode = TRUE)
      
      
      highchart() %>%
        hc_xAxis(type = "category") %>%
        hc_add_series(tibbled, "column", hcaes(x = category, y = Quantity), color = "#E4551F") %>%
        hc_plotOptions(column = list(stacking = "normal", events = list(click = pointClickFunction)))
    
  })
  # output$Working <-  renderHighchart({
  # 
  #   
  #   x_level <- "Farm"
  #   
  #   sub <- dat[dat$x == x_level, ]
  #   
  # 
  #   data.frame(category = sub$y, amount = sub$a) %>% 
  #     group_by(category) %>%
  #     summarize(Quantity = sum(amount))
  #   
  #   
  #   highchart() %>%
  #     hc_xAxis(type = "category") %>%
  #     hc_add_series(dat, "column", hcaes(x = name, y = y), color = "#E4551F") %>%
  #     hc_plotOptions(column = list(stacking = "normal", events = list(click = pointClickFunction)))
  # })
  
  
}
  
shinyApp(ui, server)