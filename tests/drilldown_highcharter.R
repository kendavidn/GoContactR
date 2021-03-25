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

header <- dashboardHeader()
body <- dashboardBody(
  actionButton("Back", "Back"),
  highchartOutput("Working"),
  verbatimTextOutput("trial")
  
)
sidebar <- dashboardSidebar()

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output, session) {
  # To hold the current drilldown status as list, i.e. list("Farm", "Sheep")
  state <- reactiveValues(drills = list())
  
  
  # Since Drilldown from Highcharts is not used: Install own click handler that builds up the drill list.
  observeEvent(eventExpr = input$ClickedInput,
               handlerExpr = {
                 if (length(state$drills) < 2) {
                   # Push drill name.
                   state$drills <<- c(state$drills, input$ClickedInput)
                 }
               })
  
  # Since Drilldown from Highcharts is not used: Back button is manually inserted.
  observeEvent(eventExpr = input$Back,
               handlerExpr = {
                 if (length(state$drills) > 0) {
                   # Pop drill name.
                   state$drills <<- state$drills[-length(state$drills)]
                 }
               })
  
  # Reactive reacting to the above drill list, giving out a normalized data.frame (category, amount)
  filtered <- reactive({
    if (length(state$drills) == 0) {
      # Case no drills are present.
      data.frame(category = dat$x, amount = dat$a)
      
    } else if (length(state$drills) == 1) {
      # Case only x_level drill is present.
      x_level = state$drills[[1]]
      sub <- dat[dat$x == x_level,]
      data.frame(category = sub$y, amount = sub$a)
      
    } else if (length(state$drills) == 2) {
      # Case x_level and y_level drills are present.
      
      x_level = state$drills[[1]]
      y_level = state$drills[[2]]
      sub <- dat[dat$x == x_level & dat$y == y_level,]
      data.frame(category = sub$z, amount = sub$a)
    }
  })

  
  output$Working <- renderHighchart({
    
    # Using normalized names from above.
    summarized <- filtered() %>%
      group_by(category) %>%
      summarize(Quantity = sum(amount))
    
    summarized <- arrange(summarized, desc(Quantity))
    tibbled <- tibble(name = summarized$category, y = summarized$Quantity)
    
    # This time, click handler is needed.
    pointClickFunction <- JS("function(event) {Shiny.onInputChange('ClickedInput', event.point.name);}")
    
    highchart() %>%
      hc_xAxis(type = "category") %>%
      hc_add_series(tibbled, "column", hcaes(x = name, y = y), color = "#E4551F") %>%
      hc_plotOptions(column = list(stacking = "normal", events = list(click = pointClickFunction)))
  })
  
  output$trial <- renderText({input$ClickedInput})
}

shinyApp(ui, server)