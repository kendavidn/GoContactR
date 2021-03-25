library(here)
source(here("global.R"), local = TRUE, encoding = "UTF-8")
source(here("ui.R"), local = TRUE, encoding = "UTF-8")
source(here("server.R"),  local = TRUE, encoding = "UTF-8")

# Run the application
shinyApp(ui = ui, server = server)



