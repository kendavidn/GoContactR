input <- list()
output <- list()

todays_date <- as.Date("2021-03-06")

input$snake_plot_slider <- c(4,53)

input$snake_plot_date_slider <- 
  as.Date(c("2021-01-01","2021-03-01"))

input$preloaded_data <- "Guinea list 03_14"


input$snake_plot_slider_regional <- c(4,53)


region <- "Dubreka"




input$contacts_tab_select_regional <- "Nzerekore" 

todays_date_reactive <- 
  function()return(todays_date)

todays_date_reactive_regional <- 
  function()return(todays_date)


read_file_raw <- 
  function(){return(contacts_df_raw)}

read_file <- 
  function(){
    
    return(list(contacts_df_raw =contacts_df_raw,
                contacts_df = contacts_df,
                contacts_df_long = contacts_df_long))
    
    
  }

data_to_plot %>% 
  group_by(etat_suivi) %>%
  e_charts(follow_up_date) %>%
  e_scatter(row_id, bind = hc_ttip, symbol_size = 5) %>%
  e_tooltip(trigger = "item") %>% 
  e_tooltip(formatter = htmlwidgets::JS("
                                        function(params){
                                        return(params.name) }")) %>% 
  e_datazoom() %>% 
  e_color(unique(data_to_plot$colors))


# iris_one_thousand <- 
#   iris %>% 
#   slice(rep(row_number(), 40)) %>% 
#   mutate(Sepal.Length = Sepal.Length +  runif(nrow(.), 0, 10) ) %>% 
#   mutate(Sepal.Width = Sepal.Width +  runif(nrow(.), 0, 10) )
# 
# nrow(iris_one_thousand)
# 
# iris_one_thousand %>% 
#   hchart("scatter", hcaes(Sepal.Length, Sepal.Width, color = Species))
# 
# 
# iris_one_thousand %>% 
#   ggplot() + 
#   geom_point(aes(Sepal.Length, Sepal.Width, color = Species))
# 
# (iris_one_thousand %>% 
#   ggplot() + 
#   geom_point(aes(Sepal.Length, Sepal.Width, color = Species))) %>% 
#   plotly::ggplotly()
# 
# library(echarts4r)
# iris_one_thousand %>% 
#   group_by(Species) %>% 
#   e_charts(Sepal.Length) %>% 
#   e_scatter(Sepal.Width) %>% 
#   e_tooltip(trigger = "item") %>% 
#   e_datazoom(type = "slider")
#   

  
