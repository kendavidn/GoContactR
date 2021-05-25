

## First run global.R


## THen run these to manually create inputs 
## that would normally be entered with shiny 

data_to_use <-  "Use preloaded data"
preloaded_data_choice <- "Sample tracing data"
select_region <- "Abidjan 1"
todays_date <- Sys.Date()


## then run all read_... functions in server_functions



## then run the below
contacts_df <- read_file()$contacts_df_long
contacts_df_long <- read_file()$contacts_df_long



## finally, you can run the guts of each server_function


