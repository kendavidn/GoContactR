# R Shiny App for analysing Contact Tracing Data

This repo contains an R-shiny application built to allow contact tracing teams to generate automated reports that summarize contact follow-up data.

The application has been built to work with two primary data sources: data from KoboCollect-exported csv files, and data from a Go.Data instance (by direct API connection). 

At the start of the global.R file, the developer sets the `PARAMS$country_code` variable; this determines whether the Go.Data or KoboCollect versions will be loaded. 

![key_files_and_folders](https://raw.githubusercontent.com/kendavidn/GoContactR/master/www/key_files_and_folders.png)

## Folder structure

The application is split into the *global.R*, *server.R* and *ui.R* files. 
The *server* file sources its functions from the *helper_scripts* folder.


## Data Flow

The application is fairly straightforward from a shiny reactivity standpoint. 
A single dataset is loaded in with the `read_file_raw` function, processed with the `read_file_transformed` function, filtered with the `read_file_filtered` function, then passed on to all the application outputs. 

![gocontactr_data_flow](https://raw.githubusercontent.com/kendavidn/GoContactR/www/gocontactr_data_flow.png)


