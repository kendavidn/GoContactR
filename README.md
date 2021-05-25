# R Shiny App for analysing Contact Tracing Data

This repo contains an R-shiny application built to allow contact tracing teams to generate automated reports that summarize contact follow-up data.

The application has been built to work with two primary data sources: data from KoboCollect-exported csv files, and data from a Go.Data instance (by direct API connection). 

At the start of the global.R file, the developer sets the `PARAMS$country_code` variable; this determines whether the Go.Data or KoboCollect versions will be loaded. 

![key_files_and_folders](https://raw.githubusercontent.com/kendavidn/GoContactR/master/www/key_files_and_folders.png)

## Folder structure

The application is split into the *global.R*, *server.R* and *ui.R* files. 
The *server* file sources its functions from the *helper_scripts* folder.


## Data Flow

The data flow for the application is fairly straightforward. 
A single dataset is loaded in with the `read_file_raw` function, processed with the `read_file_transformed` function, filtered with the `read_file_filtered` function, then passed on to all the application outputs. 


<img src="https://imgur.com/a/NiEaPSL"/>

## Key outputs

The application calculates and plots: 

- New contacts over time;
- Total cumulative contacts over time;
- Contacts under surveillance over time;
- % follow-up of contacts;
- Number of contacts per index case;
- Number of contacts per case type; and
- Follow-up state among active contacts

among others


## Report generation

The application generates reports in the following formats:

- Powerpoint document. See example here: 
- Word document. See example here: 
- Flat HTML page; and 
- HTML slides


![gocontactr_data_flow](https://raw.githubusercontent.com/kendavidn/GoContactR/www/gocontactr_data_flow.png)


