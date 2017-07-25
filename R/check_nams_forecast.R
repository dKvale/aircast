#! /usr/bin/env Rscript

library(downloader)
library(reports)
library(magrittr)

setwd("~")
setwd("../Desktop/hysplit/__today")

source("X:\\Agency_Files\\Outcomes\\Risk_Eval_Air_Mod\\_Air_Risk_Evaluation\\Staff folders\\Dorian\\AQI\\aircast\\R\\get_nams_forecast.R")


# Create minimum exists function
# Checks if file exists and if meets minimum file size
min_exists <- function(file_name, min_size = 7E+8) { 
  
  file.exists(file_name) & file.size(file_name) > min_size
  
  }

# Set file date as today
new_date <- Sys.Date()


# Retry downloading new forecasts if files missing or empty

# NAMf
if(!min_exists("hysplit.t12z.namf")) {
  
  nam <- tryCatch(get_nams_forecast(date = new_date, folder = ".", type = "f", time_step = 3), error = function(err) NA, silent = T)
  
# If fail, download 6z       
  if(!is.null(nam)) { 
    try(get_nams_forecast(date = new_date, folder = ".", type = "f", time_step = 3, hour = '06'))
 }
}

# NAMa
if(!min_exists("hysplit.t12z.nama")) {
  
  nam <- tryCatch(get_nams_forecast(date = new_date, folder = ".", type = "a", time_step = 3), error = function(err) NA, silent = T)
  
  # If fail, download 6z       
  if(is.na(nam)) { 
    try(get_nams_forecast(date = new_date, folder = ".", type = "a", time_step = 3, hour = '06'))
  }
}

# NAMsa
if(!min_exists("hysplit.t12z.namsa")) {
  
  nam <- tryCatch(get_nams_forecast(date = new_date, folder = ".", type = "a", time_step = 1), error = function(err) NA, silent = T)
  
  # If fail, download 6z       
  if(is.na(nam)) { 
    try(get_nams_forecast(date = new_date, folder = ".", type = "a", time_step = 1, hour = '06'))
  }
}

# NAMsa
if(!min_exists("hysplit.t12z.namsf")) {
  
  nam <- tryCatch(get_nams_forecast(date = new_date, folder = ".", type = "f", time_step = 1), error = function(err) NA, silent = T)
  
  # If fail, download 6z       
  if(is.na(nam)) { 
    try(get_nams_forecast(date = new_date, folder = ".", type = "f", time_step = 1, hour = '06'))
  }
}

closeAllConnections()


##

