#! /usr/bin/env Rscript

library(downloader)
library(magrittr)

# if (computer == "desktop) {  }

source(paste0(aircast_path, "R/get_nams_forecast.R"))


setwd("~")
setwd("../Desktop/aircast/hysplit/__today")


# Set file date as today
new_date <- Sys.Date()


# Retry downloading new forecasts if files missing or empty

# NAMf
if(!min_exists("hysplit.t12z.namf", min_size = 1.2E+9)) {
  
  nam <- tryCatch(get_nams_forecast(date = new_date, folder = ".", type = "f", time_step = 3), 
                  error = function(err) NA, 
                  silent = T)
  
# If fail, download 6z       
  if(!is.null(nam)) { 
    try(get_nams_forecast(date = new_date, folder = ".", type = "f", time_step = 3, hour = '06'))
 }
}

# NAMa
if(!min_exists("hysplit.t12z.nama", min_size = 7.3E+8)) {
  
  nam <- tryCatch(get_nams_forecast(date = new_date, folder = ".", type = "a", time_step = 3), 
                  error = function(err) NA, 
                  silent = T)
  
  # If fail, download 6z       
  if(is.na(nam)) { 
    try(get_nams_forecast(date = new_date, folder = ".", type = "a", time_step = 3, hour = '06'))
  }
}

# NAMsa
if(!min_exists("hysplit.t12z.namsa", min_size = 1.1E+9)) {
  
  nam <- tryCatch(get_nams_forecast(date = new_date, folder = ".", type = "a", time_step = 1), 
                  error = function(err) NA, 
                  silent = T)
  
  # If fail, download 6z       
  if(is.na(nam)) { 
    try(get_nams_forecast(date = new_date, folder = ".", type = "a", time_step = 1, hour = '06'))
  }
}

# NAMsf
if(!min_exists("hysplit.t12z.namsf", min_size = 2.35E+9)) {
  
  nam <- tryCatch(get_nams_forecast(date = new_date, folder = ".", type = "f", time_step = 1), 
                  error = function(err) NA, 
                  silent = T)
  
  # If fail, download 6z       
  if(is.na(nam)) { 
    try(get_nams_forecast(date = new_date, folder = ".", type = "f", time_step = 1, hour = '06'))
  }
}

closeAllConnections()


##

