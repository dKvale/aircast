#! /usr/bin/env Rscript

library(downloader)
library(magrittr)
#library(reports)

aircast_path  <- "https://raw.githubusercontent.com/dKvale/aircast/master/"
aqiwatch_path <- "https://raw.githubusercontent.com/dKvale/aqi-watch/master/"
#results_path  <- "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/"

source(paste0(aircast_path, "R/get_nams_forecast.R"))


# Navigate to user Desktop
setwd("~")
setwd(hysplit_path)

# Delete old data
unlink(list.files()[grepl("traj-", list.files())], recursive = T)
unlink("__today", recursive = T)


# Create directory
dir.create("__today", mode = "777")

# Download new forecasts
#today    <- "20170323"
days_past <- 0

new_date <- Sys.Date() - days_past

print(new_date)

# Increase time (in seconds) before file connection is terminated
options(timeout = 120)

# Todays forecast
get_nams_forecast(date = new_date, folder = "__today", type = "a", time_step = 1)
get_nams_forecast(date = new_date, folder = "__today", type = "f", time_step = 1)
get_nams_forecast(date = new_date, folder = "__today", type = "f", time_step = 3)
get_nams_forecast(date = new_date, folder = "__today", type = "a", time_step = 3)

closeAllConnections()

# Yesterday's
#get_forecast_nams(date = new_date - 1, folder = "__yesterday", type = "f", time_step = 3)

#--------------------------------------------#
# For recent trajectories before current day
#--------------------------------------------#
n  <- 0  

if (FALSE) {
  for(n in (0:5)) {
  
  new_date <- Sys.Date() - n - days_past

    if(n == 0) {
      new_date <- Sys.Date() - n 
      print(new_date)
      get_forecast_nams(date = new_date, folder = "__today", type = "a", time_step = 1)
      get_forecast_nams(date = new_date, folder = "__today", type = "f", time_step = 1)
      
      
      get_forecast_nams(date = new_date, folder = "__today", type = "a", time_step = 3)
      get_forecast_nams(date = new_date, folder = "__today", type = "f", time_step = 3)
      
      
    } else {
      print(new_date)
      get_forecast_nams(date = new_date, folder = paste0("_prev", n), type = "a", time_step = 3)
      
    }
  closeAllConnections()
  
  }
}

##
