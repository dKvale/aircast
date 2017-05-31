#! /usr/bin/env Rscript

library(rvest)
library(stringr)
library(readr)
library(dplyr)
library(tidyr)
library(methods)


setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Current forecast/hysplit")
source("cmaq_forecast.R")

sites <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff folders/Dorian/AQI/MET data/Monitors and Rep Wx Stations.csv")

names(sites) <- gsub(" ", "_", tolower(names(sites)))

# Filter to one site per forecast city
sites <- filter(sites, !site_catid %in% c('27-017-7416'))

# Loop through all sites
cmaq_all <- data_frame()

for(i in 1:nrow(sites)) {
  
  # Load CMAQ site
  site     <- sites[i, ]
  
  print(site$air_monitor)
  
  o3_max   <- cmaq_forecast(site$monitor_lat, site$monitor_long, hour_gmt = NULL)
  
  o3_max$site_catid <- site$site_catid
  
  cmaq_all <- bind_rows(o3_max, cmaq_all)
  
}

# Save
setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Current forecast")
  
write.csv(cmaq_all, paste0(Sys.Date(), "_CMAQ_forecast", ".csv"), row.names = F)

##