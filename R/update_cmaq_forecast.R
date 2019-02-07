#! /usr/bin/env Rscript

library(rvest)
library(stringr)
library(readr)
library(dplyr)
library(tidyr)
library(methods)


source(paste0(aircast_path, "R/get_cmaq_forecast.R"))


sites <- aqi_sites

# Filter to one site per forecast city
sites <- filter(sites, !site_catid %in% c('27-017-7416'))


# Check if CMAQ has already run
setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Current forecast")

if (!min_exists(paste0(Sys.Date(), "_CMAQ_forecast", ".csv"), min_size = 100)) {
  
# Loop through all sites
cmaq_all <- data_frame()

for(i in 1:nrow(sites)) {
  
  # Load CMAQ site
  site     <- sites[i, ]
  
  print(site$air_monitor)
  
  o3_max   <- get_cmaq_forecast(site$monitor_lat, site$monitor_long, hour_gmt = NULL)
  
  o3_max$site_catid <- site$site_catid
  
  cmaq_all <- bind_rows(o3_max, cmaq_all)
  
}

# Save
setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Current forecast")
  
write.csv(cmaq_all, paste0(Sys.Date(), "_CMAQ_forecast", ".csv"), row.names = F)

}

##