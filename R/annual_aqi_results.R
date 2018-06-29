
library(stringr)
library(RCurl)
library(tidyverse)
library(readr)


aircast_path  <- "https://raw.githubusercontent.com/dKvale/aircast/master/"
aqiwatch_path <- "https://raw.githubusercontent.com/dKvale/aqi-watch/master/"
results_path  <- "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/"

# AirNow credentials
creds <- read_csv("C:/Users/dkvale/Desktop/credentials.csv")


# Load get_airnow() function
source(paste0(aircast_path, "R/get_airnow.R"))


# Load site locations
aqi_sites <- read_csv(paste0(aircast_path, "data/monitors_and_wx_stations.csv"))

names(aqi_sites) <- gsub(" ", "_", tolower(names(aqi_sites)))

# Drop outstate sites
sites <- filter(aqi_sites, !fcst_region %in% c("CA", "ND", "SD", "WI", "IA"))


# Drop dashes in IDs to match AQS
sites$aqsid <- gsub("-", "", sites$site_catid)

# Filter to one site per forecast city
sites <- filter(sites, !site_catid %in% c('27-017-7416'))


# Get dates
date_list <- seq(as.Date("2018-01-01"), as.Date("2018-06-28"), 1)


# Site list
site_list <- c(sites$aqsid, gsub("-", "", sites$alt_siteid))

# Pollutant list
pollutant_list <- c("OZONE-8HR", "PM2.5-24hr")


# Download results from AirNow
aqi <- get_airnow(date_list, site_list[!is.na(site_list)], pollutant_list) %>% 
       select(-Units, -Hours, -Agency)

# Save results
write_csv(aqi, paste0(results_path, 
                      "Verification/AQI History/", 
                      paste(unique(format(date_list, "%Y")), sep = ", "), "_annual_aqi_obs.csv"))


#