#! /usr/bin/env Rscript

library(readr)

aircast_path  <- "https://raw.githubusercontent.com/dKvale/aircast/master/"
aqiwatch_path <- "https://raw.githubusercontent.com/dKvale/aqi-watch/master/"
results_path  <- "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Current forecast/"
hysplit_path  <- "C:/users/dkvale/Desktop/aircast/hysplit/"
gmail_path    <- "../Desktop/credents"

#Java path
Sys.setenv(JAVA_HOME="C:/Program Files (x86)/Java/jre1.8.0_181")


# AirNow credentials
creds <- read_csv("C:/Users/dkvale/Desktop/credents/credentials.csv")


# Check file size function
min_exists <- function(file_name, min_size = 7.2E+8) { 
  
  file.exists(file_name) & file.size(file_name) > min_size
  
}

# Load site locations
aqi_sites <- read_csv(paste0(aircast_path, 
                             "data/monitors_and_wx_stations.csv"))

names(aqi_sites) <- gsub(" ", "_", tolower(names(aqi_sites)))


current_time <- as.numeric(format(Sys.time(), "%H"))


if(current_time <= 10) {
  
  # Grab yesterdays actuals
  print("Downloading yesterday actuals...")
  try(source(paste0(aircast_path, "R/get_aqi_results.R")))
  
  # Update verification table
  print("Updating verification table...")
  try(source(paste0(aircast_path, "R/update_verification.R")))
  
  # Email yesterday's monitoring results
  print("E-mailing yesterday's verification...")
  try(source(paste0(aircast_path, "R/email_verification.R")))
  
  # Update MET archive from DarkSky
  print("Updating MET archive from DarkSky...")
  try(source(paste0(aircast_path, "R/update_MET_archive.R")))
  
  # Update AQI history file
  #print("Updating site history")
  #setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Web/aqi-dashboard/")
  #try(source("R/update_aqi_history.R"))

}

if(current_time >= 10 & current_time < 12) {
  
  # Update background NAMS data
  print("Downloading NAMS data...")
  try(source(paste0(aircast_path, "R/update_nams_forecast.R")))
  
}


if(current_time == 12) {
  
  # Update background NAMS data (backup in case NOAA files did not download)
  print("Check background download...")
  try(source(paste0(aircast_path, "R/check_nams_forecast.R")))
  
  # Send e-mail alert if today's background NAM data is missing
  #try(source(paste0(aircast_path, "R/background_alert_msg.R")))
  
  # Run HYSPLIT model
  print("Running HYSPLIT...")
  try(source(paste0(aircast_path, "R/hysplit_traj.R")), silent = T)
  try(source(paste0(aircast_path, "R/run_current_hysplit.R")))
  #try(source(paste0(aircast_path, "R/run_current_hysplit_pt1.R")))
  #try(source(paste0(aircast_path, "R/run_current_hysplit_pt2.R")))
  #try(source(paste0(aircast_path, "R/run_current_hysplit_combine.R")))
  
  # Attach background monitoring results for 16z
  print("Attaching 16Z monitoring results...")
  try(source(paste0(aircast_path, "R/update_background_aqi.R")))
  
  hys <- read_csv(paste0(results_path, "/", Sys.Date(), "_16z_AQI_background.csv"))
  try(hys <- dplyr::select(hys, -row_id, -date), silent = T)
  
  write.csv(hys, paste0(results_path, "/", Sys.Date(), "_16z_AQI_background.csv"), row.names = F)
  
  # Grab CMAQ forecast
  #print("Downloading CMAQ")
  #try(source(paste0(aircast_path, "R/update_cmaq_forecast.R")))
  
}


if(current_time > 12 & current_time < 15) {
  
  # Attach background monitoring results for 17z
  print("Attaching 17Z monitoring results")
  try(source(paste0(aircast_path, "R/update_background_aqi.R")))
  
  hys <- read_csv(paste0(results_path, "/", Sys.Date(), "_17z_AQI_background.csv"))
  
  try(hys <- dplyr::select(hys, -row_id, -date), silent = T)
  
  # SAVE results
  write.csv(hys, paste0(results_path, "/", Sys.Date(), "_17z_AQI_background.csv"), row.names = F)
  
  # E-mail background results
  print("E-mailing background results")
  try(source(paste0(aircast_path, "R/email_background.R")))
  
  # Grab CMAQ forecast
  print("Downloading CMAQ")
  try(source(paste0(aircast_path, "R/update_cmaq_forecast.R")))
  
}


if(current_time >= 15) {
  
  # Empty airvision FTP folder
  print("Deleting FTP airvision files...")
  try(source(paste0(aircast_path, "R/clear_airvision_ftp.R")))
  
  # Update forecast maps
  #print("Updating forecast maps")
  #setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Web/aqi-dashboard/")
  #try(source("R/update_forecast_maps.R"))

}

##