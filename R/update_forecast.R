#! /usr/bin/env Rscript


aircast_path <- "https://raw.githubusercontent.com/dKvale/aircast/master/R/"

current_time <- as.numeric(format(Sys.time(), "%H"))


if(current_time <= 10) {
  
  # Grab yesterdays actuals
  print("Downloading yesterday actuals")
  try(source(paste0(aircast_path, "get_aqi_results.R")))
  
  # Update verification table
  print("Updating verification table")
  try(source(paste0(aircast_path, "update_verification.R")))
  
  # Email yesterday's monitoring results
  print("E-mailing yesterday's verification")
  try(source(paste0(aircast_path, "email_verification.R")))
  
  # Update MET archive from DarkSky
  print("Updating MET archive from DarkSky")
  try(source(paste0(aircast_path, "update_MET_archive.R")))
  
  # Update AQI history file
  #print("Updating site history")
  #setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Web/aqi-dashboard/R")
  #try(source("update_aqi_history.R"))

}

if(current_time >= 10 & current_time < 12) {
  
  # Update background NAMS data
  print("Downloading NAMS data...")
  try(source(paste0(aircast_path, "update_nams_forecast.R")))
  
}


if(current_time == 12) {
  
  # Update background NAMS data (backup in case NOAA files did not download)
  print("Check background download...")
  try(source(paste0(aircast_path, "check_nams_forecast.R")))
  
  # Send e-mail alert if today's background NAM data is missing
  try(source(paste0(aircast_path, "background_alert_msg.R")))
  
  # Run HYSPLIT model
  print("Running HYSPLIT...")
  try(source(paste0(aircast_path, "run_current_hysplit.R")))
  
  # Attach background monitoring results for 16z
  print("Attaching monitoring results...")
  try(source(paste0(aircast_path, "update_background_aqi.R")))
  
}


if(current_time > 12 & current_time < 15) {
  
  # Grab CMAQ forecast
  print("Downloading CMAQ")
  try(source(paste0(aircast_path, "update_cmaq_forecast.R")))
  
  # Attach background monitoring results for 17z
  print("Attaching monitoring results")
  try(source(paste0(aircast_path, "update_background_aqi.R")))
  
  # E-mail background results
  print("E-mailing background results")
  try(source(paste0(aircast_path, "email_background.R")))
  
  # Empty airvision FTP folder
  print("Deleting FTP airvision files...")
  try(source(paste0(aircast_path, "clear_airvision_ftp.R")))
  
}


if(current_time >= 15) {
  

# Update forecast maps
#print("Updating forecast maps")
#setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Web/aqi-dashboard/R")
#try(source("update_forecast_maps.R"))

}

##