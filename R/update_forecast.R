#! /usr/bin/env Rscript


current_time <- as.numeric(format(Sys.time(), "%H"))


if(current_time <= 10) {
  
  # Grab yesterdays actuals
  print("Downloading yesterday actuals")
  setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/aircast/R")
  try(source("get_aqi_results.R"))
  
  # Update verification table
  print("Updating verification table")
  setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/aircast/R")
  try(source("update_verification.R"))
  
  
  # Email yesterday's monitoring results
  print("E-mailing yesterday's verification")
  setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/aircast/R")
  try(source("email_verification.R"))
  
  # Update MET archive from DarkSky
  print("Updating MET archive from DarkSky")
  setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/aircast/R")
  try(source("update_MET_archive.R"))
  
}

if(current_time >= 10 & current_time < 12) {
  
  # Update background NAMS data
  print("Downloading NAMS data...")
  setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/aircast/R")
  try(source("update_nams_forecast.R"))
  
}


if(current_time == 12) {
  
  # Update background NAMS data (backup in case NOAA files did not download)
  setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/aircast/R")
  try(source("check_nams_forecast.R"))
  
  # Send e-mail alert if today's background NAM data is missing
  setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/aircast/R")
  try(source("background_alert_msg.R"))
  
  
  # Run HYSPLIT model
  print("Running HYSPLIT")
  setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/aircast/R")
  try(source("run_current_hysplit.R"))
  
}


if(current_time > 12) {
  
  # Grab CMAQ forecast
  print("Downloading CMAQ")
  setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/aircast/R")
  try(source("update_cmaq_forecast.R"))
  
  
  # Attach background monitoring results
  print("Attaching monitoring results")
  setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/aircast/R")
  try(source("update_background_aqi.R"))
  
  
  # E-mail background results
  print("E-mailing background results")
  setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/aircast/R")
  try(source("email_background.R"))
  
}


##