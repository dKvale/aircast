#! /usr/bin/env Rscript


current_time <- as.numeric(format(Sys.time(), "%H"))


if(current_time <= 10) {
  
  # Grab yesterdays actuals
  print("Downloading yesterday actuals")
  setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Current forecast/hysplit")
  try(source("grab_actuals.R"))
  
  # Update verification table
  print("Updating verification table")
  setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Current forecast/hysplit")
  try(source("update_verification.R"))
  
  
  # Email yesterday's monitoring results
  print("E-mailing yesterday's verification")
  setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Current forecast/hysplit")
  try(source("email_verification.R"))
  
  # Update MET archive from DarkSky
  print("Updating MET archive from DarkSky")
  setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Current forecast/hysplit")
  try(source("update_MET_archive.R"))
  
}

if(current_time >= 10 & current_time < 12) {
  
  # Update background trajectories
  setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Current forecast/hysplit")
  try(source("update_nams_forecast.R"))
  
}


if(current_time == 12) {
  
  # Update background trajectories (backup in case NOAA files did not download)
  setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Current forecast/hysplit")
  try(source("check_nams_forecast.R"))
  
  # Send e-mail alert if today's background NAM data is missing
  setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Current forecast/hysplit")
  try(source("background_alert_msg.R"))
  
}


if(current_time > 12) {
  
  # Grab Sonoma's forecast
  #print("Downloading Sonoma")
  #setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Current forecast/hysplit")
  #try(source("grab_sonoma_forecast.R"))
  
  # Grab CMAQ forecast
  print("Downloading CMAQ")
  setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Current forecast/hysplit")
  try(source("grab_cmaq.R"))
  
  
  # Run HYSPLIT model
  print("Running CMAQ")
  setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Current forecast/hysplit")
  try(source("run_current_hysplit.R"))
  
}


##