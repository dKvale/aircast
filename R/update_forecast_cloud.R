#! /usr/bin/env Rscript

library(readr)

aircast_path  <- "https://raw.githubusercontent.com/dKvale/aircast/master/"
aqiwatch_path <- "https://raw.githubusercontent.com/dKvale/aqi-watch/master/"
results_path  <- paste0(getwd(), "/results/")
hysplit_path  <- getwd()
gmail_path    <- getwd()


# Check file size function
min_exists <- function(file_name, min_size = 7.2E+8) { 
  
  file.exists(file_name) & file.size(file_name) > min_size
  
}

# Load site locations
aqi_sites <- read_csv(paste0(aircast_path, "data/monitors_and_wx_stations.csv"))

names(aqi_sites) <- gsub(" ", "_", tolower(names(aqi_sites)))

current_time <- as.numeric(format(Sys.time(), "%H"))

print(paste0("The current time is ", current_time))


if(current_time < 13) {
  
  # Update background NAMS data
  print("Downloading NAMS data...")
  try(source(paste0(aircast_path, "R/update_nams_forecast.R")), silent = T)
  
  
  # Update background NAMS data (backup in case NOAA files did not download)
  print("Check background download...")
  try(source(paste0(aircast_path, "R/check_nams_forecast.R")), silent = T)
  
  
  # Run HYSPLIT model
  print("Running HYSPLIT...")
  try(source(paste0(aircast_path, "R/hysplit_traj_cloud.R")), silent = T)
  try(source(paste0(aircast_path, "R/run_current_hysplit.R")), silent = T)
  
  
  # Attach background monitoring results for 16z
  print("Attaching 16Z monitoring results...")
  try(source(paste0(aircast_path, "R/update_background_aqi.R")), silent = T)
}


if (current_time >= 12) {
  
  # Update background NAMS data (backup in case NOAA files did not download)
  print("Check background download...")
  try(source(paste0(aircast_path, "R/check_nams_forecast.R")), silent = T)
     
  # Run HYSPLIT model
  print("Running HYSPLIT...")
  try(source(paste0(aircast_path, "R/hysplit_traj_cloud.R")), silent = T)
  try(source(paste0(aircast_path, "R/run_current_hysplit.R")), silent = T)
  
  # Attach background monitoring results for 17z
  print("Attaching 17Z monitoring results")
  try(source(paste0(aircast_path, "R/update_background_aqi.R")), silent = T)
  
  # E-mail background results
  #print("E-mailing background results")
  #try(source(paste0(aircast_path, "R/email_background.R")))
  
}

##
