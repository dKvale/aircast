# Historical HYSPLIT runs using NARR data

#devtools::install_github("rich-iannone/SplitR")
library(SplitR)
library(dplyr)
library(readr)
library(geosphere)
library(measurements)
library(readxl)
library(tidyr)
library(stringr)

setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/aircast/R/")
source("hysplit_traj.R")

# Load site locations
sites <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff folders/Dorian/AQI/MET data/Monitors and Rep Wx Stations.csv")

names(sites) <- gsub(" ", "_", tolower(names(sites)))

# Filter to one site per forecast city
sites <- filter(sites, !site_catid %in% c('27-017-7416'))


setwd("C:/Users/dkvale/Desktop/archive_hysplit")

# Trajectory function to read all sites
aqi_traj <- function(date            = NULL, 
                     receptor_height = NULL, 
                     traj_hours      = NULL) {
  
  # Trajectory table
  traj_forecast <- data_frame()
  
  for(site in unique(sites$site_catid)) {
    
    site_df <- filter(sites, site_catid == site)
    
    print(site_df$site_catid)
    
    traj    <-  hysplit_traj(lat          = round(site_df$monitor_lat, 3),
                             lon          = round(site_df$monitor_long, 3),
                             height       = receptor_height,
                             duration     = traj_hours,
                             run_period   = as.character(date),
                             daily_hours  = 17,
                             direction    = "backward",
                             met_type     = "NARR",
                             met_dir      = "C:/Users/dkvale/Desktop/archive_hysplit/NARR",
                             extended_met = F,
                             vert_motion  = 0,
                             model_height = 20000,
                             traj_name    = as.character(round(runif(1), 5)),
                             met_files    = met_list) 
    
    
    traj$site_catid      <- site
    traj$receptor_height <- receptor_height
    
    traj <- filter(traj, hour.inc %in% c(-24))
    
    Sys.sleep(0.7)
    
    traj_forecast <- bind_rows(traj, traj_forecast)
  }
  
  return(traj_forecast)
  
}

# Years to run
years <- 2015


# Days to run
days <- data_frame(date = seq(as.Date(paste0(min(years), "-01-01")), 
                              as.Date(paste0(max(years), "-12-31")), 1))

days2 <- data_frame(date = seq(as.Date(paste0(min(years)+1, "-01-01")), 
                              as.Date(paste0(max(years)+1, "-12-31")), 1))


hys_archive <- data_frame()

# Loop through days
for(i in 1:nrow(days)) {
  
  day <- days$date[i]
  
  met_list     <- paste0("NARR", unique(c(format(day-18, "%Y%m"), format(day, "%Y%m"), format(day+18, "%Y%m"))))
  
  hys_day_10m  <- aqi_traj(date = day, receptor_height = 10, traj_hours = 24)
  
  hys_day_500m <- aqi_traj(date = day, receptor_height = 500, traj_hours = 24)

  hys_archive  <- bind_rows(hys_day_500m, hys_day_10m, hys_archive)
  
  closeAllConnections()
  
}

# Loop through days2
hys_archive2 <- data_frame()

for(i in 1:nrow(days2)) {
  
  day <- days2$date[i]
  
  print(day)
  
  met_list     <- paste0("NARR", unique(c(format(day-18, "%Y%m"), format(day, "%Y%m"), format(day+18, "%Y%m"))))
  
  hys_day_10m  <- aqi_traj(date = day, receptor_height = 10, traj_hours = 24)
  
  hys_day_500m <- aqi_traj(date = day, receptor_height = 500, traj_hours = 24)
  
  hys_archive2  <- bind_rows(hys_day_500m, hys_day_10m, hys_archive2)
  
  closeAllConnections()
  
}


saveRDS(hys_archive, paste0("HYSPLIT archive using NARR data - ", min(years), ".rdata"))

saveRDS(hys_archive2, paste0("HYSPLIT archive using NARR data - ", min(years)+1, ".rdata"))


# Join multiple years
hys_all <- bind_rows(hys_archive, hys_archive2)


# Update column names
hys_all <- select(hys_all, -receptor, -pressure)

names(hys_all)[c(5,9:10)] <- c("hours_backward", "origin_date", "receptor_date")

hys_all$time_zone <- "GMT"

# Save results
saveRDS(hys_all, paste0("HYSPLIT archive using NARR data for ", years, "-", years + 1, ".rdata"))


##
