#! /usr/bin/env Rscript

library(SplitR) #devtools::install_github("rich-iannone/SplitR")
library(dplyr)
library(readr)
library(tidyr)
library(here)


source(paste0(aircast_path, "R/hysplit_traj.R"))
source(paste0(aircast_path, "R/get_cmaq_forecast.R"))


# Check if HYSPLIT has already run
setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Current forecast")

if (!min_exists(paste0(Sys.Date(), "_AQI_raw_HYSPLIT.csv"), min_size = 100)) {

# Load site locations
sites <- aqi_sites

# Filter to one site per forecast city
sites <- filter(sites, !site_catid %in% c('27-017-7416'))


setwd("~")
setwd("../Desktop/aircast/hysplit")

# Trajectory function to read all sites
aqi_traj <- function(date            = NULL, 
                     receptor_height = NULL, 
                     traj_hours      = NULL
                     ) {
  
  # Trajectory table
  traj_forecast <- data_frame()
    
  for (site in unique(sites$site_catid)) {
  
    site_df <- filter(sites, site_catid == site)
    
    print(site_df$site_catid)
    
    traj    <-  hysplit_traj(lat          = round(site_df$monitor_lat, 3),
                             lon          = round(site_df$monitor_long, 3),
                             height       = receptor_height,
                             duration     = traj_hours,
                             run_period   = as.character(date),
                             daily_hours  = 17,
                             direction    = "backward",
                             met_type     = "NAM",
                             met_dir      = "C:/Users/dkvale/Desktop/aircast/hysplit",
                             extended_met = TRUE,
                             vert_motion  = 0,
                             model_height = 20000,
                             traj_name    = as.character(round(runif(1), 5)),
                             met_files    = met_list) 
                            
    
    traj$receptor        <- site
    traj$receptor_height <- receptor_height
    traj$forecast_day    <- forecast_day
    
    # Aggregate MET observations
    traj$traj_rain_sum    <- sum(traj$rainfall, na.rm = T)
    traj$traj_rain_max    <- max(traj$rainfall, na.rm = T)
    traj$traj_rain_hrs_02 <- sum(traj$rainfall > 0.2, na.rm = T)
    traj$traj_rain_hrs_05 <- sum(traj$rainfall > 0.5, na.rm = T)
    traj$traj_rh          <- mean(traj$rh, na.rm = T) %>% round(1)
    traj$traj_sunflux     <- sum(traj$sunflux, na.rm = T) %>% round()
    
    traj <- select(traj, -rainfall, -rh, -sunflux)
    
    traj <- filter(traj, `hour.inc` %in% c(-24, -48, -72))
    
    Sys.sleep(0.7)
    
    traj_forecast <- bind_rows(traj, traj_forecast)
  }
  
  return(traj_forecast)
  
}

setwd("C:/Users/dkvale/Desktop/aircast/hysplit")

# of days in the past, Zero is today
days_past <- 0 

today <- Sys.Date() - days_past

# Today
forecast_day  <- "day0"

met_list      <- c("__today/hysplit.t12z.namsf", "__today/hysplit.t12z.namsa", "__today/hysplit.t06z.namsf", "__today/hysplit.t06z.namsa")

# Drop missing met data
met_list      <- met_list[min_exists(met_list)]

closeAllConnections()

start.time <- Sys.time()

back_forecast <- aqi_traj(date = today, receptor_height = 10, traj_hours = 24)

end.time <- Sys.time()
end.time - start.time

back_forecast <- bind_rows(back_forecast, aqi_traj(date = today, receptor_height = 500, traj_hours = 24))

# Tomorrow
forecast_day  <- "day1"

day1 <- tryCatch(aqi_traj(date = today + 1, receptor_height = 10, traj_hours = 24), error = function(err) NA, silent = T)

# If fail, use namsf      
if (is.na(day1)) {
  
  met_list   <- c("__today/hysplit.t12z.namf", "__today/hysplit.t12z.nama", "__today/hysplit.t06z.namf", "__today/hysplit.t06z.nama")
  
  # Drop missing met data
  met_list   <- met_list[min_exists(met_list)]
  
  # Run HYSPLIT
  day1 <- aqi_traj(date = today + 1, receptor_height = 10, traj_hours = 24)
  
} 

# Join
back_forecast <- bind_rows(back_forecast, day1)

# 500 meter background
back_forecast <- bind_rows(back_forecast, aqi_traj(date = today + 1, receptor_height = 500, traj_hours = 24))


# 2 days ahead
forecast_day  <- "day2"
met_list      <- c("__today/hysplit.t12z.namf", "__today/hysplit.t12z.nama", "__today/hysplit.t06z.namf", "__today/hysplit.t06z.nama")

# Drop missing met data
met_list      <- met_list[min_exists(met_list)]

back_forecast <- bind_rows(back_forecast, aqi_traj(date = today + 2, receptor_height = 10, traj_hours = 48))
back_forecast <- bind_rows(back_forecast, aqi_traj(date = today + 2, receptor_height = 500, traj_hours = 48))

# 3 days ahead
forecast_day  <- "day3"

back_forecast <- bind_rows(back_forecast, 
                           aqi_traj(date = today + 3, receptor_height = 10, traj_hours = 72))

day3_500m     <- aqi_traj(date = today + 3, receptor_height = 500, traj_hours = 72)

back_forecast <- bind_rows(back_forecast, day3_500m)

#back_forecast <- bind_rows(back_forecast, aqi_traj(date = today + 3, receptor_height = 500, traj_hours = 72))


# Filter to start location
back_forecast <- filter(back_forecast, as.Date(date2) %in% c(Sys.Date() - days_past, Sys.Date() - 1 - days_past))


# Save HYSPLIT results
back_forecast

setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Current forecast")

write_csv(back_forecast, paste0(Sys.Date() - days_past, "_AQI_raw_HYSPLIT.csv"))

}

##

