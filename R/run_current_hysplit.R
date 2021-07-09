#! /usr/bin/env Rscript

#"C:\Users\dkvale\Documents\R\R-3.5.2\bin\i386\Rscript.exe" --no-save --no-restore "X:\Agency_Files\Outcomes\Risk_Eval_Air_Mod\_Air_Risk_Evaluation\Staff folders\Dorian\AQI\aircast\R\run_current_hysplit.R"

tryCatch(library(SplitR), error = function(e) NA) #devtools::install_github("rich-iannone/SplitR")
tryCatch(library(splitr), error = function(e) NA)
library(dplyr)
library(readr)
library(tidyr)
library(here)
library(lubridate)


aircast_path  <- "https://raw.githubusercontent.com/dKvale/aircast/master/"
aqiwatch_path <- "https://raw.githubusercontent.com/dKvale/aqi-watch/master/"


if (F) {
  #aircast_path  <- "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff folders/Dorian/AQI/aircast/"
  results_path  <- "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Current forecast/"
  hysplit_path  <- "C:/users/dkvale/Desktop/aircast/hysplit/"
  
  
  #Java path
  Sys.setenv(JAVA_HOME="C:/Program Files (x86)/Java/jre1.8.0_181")
  
  current_time <- as.numeric(format(Sys.time(), "%H"))
  
  # Check file size function
  min_exists <- function(file_name, min_size = 7.2E+8) { 
    
    file.exists(file_name) & file.size(file_name) > min_size
  }
  
  # Load site locations
  aqi_sites <- read_csv(paste0(aircast_path, "data/monitors_and_wx_stations.csv"))
  
  names(aqi_sites) <- gsub(" ", "_", tolower(names(aqi_sites)))
  
  source(paste0(aircast_path, "R/hysplit_traj.R"))
  source(paste0(aircast_path, "R/get_cmaq_forecast.R"))
  
}

setwd("~")
setwd(hysplit_path)
  
# Set elevated trajectory height
elev_ht <- 500


# Chefck if HYSPLIT has already run
if (!min_exists(paste0(results_path, "/", Sys.Date(), "_AQI_raw_HYSPLIT.csv"), min_size = 100)) {

# Load site locations
sites <- aqi_sites

# Filter to one site per forecast city
sites <- filter(sites, !site_catid %in% c('27-017-7416'))


# Trajectory function to read all sites
aqi_traj <- function(date            = NULL, 
                     receptor_height = NULL, 
                     traj_hours      = NULL,
                     met_dir         = hysplit_path,
                     met_list        = met
                     ) {
  
  # Trajectory table
  traj_forecast <- tibble()
    
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
                             met_dir      = met_dir,
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
    traj$traj_sunflux     <- sum(traj$sun_flux, na.rm = T) %>% round()
    
    traj <- select(traj, -rainfall, -rh, -sun_flux)
    
    traj <- filter(traj, `hour.inc` %in% c(-24, -48, -72))
    
    Sys.sleep(0.25)
    
    traj_forecast <- bind_rows(traj, traj_forecast)
  }
  
  return(traj_forecast)
  
}

# of days in the past, Zero is today
days_past <- 0

today <- Sys.Date() - days_past

# Today
forecast_day  <- "day0"

met      <- c("__today/hysplit.t12z.namsf",
              "__today/hysplit.t12z.namsa",
              "__today/hysplit.t06z.namsf", 
              "__today/hysplit.t06z.namsa")

# Drop missing met data
met <- met[min_exists(met)]

closeAllConnections()

start.time <- Sys.time()

back_forecast <- aqi_traj(date            = today, 
                          receptor_height = 10,
                          traj_hours      = 24,
                          met_list        = met)

end.time <- Sys.time()
end.time - start.time

back_forecast <- bind_rows(back_forecast, 
                           aqi_traj(date            = today, 
                                    receptor_height = elev_ht, 
                                    traj_hours      = 24,
                                    met_list        = met))

 # Tomorrow
forecast_day  <- "day1"

day1 <- tryCatch(aqi_traj(date            = today + 1, 
                          receptor_height = 10,
                          traj_hours      = 24,
                          met_list        = met),
                 error = function(err) NA, silent = T)

# If fail, use namsf      
if (is.na(day1)) {
  
  met   <- c("__today/hysplit.t12z.namf", 
                  "__today/hysplit.t12z.nama",
                  "__today/hysplit.t06z.namf", 
                  "__today/hysplit.t06z.nama")
  
  # Drop missing met data
  met   <- met[min_exists(met)]
  
  # Run HYSPLIT
  day1 <- aqi_traj(date            = today + 1, 
                   receptor_height = 10, 
                   traj_hours      = 24)
  
} 

# Join
back_forecast <- bind_rows(back_forecast, day1)


# Elevated background
back_forecast <- bind_rows(back_forecast, 
                           aqi_traj(date            = today + 1, 
                                    receptor_height = elev_ht,
                                    traj_hours      = 24,
                                    met_list        = met))

# 2 days ahead
forecast_day  <- "day2"

met      <- c("__today/hysplit.t12z.namf", 
              "__today/hysplit.t12z.nama",
              "__today/hysplit.t06z.namf", 
              "__today/hysplit.t06z.nama")

# Drop missing met data
met      <- met[min_exists(met)]

back_forecast <- bind_rows(back_forecast, 
                           aqi_traj(date            = today + 2, 
                                    receptor_height = 10,
                                    traj_hours      = 48,
                                    met_list        = met))

back_forecast <- bind_rows(back_forecast, 
                           aqi_traj(date            = today + 2, 
                                    receptor_height = elev_ht,
                                    traj_hours      = 48,
                                    met_list        = met))

# 3 days ahead
forecast_day  <- "day3"

back_forecast <- bind_rows(back_forecast, 
                           aqi_traj(date            = today + 3,
                                    receptor_height = 10, 
                                    traj_hours      = 72,
                                    met_list        = met))

day3_elev     <- aqi_traj(date            = today + 3, 
                          receptor_height = elev_ht, 
                          traj_hours      = 72,
                          met_list        = met)

back_forecast <- bind_rows(back_forecast, day3_elev)

bk <- back_forecast

# Filter to start location
back_forecast <- filter(back_forecast, as.Date(date2) %in% c(Sys.Date() - days_past, Sys.Date() - 1 - days_past))


# Organize
back_forecast$run_date <- today

back_forecast$row_id <- 1:nrow(back_forecast)

back_forecast <- select(back_forecast, row_id, run_date, everything())


# Save HYSPLIT results
back_forecast

write_csv(back_forecast, 
          paste0(results_path, "/", Sys.Date() - days_past, "_AQI_raw_HYSPLIT.csv"))

unlink(list.files()[grepl("traj-", list.files())], force = T, recursive = T)

}

##
