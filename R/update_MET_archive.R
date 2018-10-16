#! /usr/bin/env Rscript

library(purrr)   #devtools::install_github("tidyverse/purrr")
library(dplyr)
library(readr)
library(darksky) #devtools::install_github("hrbrmstr/darksky")

options(digits = 12)

# AQI monitoring sites
sites <- aqi_sites

# DarkSky key
d_key <- 'ea8610622c9d63c30ca25dea03ec3d90'
#darksky_api_key(key = d_key, force = T)

Sys.setenv(DARKSKY_API_KEY = d_key)


# Create date table
days <- data_frame(date = seq(as.Date("2006-12-31"), as.Date("2017-12-31"), 1),
                   join = 1)

days[1:5, ]

day <- 5

days[day, ]



forecast_col_names <- c("site_catid", "time", "summary", "icon", 
                        "precipIntensity", "precipProbability", "temperature", "apparentTemperature", "dewPoint", "humidity", "pressure", "windSpeed", "windGust", 
                        "windBearing", 
                        "cloudCover", "visibility", "precipAccumulation", 
                        "precipType")

forecast_col_types <- 'ccccdddddddddidddc'



# Generate table of downloaded site-dates
all_met <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff folders/Dorian/AQI/MET data/DarkSky MET data - daily summary - 2003-2017.csv") %>% 
             select(site_catid, date) %>% 
             group_by(site_catid, date) %>% 
             slice(1) %>%
             ungroup() %>%
             mutate(date = as.character(date))
  
folder <- "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/MET data/DarkSky database/sites" 
    
files <- list.files(folder)
  
for (file in files) {
    
    print(file)
    
    tmp <- read_csv(paste0(folder, "/", file)) %>% 
             filter(!is.na(site_catid), site_catid == gsub("[.csv]", "", file))
    
    if (nrow(tmp) > 0) {
    
      tmp$date <- format(tmp$time, "%Y-%m-%d") 
      
      tmp <- select(tmp, site_catid, date) %>% 
             group_by(site_catid, date) %>% 
             slice(1)
      
      all_met <- bind_rows(tmp, all_met)
    
}
}

#saveRDS(all_met, "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/MET data/DarkSky database/AQI MET archive.rdata") 
  
# Load previous archives
#all_met <- readRDS("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/MET data/DarkSky database/AQI MET archive.rdata")

names(all_met)

all_met$site_date <- paste(all_met$site_catid, all_met$date)


# Add air toxics sites
all_sites <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff folders/Dorian/AQI/MET data/AirToxics_sites.csv")

all_sites <- filter(all_sites, Year %in% 2010:2017, !duplicated(AQS_ID)) %>%
             select(Report_Name, AQS_ID, lat, long) %>% 
             rowwise() %>%
             mutate(AQS_ID = paste(substring(AQS_ID, 1, 2), 
                                   substring(AQS_ID, 3, 5), 
                                   substring(AQS_ID, 6, 9), sep = "-"))

names(all_sites) <- c("air_monitor", "site_catid", "monitor_lat", "monitor_long")

all_sites$run_order <- 3

sites$run_order <- 2

sites <- bind_rows(sites, filter(all_sites, !site_catid %in% sites$site_catid))

# Drop duplicate sites
sites <- filter(sites,  !site_catid %in% "27-017-7417")


# Join sites to calendar
sites$join <- 1

sites <- left_join(sites, days)

sites$join <- NULL

sites$site_date <- paste(sites$site_catid, sites$date)



# Put 909 first
sites$run_order <- ifelse(sites$site_catid %in% c("27-053-0909"), 1, sites$run_order) #"27-053-0910" Pacific street

# Put AQI sites first
sites$run_order <- ifelse(sites$site_catid %in% aqi_sites$site_catid, 1.1, sites$run_order)

sites <- arrange(sites, run_order, site_catid, desc(date))


# Drop dates already downloaded 
sites <- filter(sites, !site_date %in% all_met$site_date)


# Count site-days left to download for AQI
sites %>% filter(site_catid %in% aqi_sites$site_catid) %>% nrow() %>% print()


# Loop through site table and send DarkSky request
all_forecasts <- data_frame()

requests <- 0


for (i in 1:nrow(sites)) {
  
  site <- sites[i, ]
  
  if (requests > 990) break()
  
  print(site$site_date)
  
  requests <- requests + 1
  
  day_forc <- tryCatch(get_forecast_for(round(site$monitor_lat, 2), 
                                        round(site$monitor_long, 2),
                                        paste0(site$date, "T12:00:00-0400"), 
                                        exclude = "currently,daily"), 
                       error = function(err) NA)
  
  if (!is.na(day_forc)) {
  
    day_forc            <- day_forc$hourly 
  
    day_forc$site_catid <- site$site_catid
  
    all_forecasts       <- bind_rows(day_forc, all_forecasts)
    
  } else {
    
    next()
    
  }
  
}


# Add new data to archive
if (nrow(all_forecasts) > 0) {
  
  all_forecasts$time <- as.character(all_forecasts$time)
  
  all_forecasts <- read_csv(format_csv(all_forecasts[ , forecast_col_names]), 
                            col_types = forecast_col_types)
  
  # Check date coverage
  length(unique(all_forecasts$time))
  
  length(unique(as.Date(all_forecasts$time)))
  
  range(unique(as.Date(all_forecasts$time)))
  

  # Update individual site files
  for (i in unique(all_forecasts$site_catid)) {
    
    print(i)
    
    file_loc <- paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/MET data/DarkSky database/sites/", i, ".csv")
    
    if (file.exists(file_loc)) {
      
      # Load previously downloaded data
      temp <- read_csv(file_loc) %>% filter(!is.na(temperature), !is.na(site_catid))
      
      if (nrow(temp) > 0) {
        
        # Check for missing columns
        column_chk <- forecast_col_names %in% names(temp)
        
        # Arrange columns
        temp <- temp[ , forecast_col_names[column_chk]]
        
        # Align column types
        temp$time <- as.character(temp$time)
        
        temp <- read_csv(format_csv(temp),
                         col_types = strsplit(forecast_col_types, "")[[1]][column_chk] %>% 
                                               paste(collapse = ""))
        
        # Join tables
        site_met <- bind_rows(filter(all_forecasts, site_catid == i), temp)
        
        site_met <- group_by(site_met, time, site_catid) %>% slice(1)
        
        write_csv(site_met, file_loc)
      
      }
      
    } else {
      
        site_met <- filter(all_forecasts, site_catid == i)
        
        site_met$precipAccumulation <- as.numeric(site_met$precipAccumulation)
        
        site_met$time <- as.character(site_met$time)
        
        write_csv(site_met, file_loc)
    
    }
    
  }
  
  
  # Save for WAIR database
  if (FALSE) {
    
    all_met2$date_time_gmt <- format(all_met2$time, tz = "UTC", usetz = T)
    
    all_met2$date_time_cst <- format(all_met2$time, tz = "America/Chicago", usetz = T)
    
    unique(difftime(all_met2$time[1:1000], all_met2$date_time_cst[1:1000]))
    
    unique(difftime(all_met2$time[1:1000], all_met2$date_time_gmt[1:1000]))
    
    
    all_met2 <- all_met2 %>% select(site_catid, date_time_gmt, date_time_cst, summary, icon, temperature, 
                                    dewPoint, humidity, pressure, visibility, 
                                    windBearing, windSpeed, cloudCover, 
                                    precipIntensity, precipAccumulation, precipType)
  
    names(all_met2) <- c("site_catid", "date_time_gmt", "date_time_cst", "summary", "icon", "temperature_f",       
                         "dew_point_f", "humidity", "pressure_milbar", "visibility_miles", "wind_bearing_deg", "wind_speed_mph",         
                         "cloud_cover", "precip_intensity_inph", "precip_type")
  
  # Time check
  if (FALSE) {
    a <- get_forecast_for(45.13768, -93.20762,
                          paste0("2009-12-30", "T12:00:00-0400"), 
                          exclude = "currently,daily")
    
  }
  
  
  #saveRDS(all_met2, "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/MET data/DarkSky database/AQI MET archive_wair.rdata")
  
  #write_csv(all_met2, "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/MET data/DarkSky database/AQI MET archive_wair.csv")
  
  #check <- readRDS("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/MET data/DarkSky database/AQI MET archive_wair.rdata")
  
  }
}


# Count missing values
if (FALSE) {
met_sum <- group_by(all_met2, as.Date(time)) %>% 
           summarize(wind_ms        = mean(windSpeed, na.rm = T),
                     windspeed_miss = sum(is.na(windSpeed)),
                     windbear_miss  = sum(is.na(windBearing)),
                     cloud_miss     = sum(is.na(cloudCover)),
                     sum_miss       = sum(is.na(summary)),
                     precip_miss    = sum(is.na(precipIntensity)))

}

if (FALSE) {
#------------------------------#
# Using purr for multiple sites
#-------------------------------#
more_than_one <- data.frame(loc  = c("Maine", "Seattle"),
                            lon  = c(43.2672, 47.6097),
                            lat  = c(70.8617, 122.3331),
                            when = c("2013-05-06T12:00:00-0400",
                                     "2013-05-06T12:00:00-0400"),
                            stringsAsFactors = FALSE)

bigger_list <- pmap(list(more_than_one$lon, more_than_one$lat,
                         more_than_one$when),
                    get_forecast_for)

names(bigger_list) <- more_than_one$loc

bigger_list$Seattle

}


##