#! /usr/bin/env Rscript

library(dplyr)
library(readr)
library(geosphere)
library(measurements)
library(tidyr)
library(stringr)


# Load site locations
sites <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff folders/Dorian/AQI/MET data/Monitors and Rep Wx Stations.csv")

names(sites) <- gsub(" ", "_", tolower(names(sites)))

# Filter to one site per forecast city
sites <- filter(sites, !site_catid %in% c('27-017-7416'))


# Set day
days_past   <- 0


# Load HYSPLIT trajectories
setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Current forecast")

hys  <- read_csv(paste0(Sys.Date() - days_past, "_AQI_raw_HYSPLIT.csv"))

# Download current AQI monitor readings for Noon
today       <- Sys.Date() - days_past

day         <- gsub("-", "", Sys.Date() - days_past)

year        <- format(Sys.Date(), "%Y")

gmt_time    <- (as.numeric(format(Sys.time() - 2100, tz = "GMT", "%H")) - 1) %% 24 

if(gmt_time > 17) gmt_time <- 17

airnow_base <- paste0('https://s3-us-west-1.amazonaws.com/files.airnowtech.org/airnow/', year, '/', day, '/')

sites_url   <- paste0(airnow_base, 'monitoring_site_locations.dat')

site_coords <- try(read_delim(sites_url, "|", col_names=F, col_types = paste0(rep('c', 23), collapse = "")), silent=T)

aqi_url     <- paste0(airnow_base, "HourlyData_", day, gmt_time, ".dat")

aqi         <- try(read_delim(aqi_url, "|", col_names = F, col_types = c('ccccdccdc')), silent=T)


# Add yesterday's AQI monitor readings for Noon
# Used for updating today's (day zero's) forecast
day           <- gsub("-", "", Sys.Date() - 1 - days_past)

year          <- format(Sys.Date(), "%Y")

yesterday_gmt_time <- 17

airnow_base   <- paste0('https://s3-us-west-1.amazonaws.com/files.airnowtech.org/airnow/', year, '/', day, '/')

aqi_url       <- paste0(airnow_base, "HourlyData_", day, yesterday_gmt_time, ".dat")

aqi_yesterday <- try(read_delim(aqi_url, "|", col_names = F, col_types = c('ccccdccdc')), silent=T)

aqi           <- bind_rows(aqi, aqi_yesterday)

closeAllConnections()


# Clean data
site_coords <- site_coords[ , c(1:2,4:5,7,9:10,12:13,16:21)]

names(site_coords) <- c("AqsID","Parameter","Site Name","Active","Agency","Lat","Long","Local_Time","Country","City_Code","City","State_FIPS","State","FIPS","County")

site_coords <- site_coords[!duplicated(site_coords$AqsID), ]

names(aqi) <- c("Date", "Time", "AqsID", "Site Name", "Local_Time" , "Parameter", "Units", "Concentration","Agency")

aqi$Parameter <- gsub("[.]", "", aqi$Parameter)

aqi$StateID   <- substring(aqi$AqsID, 1, 2) 

aqi$Units     <- NULL

# Filter to Ozone & PM25
unique(aqi$Parameter)

aqi <- filter(aqi, Parameter %in% c("OZONE", "PM25")) #, "PM10"))

#aqi <- mutate(aqi, units = ifelse(Parameter == "OZONE", "PPB", "UG/M3"))


# Split Ozone and PM25 columns
aqi <- spread(aqi, Parameter, Concentration)


# Check for missing site coordinates
missing <- aqi[!unique(aqi$AqsID) %in% site_coords$AqsID, ]

print(sort(unique(missing$Agency)))


# Join coordinates
aqi         <- left_join(aqi, site_coords[ , c("AqsID", "Lat", "Long")])

aqi$Lat     <- as.numeric(aqi$Lat)
aqi$Long    <- as.numeric(aqi$Long)

aqi <- filter(aqi, !is.na(Lat))

# QC max/min concentration data
max(aqi$OZONE, na.rm = T)

missing_conc <- filter(aqi, is.na(OZONE))

aqi$OZONE <- ifelse(aqi$OZONE < -8, NA, aqi$OZONE)
aqi$OZONE <- ifelse(aqi$OZONE > 250, NA, aqi$OZONE)

aqi$PM25 <- ifelse(aqi$PM25 < -8, NA, aqi$PM25)
aqi$PM25 <- ifelse(aqi$PM25 > 250, NA, aqi$PM25)

missing_conc <- filter(aqi, is.na(OZONE))

# Filter missing values
aqi          <- filter(aqi, !is.na(OZONE) | !is.na(PM25))


# Find nearest AQS monitoring sites within designated distance.
max_distance <- 250 # miles
inner_buffer <- 85  # miles


# Assign distance weighted average of site concentrations to HYSPLIT.
hys$year  <- format(as.Date(hys$date), "%Y")
hys$month <- format(as.Date(hys$date), "%m")
hys$day   <- format(as.Date(hys$date), "%d")

hysplit_columns <- c("date", "year", "month", "day", "hour", "receptor", 
                     "hour.inc", "height", "receptor_height", "date2", "lat", "lon", "forecast_day")

hys <- as_data_frame(hys[ , hysplit_columns])

names(hys)[c(1,7,8,10:12)] <- c("receptor_date", "traj_hours", "start_height", "parcel_date", "start_lat", "start_lon")


hys$wtd_Ozone_Noon_ppb         <- NA
hys$wtd_pm25_Noon              <- NA
hys$backgr_o3_site_distance    <- NA
hys$backgr_pm25_site_distance  <- NA

hys$parcel_date   <- as.character(format(as.Date(hys$parcel_date), "%m/%d/%y"))

hys$receptor_date <- format(as.Date(hys$receptor_date), "%m/%d/%Y")


for(i in 1:nrow(hys)) {
  
  print(i)
  
  hys_coords <- with(hys[i, ], c(start_lon, start_lat))
  
  near_sites_all  <- subset(aqi, Date == hys[i, ]$parcel_date)
  
  near_sites_all  <- near_sites_all %>% 
                     rowwise() %>%
                     mutate(dist_to_hys = distVincentyEllipsoid(c(Long, Lat), hys_coords) / 1609)
  
  for(pollut in c("OZONE", "PM25")) {
    
    if(pollut == "OZONE") near_sites <- subset(near_sites_all, !is.na(OZONE))
    
    if(pollut == "PM25") near_sites  <- subset(near_sites_all, !is.na(PM25))
    
    
    # If no monitors within maximum distance grab the CMAQ forecast 
    if(min(near_sites$dist_to_hys, na.rm = T) > max_distance) { # & hys[i, ]$forecast_day %in% c("day0", "day1")) {
      
      
      if(pollut == "OZONE") {
        hys[i, ]$backgr_o3_site_distance  <- min(near_sites$dist_to_hys, na.rm = T) 
        
        hys[i, ]$wtd_Ozone_Noon_ppb    <- tryCatch(as.numeric(cmaq_forecast(hys_coords[2], 
                                                                            hys_coords[1], 
                                                                            hour_gmt = 17)[1, 3]), 
                                                   error = function(err) NA)
        
        if(is.na(hys[i, ]$wtd_Ozone_Noon_ppb)) hys[i, ]$wtd_Ozone_Noon_ppb <- arrange(near_sites, dist_to_hys)[1, ]$OZONE
        
      } else {
        
        hys[i, ]$backgr_pm25_site_distance  <- min(near_sites$dist_to_hys, na.rm = T) 
        
        hys[i, ]$wtd_pm25_Noon <- arrange(near_sites, dist_to_hys)[1, ]$PM25
      }
      
    } else {
      
      near_sites_85 <- subset(near_sites, dist_to_hys <= inner_buffer)
      
      if(nrow(near_sites_85) < 1) near_sites_85 <- arrange(near_sites, dist_to_hys)[1, ]
      
      if(pollut == "OZONE") { 
        
        hys[i, ]$backgr_o3_site_distance  <- min(near_sites$dist_to_hys, na.rm = T) 
        
        hys[i, ]$wtd_Ozone_Noon_ppb <- sum(near_sites_85$OZONE / sqrt(near_sites_85$dist_to_hys), na.rm = T) / sum(1/sqrt(near_sites_85$dist_to_hys))
        
      } else {
        
        hys[i, ]$backgr_pm25_site_distance  <- min(near_sites$dist_to_hys, na.rm = T) 
        
        hys[i, ]$wtd_pm25_Noon <- sum(near_sites_85$PM25 / sqrt(near_sites_85$dist_to_hys), na.rm = T) / sum(1/sqrt(near_sites_85$dist_to_hys))
        
      }
      
    }
  }
}


hys_bk <- hys

# Round values
hys$wtd_Ozone_Noon_ppb <- round(hys$wtd_Ozone_Noon_ppb, 1)
hys$wtd_pm25_Noon      <- round(hys$wtd_pm25_Noon, 1)
hys$backgr_o3_site_distance   <- round(hys$backgr_o3_site_distance) 
hys$backgr_pm25_site_distance <- round(hys$backgr_pm25_site_distance)


#-- Fill missing values with nearest monitor
missing <- filter(hys, is.na(wtd_Ozone_Noon_ppb) | is.na(wtd_pm25_Noon))

if(nrow(missing) > 0) {

# Add coords
missing$site_catid <- missing$receptor

missing <- left_join(missing, sites[ , c("site_catid", "monitor_lat", "monitor_long")])

print("Filling missing sites...")

for(i in 1:nrow(missing)) {
  
  print(i)
  
  hys_coords <- with(missing[i, ], as.numeric(c(monitor_long, monitor_lat)))
  
  near_sites_all  <- subset(hys, parcel_date == missing[i, ]$parcel_date)
  
  near_sites_all  <- subset(near_sites_all, receptor_height == missing[i, ]$receptor_height)
  
  
  # Add coords
  near_sites_all$site_catid <- near_sites_all$receptor
  
  near_sites_all  <- left_join(near_sites_all, sites[ , c("site_catid", "monitor_lat", "monitor_long")])
  
  near_sites_all  <- near_sites_all %>% 
                     rowwise() %>%
                     mutate(dist_to_hys = distVincentyEllipsoid(as.numeric(c(monitor_long, monitor_lat)), hys_coords) / 1609)
  
 if(is.na(missing[i, ]$wtd_Ozone_Noon_ppb)) { 
      
      # Get nearest background ozone value
      missing[i, ]$wtd_Ozone_Noon_ppb <- arrange(subset(near_sites_all, !is.na(wtd_Ozone_Noon_ppb)), dist_to_hys)$wtd_Ozone_Noon_ppb[1]
      
      missing[i, ]$backgr_o3_site_distance <- NA
    }
    
 if(is.na(missing[i, ]$wtd_pm25_Noon)) {
    
      # Get nearest background PM2.5 value
      missing[i, ]$wtd_pm25_Noon <- arrange(subset(near_sites_all, !is.na(wtd_pm25_Noon)), dist_to_hys)$wtd_pm25_Noon[1]
      
      missing[i, ]$backgr_pm25_site_distance <- NA
    }
    
}


# Join missing sites
hys <- filter(hys, !is.na(wtd_Ozone_Noon_ppb) & !is.na(wtd_pm25_Noon))

hys <- bind_rows(hys, missing[ , names(hys)])

}

# Clean results
hys$traj_hours <- paste(hys$forecast_day, paste0(-hys$traj_hours, "hrs"), sep = "_")

hys_mean <- group_by(hys, receptor, parcel_date, traj_hours, hour) %>%
            summarise(mean_Ozone_Noon_ppb = mean(wtd_Ozone_Noon_ppb, na.rm = T),
                      mean_pm25_Noon      = mean(wtd_pm25_Noon, na.rm = T)) %>%
            ungroup()


group_by(hys_mean, receptor, traj_hours) %>% summarize(count = n()) %>% .$count %>% range()


#-- Wide format
hys_o3  <- spread(select(hys_mean, -parcel_date, -mean_pm25_Noon), traj_hours, mean_Ozone_Noon_ppb)

hys_pm  <- spread(select(hys_mean, -parcel_date, -mean_Ozone_Noon_ppb), traj_hours, mean_pm25_Noon)


names(hys_o3)[3:6]  <- paste0(names(hys_o3)[3:6], "_mean_backgr_ozone")
names(hys_pm)[3:6]  <- paste0(names(hys_pm)[3:6], "_mean_backgr_pm25")

hys_wide <- left_join(hys_o3, hys_pm)

hys_wide$row_id <- 1:nrow(hys_wide)

names(hys_wide)[1:2] <- c("site_catid", "hour_gmt")


# SAVE Results
setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Current forecast")

write.csv(hys_wide[ , c(1:11)], paste0(Sys.Date(), "_", gmt_time, "z_AQI_background.csv"), row.names = F)

