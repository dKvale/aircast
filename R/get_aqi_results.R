#! /usr/bin/env Rscript

library(stringr)
library(RCurl)
library(tidyverse)


# Load get_airnow() function
source(paste0(aircast_path, "R/get_airnow.R"))

       
# Get yesterday's actuals  #
#--------------------------#

# Drop outstate sites
sites <- filter(aqi_sites, !fcst_region %in% c("CA", "ND", "SD", "WI", "IA"))

# Drop dashes in IDs to match AQS
sites$aqsid <- gsub("-", "", sites$site_catid)

# Filter to one site per forecast city
sites <- filter(sites, !site_catid %in% c('27-017-7416'))


# Get dates
today     <- gsub("-", "", Sys.Date())

yesterday <- Sys.Date() - 1

year      <- format(Sys.Date() - 1, "%Y")

# Site list
site_list <- c(sites$aqsid, gsub("-", "", sites$alt_siteid))

# Pollutant list
pollutant_list <- c("OZONE-8HR", "PM2.5-24hr")

# Download results from AirNow
aqi <- get_airnow(yesterday, site_list[!is.na(site_list)], pollutant_list) %>% 
       select(-Units, -Hours, -Agency)


# Add missing sites from AirNow's "yesterday.dat" file
#...
#...


# Flip to wide format

# Create empty rows if table is blank
if (nrow(aqi) < 1) {
  
  aqi[1:2, ] <- NA
  
  aqi$Parameter <- c("OZONE-8HR", "PM2.5-24hr")
}

# Create blank results if parameter is missing
if (length(unique(aqi$Parameter)) < 2) {
  
  # Duplicate table
  aqi2 <- aqi
  
  aqi2$Parameter <- c("OZONE-8HR", "PM2.5-24hr")[!c("OZONE-8HR", "PM2.5-24hr") %in% unique(aqi$Parameter)]
  
  aqi2$Concentration <- NA
  
  aqi <- bind_rows(aqi, aqi2)
}

aqi <- spread(aqi, Parameter, Concentration)
  
names(aqi)[c(4:5)] <-  c("max_ozone_8hr", "pm25_24hr")
  


# QC MPCA sites using AirVision data

# Connect to aqi-watch FTP site
airvis_link <- paste0("ftp://", creds$airvis_ftp_usr_pwd, "@34.216.61.109/airvision/")

# Find last file of the day
file_list   <- getURL(airvis_link, verbose = T, dirlistonly = T) %>%
               strsplit("\r\n") %>% 
               .[[1]]

final_hour  <- file_list[grepl(today, file_list)][1]

# Download monitoring data
#download.file(paste0(airvis_link, final_hour), "temp.rData")
#base::load("temp.rData")

airvis_df   <- try(read_csv(paste0(airvis_link, final_hour),
                              col_names = F, 
                              col_types = c('cccccccdcccccccccccc')), 
                   silent = T)

# Try again if fail
if (class(airvis_df) == "try-error") {
  
  Sys.sleep(15)
  
  airvis_df   <- try(read_csv(paste0(airvis_link, final_hour),
                              col_names = F, 
                              col_types = c('cccccccdcccccccccccc')), 
                     silent = T)
}

# Create empty table if fail
if (class(airvis_df) == "try-error") {
  
  airvis_df <- data_frame(aqsid     = NA, x2 = NA,x3 = NA,
                          date      = as.character(NA), 
                          Parameter = NA, x6 = NA, x7 = NA,
                          Concentration = NA, x9 = NA, 
                          qc_flag   = NA)
}

airvis_df   <- airvis_df[ , c(1,4,5,8,10)]
  
names(airvis_df) <- c("aqsid", "date", "Parameter", "Concentration", "qc_flag")

closeAllConnections()


# Drop flagged concentrations
airvis_df$Concentration <- ifelse(airvis_df$qc_flag > 5, NA, airvis_df$Concentration)


# Filter to PM2.5 and ozone
airvis_df$aqsid <- gsub("840", "", airvis_df$aqsid)

airvis_ozone    <- filter(airvis_df, 
                          Parameter == 44201, 
                          aqsid %in% gsub("-", "", c(sites$site_catid, sites$alt_siteid)))

airvis_pm       <- filter(airvis_df, 
                          Parameter %in% c(88101), # 88502), 
                          aqsid %in% gsub("-", "", c(sites$site_catid, sites$alt_siteid)))


if (nrow(airvis_ozone) > 0) {
  # Ozone summary
  airvis_ozone <- group_by(airvis_ozone, aqsid, Parameter) %>% 
                  arrange(date) %>%
                  mutate(row_id = 1:n())
  
  # Calculate 8-hr ozone values
  for (i in 1:nrow(airvis_ozone)) {
  
    aqs  <- airvis_ozone[i, ]$aqsid
    
    rows <- airvis_ozone[i, ]$row_id
    
    airvis_ozone[i , "ozone_8hr"] <- mean(filter(airvis_ozone, aqsid == aqs, row_id %in% (max(1, rows - 7):rows))$Concentration, na.rm = T)
  
  }    
  
  # Drop 8-hr averages from first 4 hours of day
  airvis_ozone[airvis_ozone$row_id %in% 1:4, ]$ozone_8hr <- NA

}    
  
airvis_ozone <- group_by(airvis_ozone, aqsid) %>%                
                summarize(max_ozone_8hr_vis = round(max(ozone_8hr, na.rm = T), 2),
                          n_ozone_obs       = sum(!is.na(Concentration)),
                          n_ozone_uniq      = length(unique(Concentration)))



# PM 2.5 summary
airvis_pm    <- group_by(airvis_pm, aqsid) %>%                
                summarize(pm25_24hr_vis = round(mean(Concentration, na.rm = T), 2),
                          n_pm25_obs    = sum(!is.na(Concentration)),
                          n_pm25_uniq   = length(unique(Concentration)))



# Check Voyageur's site ID
aqi[aqi$aqsid == "271370034", "aqsid"] <- "271379000"


# Join AirNow with AirVis
aqi$City <- NULL

air_all <- full_join(aqi, airvis_ozone)

air_all <- full_join(air_all, airvis_pm)


# Join Sites and AQS-IDs
air_all <- left_join(air_all, select(sites, c(air_monitor, site_catid, aqsid)))

# Drop blank sites
air_all <- filter(air_all, !is.na(aqsid))

# Missing sites
miss_sites <- filter(sites, 
                     !aqsid %in% air_all$aqsid & 
                     !gsub("-", "", alt_siteid) %in% air_all$aqsid)


# Use AirNow value if AirVis is missing
air_all <- group_by(air_all, aqsid) %>% 
           mutate(a_max_ozone_8hr_ppb = ifelse(is.na(max_ozone_8hr_vis), round(max_ozone_8hr, 1), round(max_ozone_8hr_vis, 1)))

air_all <- group_by(air_all, aqsid) %>% 
           mutate(a_pm25_24hr_ugm3 = ifelse(is.na(pm25_24hr_vis), round(pm25_24hr, 1), round(pm25_24hr_vis, 1)))


# Quality checks

# Drop daily value if less than 14 observations
air_all <- mutate(air_all, 
                  a_max_ozone_8hr_ppb = ifelse(n_ozone_obs < 14 & !is.na(n_ozone_obs), NA, a_max_ozone_8hr_ppb),
                  a_pm25_24hr_ugm3    = ifelse(n_pm25_obs < 14 & !is.na(n_pm25_obs), NA, a_pm25_24hr_ugm3))


# Drop daily value if less than 6 unique values for ozone; 4 unique values for PM2.5
air_all <- mutate(air_all, 
                  a_max_ozone_8hr_ppb = ifelse(n_ozone_uniq < 7 & !is.na(n_ozone_uniq), NA, a_max_ozone_8hr_ppb),
                  a_pm25_24hr_ugm3    = ifelse(n_pm25_uniq < 4 & !is.na(n_pm25_uniq), NA, a_pm25_24hr_ugm3))


# Drop extra columns
air_all <- dplyr::select(air_all, -c(max_ozone_8hr, pm25_24hr, max_ozone_8hr_vis, pm25_24hr_vis, n_pm25_uniq, n_ozone_uniq))


# Set date
air_all$date <-  Sys.Date() - 1

# Check missing sites
miss_sites <- filter(sites, 
                     !aqsid %in% air_all$aqsid & 
                     !gsub("-", "", alt_siteid) %in% air_all$aqsid)


# Names
names(air_all)[c(3:4,7:8)] <- c("count_ozone_obs","count_pm25_obs","obs_max_ozone_8hr_ppb", "obs_pm25_24hr_ugm3") 


# Save
setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Current forecast")

keep_columns <- c("date", 
                  "site_catid", 
                  "air_monitor", 
                  "aqsid", 
                  "count_ozone_obs",
                  "count_pm25_obs",
                  "obs_max_ozone_8hr_ppb", 
                  "obs_pm25_24hr_ugm3")

file_name <- paste0(Sys.Date() - 1, "_AQI_observed", ".csv")

# Write file only if it isn't already there
if(!file_name %in% list.files()) {
  
  write.csv(air_all[ , keep_columns], paste0(Sys.Date() - 1, "_AQI_observed", ".csv"), row.names = F)
}




#
