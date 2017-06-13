#! /usr/bin/env Rscript


library(dplyr)
library(readr)
library(tidyr)


# AQI conversion functions
source("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Web/aqi-watch/R/aqi_convert.R")


# Sites
#--------------------------#
print("Loading sites...")

sites <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff folders/Dorian/AQI/MET data/Monitors and Rep Wx Stations.csv")
                  #stringsAsFactors = FALSE)

names(sites) <- gsub(" ", "_", tolower(names(sites)))

# Switch Voyageurs AQS ID to alt
sites[sites$site_catid == "27-137-9000", "site_catid"] <- sites[sites$site_catid == "27-137-9000", "alt_siteid"]


# Yesterday's official forecast
#--------------------------------#
setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/Air_Modeling/AQI_Forecasting/Tree_Data/Forecast/AQI_Solutions/Values")
print("Loading official forecasts...")

aqi_forc <- read.csv("All_Values.csv", stringsAsFactors = FALSE)
  

# Yesterday's model output forecast
#------------------------------------#
setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/Air_Modeling/AQI_Forecasting/Tree_Data/Forecast/AQI_Solutions/Values")
print("Loading modeling forecasts...")

#list.files()

mod_o3   <- read.csv("All_Values_O3.csv", stringsAsFactors = FALSE)

mod_pm   <- read.csv("All_Values_PM.csv", stringsAsFactors = FALSE)

mod_forc <- full_join(mod_o3, mod_pm)


# Change names of model forecast values to distinguish 
# between official submitted forecast
names(mod_forc)[c(2:3,10:11)] <- c("mod_max.avg8hr", "mod_aqi_o3", "mod_pm25avg", "mod_aqi_pm")

# Join tables into new table - "verify"
verify  <- left_join(aqi_forc, mod_forc)


names(verify) <- c("forecast_day", "fcst_ozone_ppb", "fcst_ozone_aqi", "Date", "Group", "site_catid",
                   "Latitude", "Longitude", "short_name", "fcst_pm25_ugm3", "fcst_pm25_aqi",
                   "mod_max.avg8hr", "mod_aqi_o3", "mod_pm25avg", "mod_aqi_pm")

# Add date
verify$forecast_date  <- as.Date(verify$Date, "%m/%d/%Y")

verify$Date <- NULL


# Rearrange columns with dates first
verify <- verify[ , c(ncol(verify), 1, 5, 4, 2:3, 6:(ncol(verify) - 1))]

verify <- select(verify, -Latitude, -Longitude)



# Yesterday's model inputs
#--------------------------------#
setwd("X:\\Agency_Files\\Outcomes\\Risk_Eval_Air_Mod\\Air_Modeling\\AQI_Forecasting\\Tree_Data\\Forecast\\Forecast_Met")
print("Model inputs...")

all_inputs <- read_csv("Met_View.csv")

names(all_inputs)[1:3] <- c("forecast_day", "short_name", "site_catid")

#-- Update date name and format
#all_inputs$Date <- as.Date(all_inputs$Date, "%m/%d/%Y")

all_inputs$Date <- as.Date(all_inputs$Date, "%Y-%m-%d")

names(all_inputs)[grep("Date", names(all_inputs))] <- "forecast_date"

#-- Join all
verify   <- left_join(verify, select(all_inputs, -short_name))

   
# Yesterday's HYSPLIT origins
#--------------------------------#
setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Current forecast")
print("Loading HYSPLIT...")

hys <- read_csv(paste0(Sys.Date() - 1, "_AQI_raw_HYSPLIT.csv"))

# Convert day index column
hys$forecast_day <- as.numeric(gsub("day", "", hys$forecast_day))

# Collapse origin coordinates
hys$lat_long <- paste(hys$lat, hys$lon, sep = ", ")

# Split 10m and 500m trajectory into 2 columns
hys_10m    <- filter(hys, receptor_height == 10)
hys_500m   <- filter(hys, receptor_height == 500)

# Wide format by forecast day
#hys_10m           <- spread(hys_10m[ , -c(2:13)], forecast_day, lat_long)
hys_10m            <- hys_10m[ , -c(2:11,13)]
names(hys_10m)[4]  <- "background_origin_10m"

#hys_500m          <- spread(hys_500m[ , -c(2:13)], forecast_day, lat_long)
hys_500m           <- hys_500m[ , -c(2:11,13)]
names(hys_500m)[4] <-  "background_origin_500m"


#-- Join 10m and 500m tables
hys_origin <- full_join(hys_10m, hys_500m)


#-- Update date format
hys_origin$date <- as.Date(hys_origin$date)


#-- Join HYSPLIT origin to verifcation table
names(hys_origin)[1:2] <- c("site_catid", "forecast_date")

verify <- left_join(verify, hys_origin)


# Clean table
#--------------------------------#
names(verify) <- gsub(" ", "_", tolower(names(verify)))

verify <- filter(verify, !is.na(forecast_day), !is.na(site_catid))


# Load and join to previous days
setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Verification")
print("Loading previous day verifications...")

#all_verify <- read_csv("2017_verification_table.csv")

all_verify <- readRDS(paste0("Archive/", Sys.Date() - 1, "_verification_table.Rdata"))

#all_verify$forecast_date <- as.character(all_verify$forecast_date)

#-- Remove duplicates and NA forecast days
all_verify <- filter(all_verify, 
                     !paste(forecast_date, forecast_day) %in% paste(verify$forecast_date, verify$forecast_day),
                     !is.na(forecast_day),
                     !is.na(forecast_date))

all_verify <- bind_rows(verify, all_verify)


# Yesterday's actuals
#--------------------------#
setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Current forecast")
print("Loading actuals...")

actuals <- read_csv(paste0(Sys.Date() - 1, "_AQI_observed", ".csv"))

#-- Header names
names(actuals)[c(1,5:8)] <- c("forecast_date","count_ozone_obs",
                              "count_pm25_obs","obs_ozone_ppb","obs_pm25_ugm3")


#-- Add AQI category
actuals <- ungroup(actuals) %>% 
           rowwise() %>%
           mutate(obs_ozone_aqi  = conc2aqi(obs_ozone_ppb, "OZONE"),
                  obs_pm25_aqi   = conc2aqi(obs_pm25_ugm3, "PM25")) %>%
           ungroup()

# Update Voyageurs site ID
actuals[actuals$site_catid == "27-137-0034", "site_catid"] <- "27-137-9000"

# Drop non-forecasted sites
actuals <- filter(actuals, !air_monitor %in% c("Voyageurs", "Virginia"))

actuals <- filter(actuals, !is.na(site_catid))


# Select yesterday forecasts and drop observation columns
yesterday_fcst <- filter(all_verify, forecast_date == Sys.Date() - 1) %>%
                  select(-c(count_ozone_obs, count_pm25_obs, obs_ozone_ppb, 
                            obs_pm25_ugm3, obs_ozone_aqi, obs_pm25_aqi))

# Attach actuals to yesterday forecasts
yesterday_fcst <- left_join(yesterday_fcst, select(actuals, -air_monitor, -aqsid))



# Yesterday's CMAQ forecast
#--------------------------------#
setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Current forecast")
print("CMAQ...")

cmaq_forc <- read_csv(paste0(Sys.Date() - 1, "_CMAQ_forecast.csv"))

cmaq_forc <- select(cmaq_forc, -cmaq_day0_max_ozone_1hr, -cmaq_day1_max_ozone_1hr)

names(cmaq_forc) [1:2] <- c("day0", "day1") 

# Flip to long format
cmaq_forc <- tidyr::gather(data = cmaq_forc, key = forecast_day, value = cmaq_ozone_ppb, na.rm = FALSE, day0, day1)


# Add category
cmaq_forc <- ungroup(cmaq_forc) %>% 
             rowwise() %>%
             mutate(cmaq_ozone_aqi = conc2aqi(cmaq_ozone_ppb, "OZONE")) %>%
             ungroup()

# Add days
cmaq_forc$forecast_day  <- as.numeric(gsub("day", "", cmaq_forc$forecast_day))

cmaq_forc$forecast_date <- Sys.Date() - 1 + cmaq_forc$forecast_day


# Attach CMAQ to yesterday forecasts
yesterday_fcst <- left_join(select(yesterday_fcst, -cmaq_ozone_ppb, -cmaq_ozone_aqi),
                            cmaq_forc)



# Join yesterday actuals & CMAQ to master table
#------------------------------------------------#
all_verify <- filter(all_verify, forecast_date != (Sys.Date() - 1))

all_verify <- bind_rows(yesterday_fcst, all_verify)



# Save master verification table
#------------------------------------------------#
setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Verification")
print("Saving file...")

write.csv(all_verify, "2017_verification_table.csv", row.names = F)

saveRDS(all_verify, paste0("Archive/", Sys.Date(), "_verification_table.Rdata"))


#-- Create event table
events <- filter(actuals, forecast_date == Sys.Date() - 1) %>% 
          left_join(select(sites, short_name, site_catid)) %>%
          select(forecast_date, short_name, site_catid, obs_ozone_aqi, obs_pm25_aqi)

# Add event flag and comments
events$event_flag      <- NA
events$event_comments  <- NA

# Attach new event days to event archive
print("Loading previous event flags...")

# Load
all_events <- read_csv("2017_event_table.csv")

all_events <- filter(all_events, !forecast_date %in% events$forecast_date)

# Join
all_events <- bind_rows(events, all_events)


# Save master verification table
print("Saving event file...")

write.csv(all_events, "2017_event_table.csv", row.names = F)

saveRDS(all_events, paste0("Archive/", Sys.Date(), "_event_table.Rdata"))


##
