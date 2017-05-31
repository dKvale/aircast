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


# Yesterday's forecast
#--------------------------------#
setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/Air_Modeling/Steve/AQI_Forecasting/Tree_Data/Forecast/AQI_Solutions/Values")
print("Loading forecasts...")

# Check if forecast date is current
#if(format(file.info("All_Values.csv")$mtime, "%m %d") == format(Sys.Date() - 1, "%m %d")) {
  
aqi_forc <- read.csv("All_Values.csv", stringsAsFactors = FALSE)
  
# Spread day forecasts across multiple columns
#aqi_forc <- spread(aqi_forc, DayIndex, 'Max Avg8Hr')
#names(aqi_forc)[3:7] <- c("short_name", "f_day0_max_ozone_8hr_ppb", "f_day1_max_ozone_8hr_ppb", "f_day2_max_ozone_8hr_ppb", "f_day3_max_ozone_8hr_ppb")
  
names(aqi_forc) <- c("forecast_day", "fcst_ozone_ppb", "fcst_ozone_aqi", "Date", "Group", "site_catid",
                     "Latitude", "Longitude","short_name", "fcst_pm25_ugm3","fcst_pm25_aqi")
  
# Add date
aqi_forc$day_forecast_made <- Sys.Date() - 1
  
aqi_forc$forecast_date     <- as.Date(aqi_forc$Date, "%m/%d/%Y")
  
aqi_forc$Date <- NULL
  
# Rearrange columns with dates first
aqi_forc <- aqi_forc[ , c(ncol(aqi_forc) - 1, 1, ncol(aqi_forc), 2:(ncol(aqi_forc)-2))]
  
aqi_forc <- select(aqi_forc, -Latitude, -Longitude)
  
# Add monitor's city name
#aqi_forc <- left_join(aqi_forc, select(sites, air_monitor, site_catid))


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


# Join tables into new table "verify"
verify  <- left_join(aqi_forc, cmaq_forc)


# Yesterday's model inputs
#--------------------------------#
setwd("X:\\Agency_Files\\Outcomes\\Risk_Eval_Air_Mod\\Air_Modeling\\Steve\\AQI_Forecasting\\Tree_Data\\Forecast\\Forecast_Met")
print("Model inputs...")

all_fcst <- read_csv("Met_View.csv")

if(FALSE) {
# Wide format by forecast day
n_columns     <- ncol(all_fcst)

#all_fcst_0    <- filter(all_fcst, Index == 0)
#all_fcst_1    <- filter(all_fcst, Index == 1)
#all_fcst_2    <- filter(all_fcst, Index == 2)
#all_fcst_3    <- filter(all_fcst, Index == 3)

#names(all_fcst_0)[-2]  <- paste0("day0_", names(all_fcst)[-2])
#names(all_fcst_1)[-2]  <- paste0("day1_", names(all_fcst)[-2])
#names(all_fcst_2)[-2]  <- paste0("day2_", names(all_fcst)[-2])
#names(all_fcst_3)[-2]  <- paste0("day3_", names(all_fcst)[-2])

all_fcst <- bind_cols(all_fcst_0[ , -1], 
                      all_fcst_1[ , -c(1, 2)], 
                      all_fcst_2[ , -c(1, 2)], 
                      all_fcst_3[ , -c(1, 2)])
}

names(all_fcst)[1:2] <- c("forecast_day", "short_name")

verify   <- left_join(verify, all_fcst)

verify$Date <- NULL
   
   
# Yesterday's HYSPLIT origins
#--------------------------------#
setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Current forecast")
print("Loading HYSPLIT...")

hys_source <- read_csv(paste0(Sys.Date() - 1, "_AQI_raw_HYSPLIT.csv"))

# Convert day index column
hys_source$forecast_day <- as.numeric(gsub("day", "", hys_source$forecast_day))

# Collapse origin coordinates
hys_source$lat_long <- paste(hys_source$lat, hys_source$lon, sep = ", ")

# Split 10m and 500m trajectory into 2 columns
hys_10m    <- filter(hys_source, receptor_height == 10)
hys_500m   <- filter(hys_source, receptor_height == 500)

# Wide format by forecast day
#hys_10m        <- spread(hys_10m[ , -c(2:13)], forecast_day, lat_long)
hys_10m        <- hys_10m[ , -c(2:11,13)]
names(hys_10m)[4]  <- "background_origin_10m"

#hys_500m        <- spread(hys_500m[ , -c(2:13)], forecast_day, lat_long)
hys_500m        <- hys_500m[ , -c(2:11,13)]
names(hys_500m)[4] <-  "background_origin_500m"


# Join columns
hys_source <- full_join(hys_10m, hys_500m)


names(hys_source)[1] <- "site_catid"

verify <- left_join(verify, select(hys_source, -date))


# Clean table
names(verify) <- gsub(" ", "_", tolower(names(verify)))

verify <- filter(verify, !is.na(day_forecast_made), !is.na(site_catid))



# Load and join previous days
setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Verification")
print("Loading previous day verifications...")

all_verify <- read_csv("2017_verification_table.csv")

all_verify$day_forecast_made <- as.character(all_verify$day_forecast_made)

all_verify <- filter(all_verify, !date %in% verify$date)

names(all_verify)[grep("date", names(all_verify))][1] <- "forecast_date"

all_verify <- bind_rows(verify, all_verify)


# Yesterday's actuals
#--------------------------#
setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Current forecast")
print("Loading actuals...")

actuals <- read_csv(paste0(Sys.Date() - 1, "_AQI_observed", ".csv"))

# Update date for testing
#actuals$Date <- actuals$Date + 1

names(actuals)[c(1,5:8)] <- c("forecast_date","count_ozone_obs",
                              "count_pm25_obs","obs_ozone_ppb","obs_pm25_ugm3")


# Add AQI category
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

# Attach actuals to forecasts
actuals    <- left_join(yesterday_fcst, select(actuals, -air_monitor, -aqsid))

all_verify <- filter(all_verify, forecast_date != Sys.Date()-1)

all_verify <- bind_rows(actuals, all_verify)


# Create event table
events <- select(all_verify, day_forecast_made, air_monitor, aqs_id)

# Add event flag and comments
events$event_flag      <- NA
events$event_comments  <- NA

# Attach new event days to event archive
setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Verification")
print("Loading previous event flags...")

all_events <- read_csv("2017_event_table.csv")

all_events$day_forecast_made <- as.character(all_events$day_forecast_made)

all_events <- filter(all_events, !date %in% all_events$date)


# Save
setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Verification")
print("Saving file...")

write.csv(all_verify, "2017_verification_table.csv", row.names = F)

#all_verify = verify
saveRDS(all_verify, paste0("Archive/", Sys.Date(), "_verification_table.Rdata"))


##
