# Yesterday's baseline AQI forecasting metrics

# Packages
library(tidyverse)


# Include results for these models:
#### - Persistance
#### - Rolling 7 day median
#### - Historical week median 


# AQI conversion functions
source(paste0(aqiwatch_path, "R/aqi_convert.R"))


# Set dates
today <- Sys.Date()

yesterday      <- today - 1 

two_days_ago   <- today - 2

three_days_ago <- today - 3


# Load recent results
results <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Verification/event_table.csv")


# Persistance = Previous day result
persist <- filter(results, forecast_date == three_days_ago) %>%
             mutate(forecast_date      = yesterday) %>%
             rename(persist_ozone_aqi  = obs_ozone_aqi,
                    persist_pm25_aqi   = obs_pm25_aqi) %>%
             select(forecast_date, short_name, site_catid, persist_ozone_aqi, persist_pm25_aqi)


# Rolling 7-days
roll <- filter(results, forecast_date <= three_days_ago, forecast_date > (today - 10)) %>%
              group_by(short_name, site_catid) %>%
              summarize(forecast_date   = yesterday,
                        roll_ozone_aqi  = median(obs_ozone_aqi, na.rm = T),
                        roll_pm25_aqi   = median(obs_pm25_aqi, na.rm = T)) %>%
              select(forecast_date, short_name, site_catid, roll_ozone_aqi, roll_pm25_aqi)



# Week median
if (FALSE) {

# Load historical results
data <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Forecast data/All Ozone and PM25 forecast fields - DarkSky.csv")


# Set all years to 2012
data$start_date <- format(data$start_date, "2012-%m-%d") %>% as.Date()


# Initiate table
week_med <- data_frame()

# 11 Day rolling median
for (i in unique(as.character(data$start_date))) {
  
  print(i)
  
  i <- as.Date(i)
  
  sub_data <- filter(data, start_date > (i - 6), start_date < (i + 6))
  
  sub_data <- group_by(sub_data, site_catid) %>% 
                summarize(forecast_date = i,
                          roll11_ozone_aqi = median(max_avg8hr, na.rm = T) %>% conc2aqi("ozone"),  
                          roll11_pm25_aqi  = median(pm25_avg, na.rm = T) %>% conc2aqi("pm25"))
  
  week_med <- bind_rows(sub_data, week_med)
  
}


# QC
unique(week_med$forecast_date)

# Duplicate Detroit Lakes observations for Leech Lake and Red Lake
det_lakes <- filter(week_med, site_catid == "27-005-2013")

leech_lake <- mutate(det_lakes, site_catid = "27-021-3410")

#red_lake <- mutate(det_lakes, site_catid = "27-007-2304")

week_med <- bind_rows(week_med, leech_lake)

# Use nearest ozone monitor for sites missing ozone
# ...
# ...


# SAVE
write_csv(week_med, "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Forecast data/rolling_11-day_averages.csv")

}


week_med <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Forecast data/rolling_11-day_averages.csv") %>%
               filter(forecast_date == format(yesterday, "2012-%m-%d")) %>%
               mutate(forecast_date = yesterday)


# Average of 3 results above
blend3 <- left_join(week_med, roll) %>% left_join(select(persist, -short_name))

blend3_bk <- left_join(roll, week_med) %>% left_join(select(persist, -short_name))

blend3 <- bind_rows(blend3_bk, filter(blend3, !site_catid %in% blend3_bk$site_catid))

blend3 <- blend3 %>% 
            rowwise() %>% 
            mutate(bb3_ozone_aqi = median(c(roll_ozone_aqi, persist_ozone_aqi, roll11_ozone_aqi) %>% as.numeric(), na.rm = T),
                   bb3_pm25_aqi  = median(c(roll_pm25_aqi, persist_pm25_aqi, roll11_pm25_aqi) %>% as.numeric(), na.rm = T))


##

