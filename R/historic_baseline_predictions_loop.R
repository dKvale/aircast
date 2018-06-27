# Generate baseline AQI forecasting metrics

# Include results for these models:
#### - Persistance
#### - Rolling 7 day median
#### - Historical week median 


library(tidyverse)


# AQI conversion functions
source(paste0(aqiwatch_path, "R/aqi_convert.R"))

dates <- seq(as.Date("2017-05-20"), as.Date("2018-05-31"), 1)


# Load sites
sites <- aqi_sites


# Load recent results
results <- read_csv("../Verification/event_table.csv")

results2 <- read_csv("../Verification/event_table.csv")

results_all <- bind_rows(results, results2)

results_all <- filter(results_all, forecast_date %in% dates)

rm(results2)

min(results_all$forecast_date)

unique(results_all$forecast_date)


## Check site #'s
"27-137-0034" %in% results_all$site_catid

unique(results_all$site_catid %in% sites$site_catid)


# One value per day
results <- results_all %>% 
             group_by(forecast_date, site_catid) %>% 
             summarize(obs_ozone_aqi = median(obs_ozone_aqi, na.rm = T), 
                       obs_pm25_aqi  = median(obs_pm25_aqi, na.rm = T),
                       n = n())



# Load verification table
verify  <- read_csv("../Verification/verification_table.csv")

verify <- verify %>% mutate(mod_max_avg8hr = as.numeric(mod_max_avg8hr),
                            mod_pm25avg    = as.numeric(mod_pm25avg))

verify2 <- read_csv("../Verification/Archive/2017_verification_table.csv")

verify2 <- verify2 %>% mutate(mod_aqi_o3 = as.character(mod_aqi_o3),
                              mod_aqi_pm = as.character(mod_aqi_pm ))

# Join together
verify <- bind_rows(verify, verify2)

# Select June '17 to June '18
verify <- filter(verify, forecast_date %in% dates)

rm(verify2)

min(verify$forecast_date)

unique(verify$forecast_date)


## Check site #'s
"27-137-0034" %in% verify$site_catid

verify$site_catid[verify$site_catid == "27-137-0034"] <- "27-137-9000"

unique(verify$site_catid %in% sites$site_catid)


# One value per day
verify_day <- verify %>% 
              group_by(forecast_date, site_catid) %>% 
              summarize(obs_ozone_aqi_v = median(obs_ozone_aqi, na.rm = T), 
                        obs_pm25_aqi_v  = median(obs_pm25_aqi, na.rm = T))

# Join missing days to results
blank_results <- data_frame(forecast_date = rep(dates, each = 30),
                            site_catid    = rep(unique(results$site_catid), 
                                                length(dates)))

results <- left_join(blank_results, results)

results <- left_join(results, verify_day)

results <- results %>% 
             rowwise() %>% 
             mutate(obs_pm25_aqi = ifelse(is.na(obs_pm25_aqi), obs_pm25_aqi_v, obs_pm25_aqi),
                    obs_ozone_aqi = ifelse(is.na(obs_ozone_aqi), obs_ozone_aqi_v, obs_ozone_aqi))
   
               
# Check missing
filter(results, is.na(obs_pm25_aqi), !is.na(obs_pm25_aqi_v))


# Drop wildfire events
event_dates <- paste(filter(results_all, verified_bad_obs == "Y")$forecast_date, filter(results_all, verified_bad_obs == "Y")$site_catid)

results <- results %>% 
            select(-obs_pm25_aqi_v, -obs_ozone_aqi_v, -n) %>%
            mutate(obs_pm25_aqi = ifelse(paste(forecast_date, site_catid) %in% event_dates, NA, obs_pm25_aqi))


# Historical results
weeks_median <- read_csv("../Forecast data/rolling_11-day_averages.csv")

names(weeks_median)[2:4] <-  c("hist_date", "hist_week_ozone_aqi", "hist_week_pm25_aqi")


# Blank columns
if (F) {
results <- mutate(results,
                  persist_ozone_aqi   = NA,
                  persist_pm25_aqi    = NA,
                  roll_ozone_aqi      = NA,
                  roll_pm25_aqi       = NA,
                  hist_week_ozone_aqi = NA,
                  hist_week_pm25_aqi  = NA,
                  bb3_ozone_aqi       = NA,
                  bb3_pm25_aqi        = NA)

}

# Drop non-forecast sites 
results <- filter(results, !site_catid %in% c("27-137-9000", "27-137-0034"))


# Super mutate
results <- results %>% 
             ungroup() %>% 
             arrange(site_catid, forecast_date) %>% 
             mutate(n = 1:n()) %>% 
             rowwise() %>% 
             mutate(today               = forecast_date,
                    hist_date           = format(today, "2012-%m-%d") %>% as.Date(),
                    #two_days_ago       = forecast_date - 2,
                    persist_ozone_aqi   = ifelse(n < 3, NA, results$obs_ozone_aqi[(n - 2)]),
                    persist_pm25_aqi    = ifelse(n < 3, NA, results$obs_pm25_aqi[(n - 2)]),
                    roll_ozone_aqi      = ifelse((n < 10) || sum(is.na(results$obs_ozone_aqi[(n - 9):(n - 2)])) > 3, NA, median(results$obs_ozone_aqi[(n - 9):(n - 2)], na.rm = T)),
                    roll_pm25_aqi       = ifelse((n < 10) || sum(is.na(results$obs_pm25_aqi[(n - 9):(n - 2)])) > 3, NA, median(results$obs_pm25_aqi[(n - 9):(n - 2)], na.rm = T))
             )


# Join historical medians
results <- left_join(results, weeks_median)


# Add BB3 median of models
results <- mutate(results,
                  bb3_ozone_aqi = median(c(roll_ozone_aqi, persist_ozone_aqi, hist_week_ozone_aqi), na.rm = T),
                  bb3_pm25_aqi  = median(c(roll_pm25_aqi, persist_pm25_aqi, hist_week_pm25_aqi), na.rm = T))

# Super loop
if (F) {
i <- 1

for (i in seq_along(results$forecast_date)) {
  
  temp <- results[i, ]
  
  # Set dates
  today <- temp$forecast_date
  
  print(paste(today, temp$site_catid))
  
  if (today < as.Date("2017-06-01")) next()
  
  yesterday <- today - 1 
  
  two_days_ago <- today - 2
  

## Persistance = Previous day's result (2 days before forecast day)
persist <- filter(results, forecast_date == two_days_ago, site_catid == temp$site_catid) %>%
              mutate(forecast_date      = today,
                     persist_ozone_aqi  = obs_ozone_aqi,
                     persist_pm25_aqi   = obs_pm25_aqi) %>%
              select(forecast_date, persist_ozone_aqi, persist_pm25_aqi)
            

## Rolling 7-days
roll <- filter(results, forecast_date <= two_days_ago, forecast_date > (today - 9), site_catid == temp$site_catid) %>% 
          ungroup() %>%
          summarize(forecast_date   = today,
                    roll_ozone_aqi  = ifelse(sum(is.na(obs_ozone_aqi)) > 3, NA, median(obs_ozone_aqi, na.rm = T)),
                    roll_pm25_aqi   = ifelse(sum(is.na(obs_pm25_aqi)) > 3, NA, median(obs_pm25_aqi, na.rm = T)))



## Week median
if (FALSE) {
  
  # Load historical results
  data <- read_csv("../Forecast data/All Ozone and PM25 forecast fields - DarkSky.csv")
  
  
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
                roll11_ozone_ppb = median(max_avg8hr, na.rm = T),  
                roll11_pm25_ugm3 = median(pm25_avg, na.rm = T),
                roll11_ozone_aqi = roll11_ozone_ppb %>% conc2aqi("ozone"),  
                roll11_pm25_aqi  = roll11_pm25_ugm3 %>% conc2aqi("pm25"))
    
    week_med <- bind_rows(sub_data, week_med)
  }
  
# QC
unique(week_med$forecast_date)
  
# Duplicate Detroit Lakes observations for Leech Lake and Red Lake
det_lakes <- filter(week_med, site_catid == "27-005-2013")
  
leech_lake <- mutate(det_lakes, site_catid = "27-021-3410")
  
#red_lake <- mutate(det_lakes, site_catid = "27-007-2304")
  
week_med <- bind_rows(week_med, leech_lake)
  
# SAVE
write_csv(week_med, "../Forecast data/rolling_11-day_averages.csv")

}

week_med <- weeks_median %>% 
              filter(forecast_date == format(today, "2012-%m-%d"),
                     site_catid %in% c(temp$site_catid, temp$alt_siteid)) %>%
              mutate(forecast_date = today)

if (nrow(week_med) < 1) week_med[1, ] <- NA; print("Missing historical.")

# Average of 3 results above
#blend3 <- left_join(week_med, roll) %>% left_join(persist)

#if (nrow(blend3) < 1)  blend3 <- left_join(roll, week_med) %>% left_join(persist)

bb3_ozone_aqi <- median(c(roll$roll_ozone_aqi, persist$persist_ozone_aqi, week_med$roll11_ozone_aqi), na.rm = T)

bb3_pm25_aqi  <- median(c(roll$roll_pm25_aqi, persist$persist_pm25_aqi, week_med$roll11_pm25_aqi), na.rm = T)


results[i, ]$hist_week_ozone_aqi <- week_med$roll11_ozone_aqi
results[i, ]$hist_week_pm25_aqi  <- week_med$roll11_pm25_aqi

results[i, ]$roll_week_ozone_aqi <- roll$roll_ozone_aqi
results[i, ]$roll_week_pm25_aqi  <- roll$roll_pm25_aqi

results[i, ]$persist_ozone_aqi   <- persist$persist_ozone_aqi
results[i, ]$persist_pm25_aqi    <- persist$persist_pm25_aqi

results[i, ]$bb3_ozone_aqi       <- bb3_ozone_aqi
results[i, ]$bb3_pm25_aqi        <- bb3_pm25_aqi

}
}


## QC results



## Join ROBOT results
day1 <- filter(verify, forecast_day == 1) %>% 
          group_by(forecast_date, site_catid) %>% 
          slice(1)

day1 <- mutate(day1, 
               rf_robot_ozone_aqi = ifelse(!is.na(mod_aqi_o3), 
                                           mod_aqi_o3, 
                                           as.character(mod_aqi_o3_gen1)),
               rf_robot_pm25_aqi  = ifelse(!is.na(mod_aqi_pm), 
                                           mod_aqi_pm, 
                                           as.character(mod_aqi_pm_gen1)))

results <- left_join(select(results, -n, -today, -hist_date),
                     select(day1, 
                            forecast_date, 
                            site_catid, 
                            rf_robot_ozone_aqi, 
                            rf_robot_pm25_aqi,
                            cmaq_ozone_aqi))


## Drop non-ozone season obs
results <- results %>%
             mutate(obs_ozone_aqi = ifelse(as.numeric(format(forecast_date - 1, "%m")) %in% 4:10, obs_ozone_aqi, NA))


## Calc performance stats
stats <- gather(results, models, forecast, persist_ozone_aqi:cmaq_ozone_aqi) %>% 
          filter(forecast_date > "2017-05-31")

stats_o3 <-  %>% 
              filter(as.numeric(format(forecast_date - 1, "%m")) %in% 4:10)

stats <- stats %>% 
           group_by(site_catid, model) %>%
            



##


