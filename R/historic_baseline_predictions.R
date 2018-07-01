# Generate baseline AQI forecasting metrics

# Include results for these models:
#### - Persistance
#### - Rolling 7 day median
#### - Historical week median 

# Packages
library(tidyverse)

aircast_path  <- "https://raw.githubusercontent.com/dKvale/aircast/master/"
aqiwatch_path <- "https://raw.githubusercontent.com/dKvale/aqi-watch/master/"
results_path  <- "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/"

# AQI conversion functions
source(paste0(aqiwatch_path, "R/aqi_convert.R"))

dates <- seq(as.Date("2017-05-20"), as.Date("2018-05-31"), 1)


# Load sites
sites <- aqi_sites


# Drop outstate sites
sites <- filter(sites, !fcst_region %in% c("CA", "ND", "SD", "WI", "IA"))

# Drop Voyageurs & Leech Lake
sites <- filter(sites, !site_catid %in% c("27-137-9000", "27-021-3410")) 


# Drop dashes in IDs to match AQS
sites$aqsid <- gsub("-", "", sites$site_catid) %>% as.numeric()


# Filter to one site per forecast city
sites <- filter(sites, !site_catid %in% c('27-017-7416'))



# Load AirNow obs for 2017-2018
air_obs <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Verification/AQI History/2017_annual_aqi_obs.csv")

air_obs <- bind_rows(air_obs, read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Verification/AQI History/2018_annual_aqi_obs.csv"))


# One value per day
air_obs <- air_obs %>% 
           mutate(date = as.Date(date, "%m/%d/%y")) %>% 
           group_by(date, aqsid, City, Parameter) %>% 
           summarize(Concentration = median(Concentration, na.rm = T)) %>% 
           ungroup() %>%
           filter(date %in% dates, !aqsid %in% c('270177416'))


air_obs <- rename(air_obs, forecast_date = date)



# Create blank results table
blank_results <- data_frame(forecast_date = rep(dates, each = length(unique(sites$site_catid))),
                            site_catid    = rep(unique(sites$site_catid), length(dates)),
                            aqsid         = rep(unique(sites$aqsid), length(dates)),
                            join = 1)

# Expand results table to all parameters
if (FALSE) {
blank_results <- left_join(blank_results, 
                           data_frame(Parameter  = unique(air_obs$Parameter),
                                      join       = 1)) %>% 
                 select(-join)
}

# Load recent results
events <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Verification/event_table.csv")

events_all <- filter(events, forecast_date %in% dates)


min(events_all$forecast_date)

unique(events_all$forecast_date)


## Check site #'s
"27-137-0034" %in% events_all$site_catid

unique(events_all$site_catid %in% sites$site_catid)


# One value per day
events <- events_all %>% 
             group_by(forecast_date, site_catid) %>% 
             summarize(obs_ozone_aqi_e = median(obs_ozone_aqi, na.rm = T), 
                       obs_pm25_aqi_e  = median(obs_pm25_aqi, na.rm = T),
                       n = n())



# Load verification table
verify  <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Verification/verification_table2.csv")

verify <- verify %>% mutate(mod_max_avg8hr = as.numeric(mod_max_avg8hr),
                            mod_pm25avg    = as.numeric(mod_pm25avg))

verify2 <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Verification/Archive/2017_verification_table.csv")

verify2 <- verify2 %>% mutate(mod_aqi_o3 = as.character(mod_aqi_o3),
                              mod_aqi_pm = as.character(mod_aqi_pm ),
                              fcst_ozone_aqi = as.character(fcst_ozone_aqi))


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
verify <- verify %>% 
            group_by(forecast_date, site_catid) %>% 
            mutate(obs_ozone_ppb = median(obs_ozone_ppb, na.rm = T), 
                   obs_pm25_ugm3 = median(obs_pm25_ugm3, na.rm = T))


verify_day <- verify %>% 
              group_by(forecast_date, site_catid) %>% 
              summarize(obs_ozone_ppb = median(obs_ozone_ppb, na.rm = T), 
                        obs_pm25_ugm3 = median(obs_pm25_ugm3, na.rm = T))



# Join missing days to results
results <- left_join(blank_results, filter(verify, forecast_day == 1))

results <- left_join(results, events)


# Switch to AirNow results
results <- left_join(blank_results, air_obs)  %>% select(-join) 


# Drop site-parameter combos that don't exist
results <- results %>% 
           mutate(site_param = paste(aqsid, Parameter)) %>%
           filter(site_param %in% paste(air_obs$aqsid, air_obs$Parameter)) %>%
           select(-site_param)
  

if (FALSE) {
  
# Check missing
a <-  filter(results, is.na(obs_pm25_aqi), !is.na(obs_pm25_aqi_e))

a <-  filter(results, is.na(obs_ozone_aqi), !is.na(obs_ozone_aqi_e))


# Replace missing observations with converted AQI
results <- results %>% 
           mutate(obs_pm25_aqi  = ifelse(is.na(obs_pm25_aqi) & !is.na(obs_pm25_aqi_e), obs_pm25_aqi_e, obs_pm25_aqi),
                  obs_pm25_ugm3 = ifelse(is.na(obs_pm25_ugm3) & !is.na(obs_pm25_aqi_e), aqi2conc(obs_pm25_aqi_e, "pm25"), obs_pm25_ugm3),
                  obs_ozone_aqi = ifelse(is.na(obs_ozone_aqi) & !is.na(obs_ozone_aqi_e), obs_ozone_aqi_e, obs_ozone_aqi),
                  obs_ozone_ppb = ifelse(is.na(obs_ozone_ppb) & !is.na(obs_ozone_aqi_e), aqi2conc(obs_ozone_aqi_e, "ozone"), obs_ozone_ppb))

}

# Drop wildfire events
if (FALSE) {
  
  event_dates <- paste(filter(events_all, verified_bad_obs == "Y")$forecast_date, 
                       filter(events_all, verified_bad_obs == "Y")$site_catid)
  
  results <- results %>% 
              select(-obs_pm25_aqi_e, -obs_ozone_aqi_e, -n) %>%
              mutate(obs_pm25_aqi = ifelse(paste(forecast_date, site_catid) %in% event_dates, NA, obs_pm25_aqi),
                     obs_pm25_ugm3 = ifelse(paste(forecast_date, site_catid) %in% event_dates, NA, obs_pm25_ugm3))
  
}


# Historical results
weeks_median <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Forecast data/rolling_11-day_averages.csv") %>% 
                mutate(hist_week_ozone_ppb = aqi2conc(roll11_ozone_aqi, "ozone"), 
                       hist_week_pm25_ugm3 = aqi2conc(roll11_pm25_aqi, "pm2.5")) %>% 
                rename(hist_date = forecast_date)


# Drop non-forecast sites 
results <- filter(results, !site_catid %in% c("27-137-9000", "27-137-0034"))


# Spread wide format
results <- tidyr::spread(results, Parameter, Concentration) %>% 
           rename(obs_ozone_ppb = `OZONE-8HR`,   #aqi2conc(`OZONE-8HR`, "ozone"), 
                  obs_pm25_ugm3 = `PM2.5-24hr`)  #aqi2conc(`PM2.5-24hr`, "pm2.5"))


# Add predictions
results <- results %>% 
             ungroup() %>% 
             arrange(site_catid, forecast_date) %>%
             mutate(n = 1:n()) %>%
             group_by(site_catid) %>%
             mutate(n_site = 1:n())

results <- results %>% 
             rowwise() %>% 
             mutate(today               = forecast_date,
                    hist_date           = format(today, "2012-%m-%d") %>% as.Date(),
                    persist_ozone_ppb   = ifelse(n_site < 3, NA, results$obs_ozone_ppb[(n - 2)]),
                    persist_pm25_ugm3   = ifelse(n_site < 3, NA, results$obs_pm25_ugm3[(n - 2)]),
                    roll_ozone_ppb      = ifelse((n_site < 10) || sum(is.na(results$obs_ozone_ppb[(n - 9):(n - 2)])) > 8, NA, median(results$obs_ozone_ppb[(n - 9):(n - 2)], na.rm = T)),
                    roll_pm25_ugm3      = ifelse((n_site < 10) || sum(is.na(results$obs_pm25_ugm3[(n - 9):(n - 2)])) > 8, NA, median(results$obs_pm25_ugm3[(n - 9):(n - 2)], na.rm = T))
             ) %>%
             select(-n_site, -`n`, -today)



# Join historical medians
results <- left_join(results, weeks_median[ , c(1,2,5,6)]) %>% select(-hist_date)



# Add BB3 median of models
results <- results %>%
           rowwise() %>% 
           mutate(bb3_ozone_ppb = median(c(roll_ozone_ppb, persist_ozone_ppb, hist_week_ozone_ppb), na.rm = T),
                  bb3_pm25_ugm3 = median(c(roll_pm25_ugm3, persist_pm25_ugm3, hist_week_pm25_ugm3), na.rm = T))



## QC results

if (FALSE) {
## Join ROBOT results
day1 <- filter(verify, forecast_day == 1) %>% 
        group_by(forecast_date, site_catid) %>% 
        slice(1)


day1 <- mutate(day1, 
               rf_robot_ozone_ppb = ifelse(!is.na(mod_max_avg8hr), 
                                           mod_max_avg8hr, 
                                           mod_max_avg8hr_gen1),
               rf_robot_ozone_aqi = ifelse(!is.na(mod_aqi_o3), 
                                           mod_aqi_o3, 
                                           as.character(mod_aqi_o3_gen1)),
               rf_robot_pm25_ugm3  = ifelse(!is.na(mod_pm25avg), 
                                           mod_pm25avg, 
                                           mod_pm25avg_gen1),
               rf_robot_pm25_aqi = ifelse(!is.na(mod_aqi_pm), 
                                          mod_aqi_pm, 
                                          as.character(mod_aqi_pm_gen1)))

results <- left_join(select(results, -c(n,
                                        today, 
                                        hist_date, 
                                        mod_max_avg8hr, 
                                        mod_max_avg8hr_gen1, 
                                        mod_aqi_o3, 
                                        mod_aqi_o3_gen1,
                                        mod_pm25avg, 
                                        mod_pm25avg_gen1,
                                        mod_aqi_pm,
                                        mod_aqi_pm_gen1)),
                     select(day1, 
                            forecast_date, 
                            site_catid, 
                            rf_robot_ozone_ppb, 
                            rf_robot_pm25_ugm3))

}

## Drop non-ozone season obs
results <- results %>%
             mutate(ozone_season  = as.numeric(format(forecast_date, "%m")) %in% 4:10,
                    obs_ozone_ppb = ifelse(ozone_season, obs_ozone_ppb, NA)) 
                    #obs_ozone_aqi = ifelse(ozone_season, obs_ozone_aqi, NA))


## Join site name
results <- left_join(results, select(sites, air_monitor, site_catid)) 


# Non-weekend forecast dates (Not Sunday or Monday)
## Forecasters don't submit day 1 forecasts on these days
results <- results %>% mutate(weekend = weekdays(forecast_date) %in% c("Sunday", "Monday"))

## Calc performance stats
names(results)

stats <- gather(results, model, forecast, persist_ozone_ppb:bb3_pm25_ugm3) %>% 
         filter(forecast_date > "2017-05-31", !weekend)

stats_o3 <-  stats %>% 
               filter(ozone_season, grepl("ppb", model), !is.na(obs_ozone_ppb)) %>%
               rowwise() %>%
               mutate(correct = (obs_ozone_ppb > 55 & forecast > 55) |
                                (obs_ozone_ppb <= 55 & forecast <= 55)) %>%
               group_by(site_catid, air_monitor, model) %>%
               summarize(n     = sum(!is.na(obs_ozone_ppb) & !is.na(forecast)),
                         bias  = mean((obs_ozone_ppb - forecast), na.rm = T),
                         rmse  = (mean((obs_ozone_ppb - forecast)**2, na.rm = T)) %>% sqrt(),
                         mae   = mean(abs(obs_ozone_ppb - forecast), na.rm = T),
                         category_correct = sum(correct, na.rm = T) / n,
                         green_correct    = sum(correct[obs_ozone_ppb < 55], na.rm = T) / sum(obs_ozone_ppb < 55 & !is.na(forecast), na.rm = T),
                         green_false      = sum(!correct[obs_ozone_ppb > 55], na.rm = T) / sum(obs_ozone_ppb > 55 & !is.na(forecast), na.rm = T),
                         yellow_correct   = sum(correct[obs_ozone_ppb > 55], na.rm = T) / sum(obs_ozone_ppb > 55 & !is.na(forecast), na.rm = T),
                         yellow_false     = sum(!correct[obs_ozone_ppb < 55], na.rm = T) / sum(obs_ozone_ppb < 55 & !is.na(forecast), na.rm = T)) %>%
               mutate_at(vars(bias:yellow_false), round , 2)


stats_o3_overall <- stats_o3 %>% 
                      filter(n > 70) %>%
                      group_by(model) %>%
                      summarize_at(vars(bias:yellow_false), median, na.rm = T) %>%
                      mutate_at(vars(bias:yellow_false), round , 2)


stats_pm <- stats %>% 
            filter(grepl("ugm3", model), !is.na(obs_pm25_ugm3)) %>%
            rowwise() %>%
            mutate(correct = (obs_pm25_ugm3 > 12 & forecast > 12) |
                             (obs_pm25_ugm3 <= 12 & forecast <= 12)) %>%
            group_by(site_catid, air_monitor, model) %>%
            summarize(n     = sum(!is.na(obs_pm25_ugm3) & !is.na(forecast)),
                      bias  = mean((obs_pm25_ugm3 - forecast), na.rm = T),
                      rmse  = (mean((obs_pm25_ugm3 - forecast)**2, na.rm = T)) %>% sqrt(),
                      mae   = mean(abs(obs_pm25_ugm3 - forecast), na.rm = T),
                      category_correct = sum(correct, na.rm = T) / n,
                      green_correct    = sum(correct[obs_pm25_ugm3 < 12], na.rm = T) / sum(obs_pm25_ugm3 < 12 & !is.na(forecast), na.rm = T),
                      green_false      = sum(!correct[obs_pm25_ugm3 > 12], na.rm = T) / sum(obs_pm25_ugm3 > 12 & !is.na(forecast), na.rm = T),
                      yellow_correct   = sum(correct[obs_pm25_ugm3 > 12], na.rm = T) / sum(obs_pm25_ugm3 > 12 & !is.na(forecast), na.rm = T),
                      yellow_false     = sum(!correct[obs_pm25_ugm3 < 12], na.rm = T) / sum(obs_pm25_ugm3 < 12 & !is.na(forecast), na.rm = T)) %>%
            mutate_at(vars(bias:yellow_false), round , 2)

            
stats_pm_overall <- stats_pm %>% 
                    filter(n > 70) %>%
                    group_by(model) %>%
                    summarize_at(vars(bias:yellow_false), median, na.rm = T) %>%
                    mutate_at(vars(bias:yellow_false), round , 2)


## Save
write_csv(results, 
          paste0(results_path, "/Verification/Day1_history/", format(min(results$forecast_date), "%Y"), "_Day1_forecast_history_airnow_results.csv"))

# Save performance stats
write_csv(stats_pm, 
          paste0(results_path, "/Verification/Day1_history/", format(min(results$forecast_date), "%Y"), "_PM25_day1_baseline_performance.csv"))

write_csv(stats_o3, 
          paste0(results_path, "/Verification/Day1_history/", format(min(results$forecast_date), "%Y"), "_Ozone_day1_baseline_performance.csv"))
##


