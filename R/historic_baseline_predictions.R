# Generate baseline AQI forecasting metrics

# Include results for these models:
#### - Persistance
#### - Rolling 7 day median
#### - Historical week median 


# Packages
library(tidyverse)


# AQI conversion functions
source("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Web/aqi-watch/R/aqi_convert.R")

dates <- seq(as.Date("2017-05-20"), as.Date("2018-05-31"), 1)


# Load sites
sites <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff folders/Dorian/AQI/MET data/Monitors and Rep Wx Stations.csv")

names(sites) <- gsub(" ", "_", tolower(names(sites)))


# Load recent results
events <- read_csv("../Verification/event_table.csv")

events2 <- read_csv("../Verification/event_table.csv")

events_all <- bind_rows(events, events2)

events_all <- filter(events_all, forecast_date %in% dates)

rm(events2)

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
verify <- verify %>% 
            group_by(forecast_date, site_catid) %>% 
            mutate(obs_ozone_ppb = median(obs_ozone_ppb, na.rm = T), 
                   obs_pm25_ugm3 = median(obs_pm25_ugm3, na.rm = T))


verify_day <- verify %>% 
              group_by(forecast_date, site_catid) %>% 
              summarize(obs_ozone_ppb = median(obs_ozone_ppb, na.rm = T), 
                        obs_pm25_ugm3 = median(obs_pm25_ugm3, na.rm = T))

# Join missing days to results
blank_results <- data_frame(forecast_date = rep(dates, each = 30),
                            site_catid    = rep(unique(results$site_catid), 
                                                length(dates)))

results <- left_join(blank_results, filter(verify, forecast_day == 1))

results <- left_join(results, events)
   
               
# Check missing
a <-  filter(results, is.na(obs_pm25_aqi), !is.na(obs_pm25_aqi_e))

a <-  filter(results, is.na(obs_ozone_aqi), !is.na(obs_ozone_aqi_e))

results <- results %>% mutate(obs_pm25_aqi  = ifelse(is.na(obs_pm25_aqi) & !is.na(obs_pm25_aqi_e), obs_pm25_aqi_e, obs_pm25_aqi),
                              obs_pm25_ugm3 = ifelse(is.na(obs_pm25_ugm3) & !is.na(obs_pm25_aqi_e), aqi2conc(obs_pm25_aqi_e, "pm25"), obs_pm25_ugm3),
                              obs_ozone_aqi = ifelse(is.na(obs_ozone_aqi) & !is.na(obs_ozone_aqi_e), obs_ozone_aqi_e, obs_ozone_aqi),
                              obs_ozone_ppb = ifelse(is.na(obs_ozone_ppb) & !is.na(obs_ozone_aqi_e), aqi2conc(obs_ozone_aqi_e, "ozone"), obs_ozone_ppb))


# Drop wildfire events
event_dates <- paste(filter(events_all, verified_bad_obs == "Y")$forecast_date, filter(events_all, verified_bad_obs == "Y")$site_catid)

results <- results %>% 
            select(-obs_pm25_aqi_e, -obs_ozone_aqi_e, -n) %>%
            mutate(obs_pm25_aqi = ifelse(paste(forecast_date, site_catid) %in% event_dates, NA, obs_pm25_aqi),
                   obs_pm25_ugm3 = ifelse(paste(forecast_date, site_catid) %in% event_dates, NA, obs_pm25_ugm3))


# Historical results
weeks_median <- read_csv("../Forecast data/rolling_11-day_averages.csv")

names(weeks_median)[2:6] <-  c("hist_date", 
                               "hist_week_ozone_ppb", 
                               "hist_week_pm25_ugm3",
                               "hist_week_ozone_aqi", 
                               "hist_week_pm25_aqi")


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
                    persist_ozone_ppb   = ifelse(n < 3, NA, results$obs_ozone_ppb[(n - 2)]),
                    persist_pm25_ugm3   = ifelse(n < 3, NA, results$obs_pm25_ugm3[(n - 2)]),
                    roll_ozone_ppb      = ifelse((n < 10) || sum(is.na(results$obs_ozone_ppb[(n - 9):(n - 2)])) > 3, NA, median(results$obs_ozone_ppb[(n - 9):(n - 2)], na.rm = T)),
                    roll_pm25_ugm3      = ifelse((n < 10) || sum(is.na(results$obs_pm25_ugm3[(n - 9):(n - 2)])) > 3, NA, median(results$obs_pm25_ugm3[(n - 9):(n - 2)], na.rm = T))
             )

# Join historical medians
results <- left_join(results, weeks_median[ , 1:4])


# Add BB3 median of models
results <- mutate(results,
                  bb3_ozone_ppb = median(c(roll_ozone_ppb, persist_ozone_ppb, hist_week_ozone_ppb), na.rm = T),
                  bb3_pm25_ugm3  = median(c(roll_pm25_ugm3, persist_pm25_ugm3, hist_week_pm25_ugm3), na.rm = T))



## QC results



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


## Drop non-ozone season obs
results <- results %>%
             mutate(ozone_season  = as.numeric(format(forecast_date - 1, "%m")) %in% 4:10,
                    obs_ozone_ppb = ifelse(ozone_season, obs_ozone_ppb, NA),
                    obs_ozone_aqi = ifelse(ozone_season, obs_ozone_aqi, NA))


## Calc performance stats
names(results)

stats <- gather(results, model, forecast, cmaq_ozone_ppb:rf_robot_pm25_ugm3) %>% 
           filter(forecast_date > "2017-05-31")

stats_o3 <-  stats %>% 
               filter(ozone_season, grepl("ppb", model)) %>%
               group_by(site_catid, model) %>%
               summarize(mean_bias = mean((obs_ozone_ppb - forecast), na.rm = T),
                         rmse      = (mean((obs_ozone_ppb - forecast)**2, na.rm = T)) %>% sqrt())
               

stats_o3_overall <- stats_o3 %>% 
                      group_by(model) %>%
                      summarize(bias    = mean(mean_bias, na.rm = T),
                                rmse    = mean(rmse, na.rm = T))

stats_pm <- stats %>% 
              filter(grepl("ugm3", model)) %>%
              group_by(site_catid, model) %>%
              summarize(mean_bias = mean((obs_pm25_ugm3 - forecast), na.rm = T),
                        rmse      = (mean((obs_pm25_ugm3 - forecast)**2, na.rm = T)) %>% sqrt())

            
stats_pm_overall <- stats_pm %>% 
                      group_by(model) %>%
                      summarize(bias    = mean(mean_bias, na.rm = T),
                                rmse    = mean(rmse, na.rm = T))


## Save
write_csv(results, "Day1_history/Day1_forecast_history.csv")


##


