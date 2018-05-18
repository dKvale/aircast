library(tidyverse)


verify <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Verification/2017_verification_table.csv")

fond <- filter(verify, group == "Fond du Lac" | site_catid == "27-017-7417") %>%  
          group_by(forecast_date) %>%
          mutate(obs_pm25_ugm3 = max(obs_pm25_ugm3, na.rm = T)) %>%
          ungroup()


yellow  <- filter(fond, obs_pm25_ugm3 > 12)

yellow1 <- filter(fond, obs_pm25_ugm3 > 12, forecast_day == 1)

# Day 1 Category accuracy
day1 <- filter(fond, forecast_day == 1) %>% mutate(correct = (mod_pm25avg < 12.1 & obs_pm25_ugm3 < 12.1) |  (mod_pm25avg >= 12.1 & obs_pm25_ugm3 >= 12.1))

acc <- day1 %>% summarize(total   = sum(!is.na(obs_pm25_ugm3) & !is.na(mod_pm25avg)),
                          correct = sum((mod_pm25avg < 12.1 & obs_pm25_ugm3 < 12.1) |  
                                        (mod_pm25avg >= 12.1 & obs_pm25_ugm3 >= 12.1), na.rm = T),
                          acc     = 100 * correct / total)



