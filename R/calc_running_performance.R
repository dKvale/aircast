

verify <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Verification/verification_table2.csv")


verify[verify$site_catid == "27-137-0034", "site_catid"] <- "27-137-9000"

verify %<>% filter(forecast_day == 1)


#-- Join missing site names
sites <- read_csv(paste0(aircast_path, "data/monitors_and_wx_stations.csv"))

names(sites) <- gsub(" ", "_", tolower(names(sites)))

verify <- left_join(select(verify, -contains("short_name")), 
                    select(sites, short_name, site_catid), by = "site_catid")


#-- Recalculate AQI
verify$obs_pm25_aqi <- conc2aqi(verify$obs_pm25_ugm3, "pm25")


# Set forecast to numeric
verify <- mutate(verify, 
                 fcst_ozone_aqi = as.numeric(fcst_ozone_aqi),
                 fcst_pm25_aqi  = as.numeric(fcst_pm25_aqi))

#-- Change column name to match CMAQ
names(verify)[grep("site_catid", names(verify))] <- "aqs_id"


#-- Select columns for report
verify <- select(verify, short_name, group, aqs_id, forecast_date,
                 count_ozone_obs, obs_ozone_ppb, obs_ozone_aqi, fcst_ozone_aqi, cmaq_ozone_aqi, 
                 persist_ozone_aqi, roll_ozone_aqi, roll11_ozone_aqi, bb3_ozone_aqi,
                 count_pm25_obs, obs_pm25_ugm3, obs_pm25_aqi, fcst_pm25_aqi,
                 persist_pm25_aqi, roll_pm25_aqi, roll11_pm25_aqi, bb3_pm25_aqi,
                 mod_max_avg8hr_ens:mod_aqi_pm_superens,
                 cmaq_prod_o3:cmaq_prod_pm_aqi) %>%
  select(-contains("super"))


names(verify)[1:3] <- c("Site", "Group", "AQS ID") 



#-- Clip long site names
verify$Site <- ifelse(verify$Site == "Duluth_WDSE", "Duluth WD", verify$Site)
verify$Site <- ifelse(verify$Site == "St_Paul_Ramsey_Health", "St Paul Ramsey", verify$Site)
verify$Site <- ifelse(verify$Site == "MSP_HC_Anderson", "HC Andersen", verify$Site)
verify$Site <- ifelse(verify$Site == "HC_Anderson", "HC Andersen", verify$Site)
verify$Site <- ifelse(verify$Site == "MSP_St_Louis_Park", "St Louis Park", verify$Site)


# Remove underscores from names
verify$Site  <- gsub("_", " ", verify$Site)
verify$Group <- gsub("_", " ", verify$Group)


#-- Select highest forecast per group
verify_pm <- verify %>%
              group_by(Group, forecast_date) %>%
              arrange(-obs_pm25_ugm3) %>%
              mutate_if(is.numeric, max, na.rm = T) %>%
              slice(1) %>%
              ungroup()

verify_pm <- rename(verify_pm,
                    `Ozone obs count` = count_ozone_obs,
                    `Ozone ppb`       = obs_ozone_ppb, 
                    `Ozone AQI`       = obs_ozone_aqi,
                    `Ozone forecast`  = fcst_ozone_aqi,
                    `CMAQ forecast`   = cmaq_ozone_aqi,
                    `PM2.5 obs count` = count_pm25_obs, 
                    `PM2.5 ug/m3`     = obs_pm25_ugm3, 
                    `PM2.5 AQI`       = obs_pm25_aqi, 
                    `PM2.5 forecast`  = fcst_pm25_aqi)


verify_o3 <- verify %>%
              group_by(Group, forecast_date) %>%
              arrange(-obs_ozone_ppb) %>%
              mutate_if(is.numeric, max, na.rm = T) %>%
              slice(1) %>%
              ungroup()


verify_o3 <- rename(verify_o3,
                    `Ozone obs count` = count_ozone_obs,
                    `Ozone ppb`       = obs_ozone_ppb, 
                    `Ozone AQI`       = obs_ozone_aqi,
                    `Ozone forecast`  = fcst_ozone_aqi,
                    `CMAQ forecast`   = cmaq_ozone_aqi,
                    `PM2.5 obs count` = count_pm25_obs, 
                    `PM2.5 ug/m3`     = obs_pm25_ugm3, 
                    `PM2.5 AQI`       = obs_pm25_aqi, 
                    `PM2.5 forecast`  = fcst_pm25_aqi)


# Join together
verify <- bind_rows(verify_pm, verify_o3)

verify[verify == -Inf]       <- NA
verify_o3[verify_o3 == -Inf] <- NA
verify_pm[verify_pm == -Inf] <- NA



# Ozone table
verify_o3 <- select(verify_o3, -c(`AQS ID`,
                                  `PM2.5 obs count`:bb3_pm25_aqi, 
                                  mod_pm25avg_ens:mod_aqi_pm_rf, 
                                  contains("_pm"))) %>% 
             filter(!is.na(`Ozone forecast`), !is.na(`Ozone AQI`)) %>%
             arrange(-`Ozone AQI`)

names(verify_o3)[c(3:8,13)] <- c("date", "Obs count", "conc", "Obs AQI", "MPCA forecast", "CMAQ forecast", "Model output")

names(verify_o3) <- gsub("max_avg8hr", "conc", names(verify_o3))

names(verify_o3) <- gsub("_o3_", "_", names(verify_o3))


# PM2.5 table
verify_pm <- select(verify_pm, c(Site, Group, forecast_date,
                                 `PM2.5 obs count`:bb3_pm25_aqi, 
                                 mod_pm25avg_ens:mod_aqi_pm_rf,
                                 contains("_pm"))) %>% 
              filter(!is.na(`PM2.5 forecast`), !is.na(`PM2.5 AQI`)) %>%
              arrange(-`PM2.5 AQI`)

names(verify_pm)[c(3:7,12)]  <- c("date", "Obs count", "conc", "Obs AQI", "MPCA forecast", "Model output")

names(verify_pm) <- gsub("pm25avg", "conc", names(verify_pm))

names(verify_pm) <- gsub("_pm_", "_", names(verify_pm))



# Flip to tall table
tall_pm <- tidyr::gather(select(verify_pm, -c(conc, `Model output`:mod_conc_ab, cmaq_prod_pm)),
                         key   = model, 
                         value = aqi, 
                         na.rm = FALSE, 
                         `Obs AQI`:names(verify_pm)[length(names(verify_pm))])

tall_o3 <- tidyr::gather(select(verify_o3, -c(conc, `Model output`:mod_conc_ab, cmaq_prod_o3)),
                         key   = model, 
                         value = aqi, 
                         na.rm = FALSE, 
                         `Obs AQI`:names(verify_o3)[length(names(verify_o3))])


# Set colors
# Convert to concentration
tall_o3 <- tall_o3 %>% 
            rowwise() %>%
            mutate(background_color = aqi2color(aqi),
                   conc             = aqi2conc(aqi, "OZONE"))

tall_pm <- tall_pm %>% 
            rowwise() %>%
            mutate(background_color = aqi2color(aqi),
                   conc             = aqi2conc(aqi, "PM25"))


# Calculate model-monitor concentration difference
tall_o3 <- tall_o3 %>% 
            group_by(Site, date) %>%
            mutate(`conc_diff (ppb)` = conc - conc[model == "Obs AQI"],
                   fcst_correct      = identical(background_color[model == "Obs AQI"], 
                                                 background_color[model == "MPCA forecast"]))


tall_pm <- tall_pm %>%
            group_by(Site, date) %>%
            mutate(`conc_diff (ug/m3)` = conc - conc[model == "Obs AQI"],
                   fcst_correct        = identical(background_color[model == "Obs AQI"], 
                                                   background_color[model == "MPCA forecast"]))
          

# Load past results
lead_board <- try(read_csv(paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff folders/Dorian/AQI/Verification/Archive leaders/", 
                                  Sys.Date(), "-lead_board.csv")),
                  silent = TRUE)



names(lead_board) <- gsub("Forecasts n", "Forecasts", names(lead_board))

names(lead_board)[12] <- "PM Forecasts"


# New performance metrics
pm_performance <- tall_pm %>%
                  filter(!is.na(conc)) %>%
                  group_by(Site, date) %>%
                  mutate(n            = 1:n(),
                         obs_col      = background_color[model == "Obs AQI"],
                         obs_conc     = conc[model == "Obs AQI"],
                         fcst_correct = obs_col == background_color[n],
                         conc_diff    = abs(obs_conc - conc[n])) %>%
                  group_by(model) %>%
                  summarize(n_obs          = sum(!is.na(tall_pm$aqi[tall_pm$model == "Obs AQI"])),
                            n_preds        = sum(!is.na(fcst_correct)),
                            elev_preds     = sum(!is.na(fcst_correct) & obs_col != "#53BF33"), 
                            accuracy       = 100 * sum(fcst_correct, na.rm = T) / n_preds,
                            elev_accuracy  = 100 * sum(fcst_correct & obs_col != "#53BF33", na.rm = T) / elev_preds, 
                            mean_conc_diff = mean(conc_diff, na.rm = T),
                            bulls_eyes     = 100 * sum(conc_diff <= 2.5, na.rm = T) / n_preds,
                            near_misses    = 100 * sum(conc_diff <= 5, na.rm = T) / n_preds) %>%
                  arrange(desc(model))


o3_performance <- tall_o3 %>%
                  filter(!is.na(conc)) %>%
                  group_by(Site, date) %>%
                  mutate(n            = 1:n(),
                         obs_col      = background_color[model == "Obs AQI"],
                         obs_conc     = conc[model == "Obs AQI"],
                         fcst_correct = obs_col == background_color[n],
                         conc_diff    = abs(obs_conc - conc[n])) %>%
                  group_by(model) %>%
                  summarize(n_obs          = sum(!is.na(tall_o3$aqi[tall_o3$model == "Obs AQI"])),
                            n_preds        = sum(!is.na(fcst_correct)),
                            elev_preds     = sum(!is.na(fcst_correct) & obs_col != "#53BF33"), 
                            accuracy       = 100 * sum(fcst_correct, na.rm = T) / n_preds,
                            elev_accuracy  = 100 * sum(fcst_correct & obs_col != "#53BF33", na.rm = T) / elev_preds, 
                            mean_conc_diff = mean(conc_diff, na.rm = T),
                            bulls_eyes     = 100 * sum(conc_diff <= 5, na.rm = T) / n_preds,
                            near_misses    = 100 * sum(conc_diff <= 10, na.rm = T) / n_preds) %>%
                  arrange(desc(model))


## New performance table
model_names <- data_frame(Model = c("MPCA Team", 
                                    "Dr Robot", 
                                    "GradBooster", 
                                    "Big RF", 
                                    "Ensemble",  
                                    "CMAQ", 
                                    "Farmer Weeks", 
                                    "Persistant Assistant",
                                    "J.K. Rollings",
                                    "BB3 Blender",
                                    "CMAQ 6z",
                                    "CMAQ 6z beta+"),
                          model_o3 = c("MPCA forecast",
                                      "mod_aqi_gen1",
                                      "mod_aqi_gb",
                                      "mod_aqi_rf",
                                      "mod_aqi_ens",
                                      "CMAQ forecast",
                                      "roll11_ozone_aqi",
                                      "persist_ozone_aqi",
                                      "roll_ozone_aqi", 
                                      "bb3_ozone_aqi",
                                      "cmaq_prod_o3",
                                      "cmaq_bc_o3"),
                          model_pm = c("MPCA forecast",
                                      "mod_aqi_gen1",
                                      "mod_aqi_gb",
                                      "mod_aqi_rf",
                                      "mod_aqi_ens",
                                      "CMAQ forecast",
                                      "roll11_pm25_aqi",
                                      "persist_pm25_aqi",
                                      "roll_pm25_aqi", 
                                      "bb3_pm25_aqi",
                                      "cmaq_prod_aqi",
                                      "cmaq_bc_aqi"))

# Combine
yest_board <- left_join(model_names, o3_performance, by = c("model_o3" = "model")) %>%
                select(-model_o3, -model_pm, -n_obs, -n_preds, -elev_preds) %>%
              left_join(left_join(model_names, pm_performance, by = c("model_pm" = "model")), by = "Model") %>%
                select(-model_o3, -model_pm, -n_obs, -n_preds, -elev_preds)

yest_board <- rename(yest_board,
                     `O3 cat accuracy %`    = accuracy.x,
                     `O3 yellow accuracy %` = elev_accuracy.x,
                     `O3 error ppb`         = mean_conc_diff.x,
                     `O3 bullseyes %`       = bulls_eyes.x,
                     `O3 on target %`       = near_misses.x,
                     
                     `PM cat accuracy %`    = accuracy.y,
                     `PM yellow accuracy %` = elev_accuracy.y,
                     `PM error ug/m3`       = mean_conc_diff.y,
                     `PM bullseyes %`       = bulls_eyes.y,
                     `PM on target %`       = near_misses.y)


# Add CMAQ models to lead_board
lead_board <- bind_rows(lead_board, filter(yest_board, grepl("CMAQ 6", Model)))

lead_board[grepl("CMAQ 6", lead_board$Model), "PM Forecasts"] <- (pm_performance[grepl("cmaq", pm_performance$model), "n_preds"] / 15)[1:2, ] 


# SAVE updated leader board
write_csv(lead_board,
          paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff folders/Dorian/AQI/Verification/Archive leaders/", 
                 Sys.Date(), "-lead_board.csv"))
