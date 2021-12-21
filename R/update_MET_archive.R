#! /usr/bin/env Rscript

library(dplyr)
library(readr)
library(darksky) #devtools::install_github("hrbrmstr/darksky")

options(digits = 12)


aircast_path  <- "https://raw.githubusercontent.com/dKvale/aircast/master/"
aqiwatch_path <- "https://raw.githubusercontent.com/dKvale/aqi-watch/master/"
results_path  <- "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/"


# Load site locations
aqi_sites <- read_csv(paste0(aircast_path,
                             "data/monitors_and_wx_stations.csv"))

names(aqi_sites) <- gsub(" ", "_", tolower(names(aqi_sites)))


#"C:\Users\dkvale\Documents\R\R-4.0.2\bin\x64\Rscript.exe" --no-save --no-restore "X:\Agency_Files\Outcomes\Risk_Eval_Air_Mod\_Air_Risk_Evaluation\Staff folders\Dorian\AQI\aircast\R\update_MET_archive.R"


# AQI monitoring sites
sites <- aqi_sites

# DarkSky key
d_key <- 'ea8610622c9d63c30ca25dea03ec3d90'
#darksky_api_key(key = d_key, force = T)

Sys.setenv(DARKSKY_API_KEY = d_key)


# Create date table
days <- tibble(date = seq(as.Date("2019-12-31"), as.Date("2021-08-02"), 1),
               join = 1)

days[1:5, ]

day <- 5

days[day, ]


# MET file structure
forecast_col_names <- c("site_catid", "time", "summary", "icon", 
                        "precipIntensity", "precipProbability", "temperature", "apparentTemperature", "dewPoint", "humidity", "pressure", "windSpeed", "windGust", 
                        "windBearing", 
                        "cloudCover", "visibility", "precipAccumulation", 
                        "precipType")

forecast_col_types <- 'ccccdddddddddidddc'


# Generate table of downloaded site-dates
## Or load the pre-generated list
folder <- "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/MET data/DarkSky database/sites" 

all_met <- read_csv(paste0(folder, "/../progress_on_downloaded_met_site_data.csv"))

if(F) {
    
  files <- list.files(folder)
    
  all_met <- read_csv(paste0(folder, "/", files[1]), guess_max = 5) %>% 
             filter(!is.na(site_catid), 
                    site_catid == gsub("[.csv]", "", files[1])) %>%
             rowwise() %>%
             mutate(date = format(time, "%Y-%m-%d")) %>%
             select(site_catid, date) %>% 
             group_by(site_catid, date) %>% 
             slice(1)
  
  
  for (file in files[-1]) {
      
      print(file)
      
      tmp <- read_csv(paste0(folder, "/", file), guess_max = 5) %>% 
             filter(!is.na(site_catid), 
                    site_catid == gsub("[.csv]", "", file))
      
      if (nrow(tmp) > 0) {
      
        tmp$date <- format(tmp$time, "%Y-%m-%d") 
        
        tmp <- tmp %>%
               select(site_catid, date) %>% 
               group_by(site_catid, date) %>% 
               slice(1)
        
        all_met <- bind_rows(tmp, all_met)
  }
  }

}

# Create site/date identifier
names(all_met)

all_met$site_date <- paste(all_met$site_catid, all_met$date)


# Add air toxics sites
all_sites <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff folders/Dorian/AQI/MET data/AirToxics_sites.csv")

all_sites <- filter(all_sites, Year %in% 2010:2019) %>%
             select(Report_Name, AQS_ID, lat, long) %>% 
             rowwise() %>%
             mutate(AQS_ID = paste(substring(AQS_ID, 1, 2), 
                                   substring(AQS_ID, 3, 5), 
                                   substring(AQS_ID, 6, 9), sep = "-")) %>%
            group_by(AQS_ID) %>%
            slice(1)

names(all_sites) <- c("air_monitor", "site_catid", "monitor_lat", "monitor_long")

all_sites$run_order <- 3

sites$run_order <- 2

sites <- bind_rows(sites, 
                   filter(all_sites, !site_catid %in% sites$site_catid))

# Drop duplicate Fond du Lac site
sites <- filter(sites, !site_catid %in% "27-017-7417")


# Combine Rosemounts- Flint Hills
sites <- filter(sites, !site_catid %in% c("27-037-0020", "27-037-0423", "27-037-0442"))


# Combine Red Lake Nation
sites <- filter(sites, !site_catid %in% "27-007-2303")


# Drop sites with similar coordinates
sites2 <- sites %>%
          group_by(round(monitor_lat, 1), round(monitor_long, 1), 
                   site_catid == "27-137-7549") %>% # Keeps Duluth Mich. St. seperate from WDSE
          slice(1)

sites$air_monitor[!sites$site_catid %in% sites2$site_catid]


sites <- sites2


# Join sites to calendar
sites$join <- 1

sites <- left_join(sites, days)

sites$join <- NULL

sites$site_date <- paste(sites$site_catid, sites$date)


# Put 909 first
#sites$run_order <- ifelse(sites$site_catid %in% c("27-053-0909"), 1, sites$run_order) #"27-053-0910" Pacific street

# Put AQI sites first
sites$run_order <- ifelse(sites$site_catid %in% aqi_sites$site_catid, 1.1, sites$run_order)

# Put PFAS sites first
sites$run_order <- ifelse(sites$site_catid %in% c("27-037-0465", "27-031-7810", "27-053-2006", "27-137-7549", "27-137-7555"), 1, sites$run_order)


sites <- arrange(sites, run_order, site_catid, desc(date))


# Drop dates already downloaded 
sites <- filter(sites, !site_date %in% all_met$site_date)


# Count site-days left to download for AQI
sites %>% filter(site_catid %in% aqi_sites$site_catid) %>% nrow() %>% print()

#rm(all_met)

# Loop through site table and send DarkSky request
all_forecasts <- tibble()

requests <- 0

max_requests <- 995 #998

for (i in 1:nrow(sites)) {
  
  Sys.sleep(0.5)
  
  site <- sites[i, ]
  
  if (requests > max_requests) break()
  
  print(site$site_date)
  
  requests <- requests + 1
  
  day_forc <- tryCatch(get_forecast_for(round(site$monitor_lat, 2), 
                                        round(site$monitor_long, 2),
                                        paste0(site$date, "T12:00:00-0400"), 
                                        exclude = "currently,daily"), 
                       error = function(err) NA)
  
  if (!is.na(day_forc)) {
    
    print("Download success!")
  
    day_forc            <- day_forc$hourly 
  
    day_forc$site_catid <- site$site_catid
  
    all_forecasts       <- bind_rows(day_forc, all_forecasts) %>%
                           select(site_catid, everything())
    
  } else {
    
    print("Download fail.")
    
    next()
    
  }
}


# Add new data to archive
if (nrow(all_forecasts) > 0) {
  
  all_forecasts$time <- as.character(all_forecasts$time)
  
  keep_names <- forecast_col_names %in% names(all_forecasts) 
  
  keep_types <- strsplit(forecast_col_types, "") %>% 
                unlist() %>% 
                .[keep_names] %>%
                paste0(collapse = "")
  
  keep_names <- forecast_col_names[keep_names]
  
  all_forecasts <- all_forecasts[ , keep_names] %>%
                   format_csv() %>%
                   read_csv(col_types = keep_types)
  
  # Check date coverage
  length(unique(all_forecasts$time))
  
  length(unique(as.Date(all_forecasts$time)))
  
  range(unique(as.Date(all_forecasts$time)))
  
  
  # Update download progress file
  new_met_dates <- all_forecasts %>%
                   rowwise() %>%
                   mutate(date = as.Date(time) %>% format("%Y-%m-%d")) %>%
                   select(site_catid, date) %>%
                   unique()
                   
  
  all_met <- all_met %>%
             mutate(date = as.character(date)) %>%
             bind_rows(new_met_dates) %>%             
             select(site_catid, date) %>%
             ungroup() %>% 
             unique()
             
  
  write_csv(all_met, paste0(folder, "/../progress_on_downloaded_met_site_data.csv"))

  # Update individual site files
  for (i in unique(all_forecasts$site_catid)) {
    
    print(i)
    
    file_loc <- paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/MET data/DarkSky database/sites/", i, ".csv")
    
    if (file.exists(file_loc)) {
      
      # Load previously downloaded data
      temp <- read_csv(file_loc) %>% filter(!is.na(temperature), !is.na(site_catid))
      
      if (nrow(temp) > 1) {
        
        # Check for missing columns
        column_chk <- forecast_col_names %in% names(temp)
        
        # Arrange columns
        temp <- temp[ , forecast_col_names[column_chk]]
        
        # Align column types
        temp$time <- as.character(temp$time)
        
        temp <- read_csv(format_csv(temp),
                         col_types = strsplit(forecast_col_types, "")[[1]][column_chk] %>% 
                                               paste(collapse = ""))
        
        # Join tables
        site_met <- bind_rows(temp, filter(all_forecasts, site_catid == i))
        
        site_met <- group_by(site_met, time, site_catid) %>% 
                    arrange(precipProbability, windGust, temperature) %>%
                    slice(1)
        
        write_csv(site_met, file_loc, na = "")
      
      }
      
    } else {
      
        site_met <- filter(all_forecasts, site_catid == i)
        
        site_met$precipIntensity <- as.numeric(site_met$precipIntensity)
        
        site_met$time <- as.character(site_met$time)
        
        write_csv(site_met, file_loc, na = "")
    
    }
    
  }
  
  
  # Save for WAIR database
  if (FALSE) {
    
    all_met2$date_time_gmt <- format(all_met2$time, tz = "UTC", usetz = T)
    
    all_met2$date_time_cst <- format(all_met2$time, tz = "America/Chicago", usetz = T)
    
    unique(difftime(all_met2$time[1:1000], all_met2$date_time_cst[1:1000]))
    
    unique(difftime(all_met2$time[1:1000], all_met2$date_time_gmt[1:1000]))
    
    
    all_met2 <- all_met2 %>% select(site_catid, date_time_gmt, date_time_cst, summary, icon, temperature, 
                                    dewPoint, humidity, pressure, visibility, 
                                    windBearing, windSpeed, cloudCover, 
                                    precipIntensity, precipAccumulation, precipType)
  
    names(all_met2) <- c("site_catid", "date_time_gmt", "date_time_cst", "summary", "icon", "temperature_f",       
                         "dew_point_f", "humidity", "pressure_milbar", "visibility_miles", "wind_bearing_deg", "wind_speed_mph",         
                         "cloud_cover", "precip_intensity_inph", "precip_type")
  
  # Time check
  if (FALSE) {
    a <- get_forecast_for(45.13768, -93.20762,
                          paste0("2009-12-30", "T12:00:00-0400"), 
                          exclude = "currently,daily")
    
  }
  
  
  #saveRDS(all_met2, "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/MET data/DarkSky database/AQI MET archive_wair.rdata")
  
  #check <- readRDS("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/MET data/DarkSky database/AQI MET archive_wair.rdata")
  
  }
}


# Count missing values
if (FALSE) {
met_sum <- group_by(all_met2, as.Date(time)) %>% 
           summarize(wind_ms        = mean(windSpeed, na.rm = T),
                     windspeed_miss = sum(is.na(windSpeed)),
                     windbear_miss  = sum(is.na(windBearing)),
                     cloud_miss     = sum(is.na(cloudCover)),
                     sum_miss       = sum(is.na(summary)),
                     precip_miss    = sum(is.na(precipIntensity)))
}


##
