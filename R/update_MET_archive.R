#devtools::install_github("hrbrmstr/darksky")
#devtools::install_github("tidyverse/purrr")
library(purrr)
library(dplyr)
library(readr)
library(darksky)

d_key <- 'ea8610622c9d63c30ca25dea03ec3d90'
#darksky_api_key(key = d_key, force = T)

Sys.setenv(DARKSKY_API_KEY = d_key)

days <- data_frame(date = seq(as.Date("2009-12-31"), as.Date("2017-01-01"), 1),
                   join = 1)

days[1:5, ]

day <- 5

days[day, ]


# Load previous archives
all_met <- readRDS("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/MET data/DarkSky database/AQI MET archive.rdata")

names(all_met)

all_met$date      <- format(all_met$time, "%Y-%m-%d")

all_met$site_date <- paste(all_met$site_catid, all_met$date)


# Site list
sites <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff folders/Dorian/AQI/MET data/Monitors and Rep Wx Stations.csv")

# Remove duplicate sites
#sites <- filter(sites, !short_name %in% c("Fond_Du_Lac2"))

sites$join <- 1

sites <- left_join(sites, days)

sites$join <- NULL

sites$site_date <- paste(sites$site_catid, sites$date)

sites$run_order <- ifelse(sites$site_catid == "27-003-1002", 1, ifelse(sites$site_catid == "27-109-5008", 2, 3))

sites <- arrange(sites, run_order, site_catid)

# Loop through site table and send DarkSky request
all_forecasts <- data_frame()

for(i in 1:nrow(sites)) {
  
  site <- sites[i, ]
  
  if(site$site_date %in% all_met$site_date) next()
  
  print(site$site_date)
  
  day_forc <- tryCatch(get_forecast_for(site$monitor_lat, 
                                        site$monitor_long,
                                        paste0(site$date, "T12:00:00-0400"), 
                                        exclude = "currently"), 
                       error = function(err) NA)
  
  if(!is.na(day_forc)) {
  
    day_forc            <- day_forc$hourly 
  
    day_forc$site_catid <- site$site_catid
  
    all_forecasts       <- bind_rows(day_forc, all_forecasts)
    
  } else {
    
    next()
    
  }
  
}

all_met$date      <- NULL

all_met$site_date <- NULL

# Add new data to archive
if(nrow(all_forecasts) > 0) {
  
  all_met2 <- bind_rows(all_met, all_forecasts)
  
  # Check date coverage
  length(unique(all_met2$time))
  
  length(unique(as.Date(all_met2$time)))
  
  range(unique(as.Date(all_met2$time)))
  
  # Save to R file
  saveRDS(all_met2, "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/MET data/DarkSky database/AQI MET archive.rdata")

}


# Count missing values
if(FALSE) {
met_sum <- group_by(all_met2, as.Date(time)) %>% 
           summarize(wind_ms        = mean(windSpeed, na.rm = T),
                     windspeed_miss = sum(is.na(windSpeed)),
                     windbear_miss  = sum(is.na(windBearing)),
                     cloud_miss     = sum(is.na(cloudCover)),
                     sum_miss       = sum(is.na(summary)),
                     precip_miss    = sum(is.na(precipIntensity)))

}

if(FALSE) {
#----------------#
# Using purr
#----------------#
more_than_one <- data.frame(loc  = c("Maine", "Seattle"),
                            lon  = c(43.2672, 47.6097),
                            lat  = c(70.8617, 122.3331),
                            when = c("2013-05-06T12:00:00-0400",
                                     "2013-05-06T12:00:00-0400"),
                            stringsAsFactors = FALSE)

bigger_list <- pmap(list(more_than_one$lon, more_than_one$lat,
                         more_than_one$when),
                    get_forecast_for)

names(bigger_list) <- more_than_one$loc

bigger_list$Seattle

}

met_chk <- readRDS("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/MET data/DarkSky database/AQI MET archive.rdata")

unique(met_chk$site_catid)

##