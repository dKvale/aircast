
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(RCurl)


# Get yesterday's actuals  #
#--------------------------#
sites <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff folders/Dorian/AQI/MET data/Monitors and Rep Wx Stations.csv")

names(sites) <- gsub(" ", "_", tolower(names(sites)))

# Switch Voyageurs AQS ID to alt
sites[sites$site_catid == "27-137-9000", "site_catid"] <- sites[sites$site_catid == "27-137-9000", "alt_siteid"]

# Drop dashes in IDs to match AQS
sites$aqsid <- gsub("-", "", sites$site_catid)
#sites$aqsid <- gsub("-", "", sites$site_catid)


# Filter to one site per forecast city
sites <- filter(sites, !site_catid %in% c('27-017-7416'))

# Get date
yesterday <- gsub("-", "", Sys.Date() - 1)

year      <- format(Sys.Date() - 1, "%Y")


# Connect to AirNow data site
#https://files.airnowtech.org/

airnow_link <- paste0("https://s3-us-west-1.amazonaws.com//files.airnowtech.org/airnow/",
                      year, "/",
                      yesterday,
                      "/daily_data.dat")
  
aqi   <- try(read_delim(airnow_link, "|", 
                        col_names = F, 
                        col_types = c('cccccdic')), 
             silent = T)
  
closeAllConnections()


# Clean
names(aqi) <- c("date", "aqsid", "City", "Parameter", "Units", "Concentration", "Hours", "Agency")

# Filter sites
aqi <- filter(aqi[ -c(5,7:8)], 
              aqsid %in% c(sites$aqsid, gsub("-", "", sites$alt_siteid)),
              Parameter %in% c("OZONE-8HR", "PM2.5-24hr"))


# Flip to wide format
if(nrow(aqi) < 1) {
  
  aqi[1:2, ] <- NA
  
  aqi$Parameter <- c("OZONE-8HR", "PM2.5-24hr")
 
}

aqi <- spread(aqi, Parameter, Concentration)
  
names(aqi)[c(4:5)] <-  c("max_ozone_8hr", "pm25_24hr")
  

# QC MPCA sites using AirVision data

# Get date
yesterday <- gsub("-", "", Sys.Date() - 1)

today     <- gsub("-", "", Sys.Date())

year      <- format(Sys.Date() - 1, "%Y")


# Connect to aqi-watch FTP site
airvis_link <- paste0("ftp://airvis:mpca@52.27.98.92/airvision/")

# Find last file of the day
file_list   <- getURL(airvis_link, verbose = T, dirlistonly = T) %>%
               strsplit("\r\n") %>% 
               .[[1]]

final_hour  <- file_list[grepl(today, file_list)][1]

# Download monitoring data
airvis_df   <- try(read_csv(paste0(airvis_link, final_hour),
                              col_names = F, 
                              col_types = c('cccccccdcccccccccccc')), 
                   silent = T)

airvis_df   <- airvis_df[ , c(1,4,5,8,10)]
  
names(airvis_df) <- c("aqsid", "date", "Parameter", "Concentration", "qc_flag")

closeAllConnections()


# Drop flagged concentrations
airvis_df$Concentration <- ifelse(airvis_df$qc_flag > 5, NA, airvis_df$Concentration)


# Filter to PM2.5 and ozone
airvis_df$aqsid <- gsub("840", "", airvis_df$aqsid)

airvis_ozone    <- filter(airvis_df, 
                          Parameter == 44201, 
                          aqsid %in% c(sites$aqsid, gsub("-", "", sites$alt_siteid)))

airvis_pm       <- filter(airvis_df, 
                          Parameter %in% c(88101), # 88502), 
                          aqsid %in% c(sites$aqsid, gsub("-", "", sites$alt_siteid)))


# Ozone summary
airvis_ozone <- group_by(airvis_ozone, aqsid, Parameter) %>% 
                arrange(date) %>%
                mutate(row_id = 1:n())

# Calculate 8-hr ozone values
for(i in 1:nrow(airvis_ozone)) {

  aqs  <- airvis_ozone[i, ]$aqsid
  
  rows <- airvis_ozone[i, ]$row_id
  
  airvis_ozone[i , "ozone_8hr"] <- mean(filter(airvis_ozone, aqsid == aqs, row_id %in% (max(1, rows - 7):rows))$Concentration, na.rm = T)

}    

# Drop 8-hr averages from first 4 hours of day
airvis_ozone[airvis_ozone$row_id %in% 1:4, ]$ozone_8hr <- NA
  

airvis_ozone <- group_by(airvis_ozone, aqsid) %>%                
                summarize(max_ozone_8hr_vis = round(max(ozone_8hr, na.rm = T), 2),
                          n_ozone_obs       = sum(!is.na(Concentration)))


# PM 2.5 summary
airvis_pm    <- group_by(airvis_pm, aqsid) %>%                
                summarize(pm25_24hr_vis = round(mean(Concentration, na.rm = T), 2),
                          n_pm25_obs    = sum(!is.na(Concentration)))



# Join AirNow with AirVis
aqi$City <- NULL

air_all <- full_join(aqi, airvis_ozone)
air_all <- full_join(air_all, airvis_pm)


# Join Sites and AQS-IDs
air_all <- left_join(air_all, select(sites, c(air_monitor, site_catid, aqsid)))

# Drop blank sites
air_all <- filter(air_all, !is.na(aqsid))

# Missing sites
miss_sites <- filter(sites, 
                     !aqsid %in% aqi$aqsid & 
                     !gsub("-", "", alt_siteid) %in% aqi$aqsid)


# Use AirNow value if AirVis is missing
air_all <- group_by(air_all, aqsid) %>% 
           mutate(a_max_ozone_8hr_ppb = ifelse(is.na(max_ozone_8hr_vis), round(max_ozone_8hr, 1), round(max_ozone_8hr_vis, 1)))

air_all <- group_by(air_all, aqsid) %>% 
           mutate(a_pm25_24hr_ugm3 = ifelse(is.na(pm25_24hr_vis), round(pm25_24hr, 1), round(pm25_24hr_vis, 1)))


# Drop daily value if less than 14 observations
air_all <- mutate(air_all, 
                  a_max_ozone_8hr_ppb = ifelse(n_ozone_obs < 14 & !is.na(n_ozone_obs), NA, a_max_ozone_8hr_ppb),
                  a_pm25_24hr_ugm3    = ifelse(n_pm25_obs < 14 & !is.na(n_pm25_obs ), NA, a_pm25_24hr_ugm3))


# Drop extra columns
air_all <- select(air_all, -max_ozone_8hr, -pm25_24hr, -max_ozone_8hr_vis, -pm25_24hr_vis)


# Set date
air_all$date <- Sys.Date() - 1 

# Check missing sites
miss_sites <- filter(sites, 
                     !aqsid %in% air_all$aqsid & 
                     !gsub("-", "", alt_siteid) %in% air_all$aqsid)


# Names
names(air_all)[c(3:4,7:8)] <- c("count_ozone_obs","count_pm25_obs","obs_max_ozone_8hr_ppb", "obs_pm25_24hr_ugm3") 


# Save
setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Current forecast")

keep_columns <- c("date", 
                  "site_catid", 
                  "air_monitor", 
                  "aqsid", 
                  "count_ozone_obs",
                  "count_pm25_obs",
                  "obs_max_ozone_8hr_ppb", 
                  "obs_pm25_24hr_ugm3")


write.csv(air_all[ , keep_columns], paste0(Sys.Date() - 1, "_AQI_observed", ".csv"), row.names = F)


#-- Clear outdated FTP files
#-------------------------------#

# Read .sh file
sh <- readLines("X:\\Agency_Files\\Outcomes\\Risk_Eval_Air_Mod\\_Air_Risk_Evaluation\\Staff Folders\\Dorian\\AQI\\aircast\\_sh scripts\\clear_ftp.sh")

# Update file to search for yesterday's date
new_line <- grep("grep", sh)

sh[new_line] <- paste0("for i in `curl -s -l ftp://\"$ftp_username\":\"$ftp_password\"@$ftp_ip/$ftp_path/ | grep ", yesterday, "`; do")

writeLines(sh, "X:\\Agency_Files\\Outcomes\\Risk_Eval_Air_Mod\\_Air_Risk_Evaluation\\Staff Folders\\Dorian\\AQI\\aircast\\_sh scripts\\clear_ftp.sh")

# Run .sh file in command line to delete files from yesterday
shell(paste0('C: & C:\\Users\\dkvale\\AppData\\Local\\Programs\\Git\\bin\\sh ',
              '"X:\\Agency_Files\\Outcomes\\Risk_Eval_Air_Mod\\_Air_Risk_Evaluation\\Staff Folders\\Dorian\\AQI\\aircast\\_sh scripts\\clear_ftp.sh"'))


##
