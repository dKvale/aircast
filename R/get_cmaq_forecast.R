#! /usr/bin/env Rscript

library(rvest)
library(stringr)
library(readr)
library(dplyr)
library(tidyr)
library(methods)


get_cmaq_forecast <- function(lat, long, hour_gmt = NULL) {  

  url       <- paste0('http://airquality.weather.gov/probe_aq_data.php?latitude=', lat, "&longitude=", long, '&Submit=Get+Guidance')
  
  noaa_web  <- read_html(url)
  
  o3        <- html_nodes(noaa_web, 'table')
  
  o3        <- html_table(o3, fill = T)[[1]]
  
  o3        <- o3[-(1:4), c(1:3)]
  
  names(o3) <- o3[1, ]
  names(o3) <- c("Time", "ozone_1hr", "ozone_8hr")
  
  o3        <- o3[2:(grep("moke", o3[ ,1])[1]-1), ]
  
  o3$Date_local  <- as.POSIXct(as.character(o3$Time), "%a, %d %b  %I %p", tz = "America/Chicago")
  
  o3$Date_GMT    <- as.POSIXct(format(o3$Date_local, tz = "GMT", uzetz = T), tz = "GMT")
  
  if(!is.null(hour_gmt)) {
    
    o3_hr <- filter(o3, format(o3$Date_GMT, "%H") == (hour_gmt + 1))
   
    if(nrow(o3_hr) < 2) o3_hr <- bind_rows(o3[1, ], o3_hr)
    
    o3 <- o3_hr
  }
  
  o3$Date_local <- format(o3$Date_local, "%Y-%m-%d", tz = "America/Chicago")
  
  o3       <- filter(o3, Date_local %in% unique(Date_local)[1:2])
  
  # Label forecast day
  o3_max   <- o3 %>%
              mutate(forc_day = ifelse(Date_local == min(Date_local, na.rm = T), 
                                       "day0", 
                                       "day1"))
              
  # Select max 8-hr average
  o3_max <- o3_max %>%
            group_by(forc_day) %>%
            summarize(max_1hr_ozone = max(as.numeric(ozone_1hr), na.rm=T),
                      max_8hr_ozone = max(as.numeric(ozone_8hr), na.rm=T))
  
  if("-Inf" %in% c(o3_max$max_1hr_ozone, o3_max$max_8hr_ozone) | "Inf" %in% c(o3_max$max_1hr_ozone, o3_max$max_8hr_ozone)) stop()
  
  # 8-hr Ozone
  o3_8hr <- spread(o3_max[ , -2], forc_day, max_8hr_ozone)
  names(o3_8hr) <- c("cmaq_day0_max_ozone_8hr", "cmaq_day1_max_ozone_8hr")
  
  # 1-hr Ozone
  o3_1hr <- spread(o3_max[ , -3], forc_day, max_1hr_ozone)
  names(o3_1hr) <- c("cmaq_day0_max_ozone_1hr", "cmaq_day1_max_ozone_1hr")
  
  o3_max <- bind_cols(o3_8hr, o3_1hr)
  
  return(o3_max)
  
}
##