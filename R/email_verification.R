library(gmailr)
library(dplyr)
library(readr)
library(tidyr)
#library(knitr)
library(lubridate)
library(DT)

aqi_subsribers <- c("dorian.kvale@state.mn.us")

aqi_colors <- c("#FFF",     # White
                "#9BF59B",  
                "#ffff00",  
                "#ff7e00", 
                "#ff0000",  
                "#99004c")  

days_past <- 0

#-- Load e-mail credentials
#creds  <- read_csv("C:/Users/dkvale/Desktop/credentials.csv")

#-- Load yesterday's observations
setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Verification")

verify <- readRDS(paste0("Archive/", Sys.Date() - days_past, "_verification_table.Rdata"))


names(verify)[grep("site_catid", names(verify))] <- "aqs_id"

#-- Filter to yesterday's results
verify <- filter(verify, day_forecast_made == Sys.Date() - 1)

verify <- select(verify, air_monitor, aqs_id, group,
                 obs_ozone_aqi, count_ozone_obs, fcst_ozone_aqi, 
                 cmaq_ozone_aqi,
                 obs_pm25_aqi, count_pm25_obs, fcst_pm25_aqi)


names(verify) <- c("Site", "AQS ID", "Region", "Ozone AQI", "Ozone obs count",
                   "Ozone forecast", "Ozone cmaq forecast",
                   "PM2.5 AQI", "PM2.5 obs count", "PM2.5 forecast")

# Ozone table
verify_o3 <- filter(verify[ , c("Region", "Site", "AQS ID", "Ozone AQI", 
                         "Ozone forecast", "Ozone cmaq forecast", "Ozone obs count")], 
                    !is.na(`Ozone AQI`))


# Web format



# PM table
verify_pm <- filter(verify[ , c("Region", "Site", "AQS ID", "PM2.5 AQI", 
                                "PM2.5 forecast", "PM2.5 obs count")], 
                    !is.na(`PM2.5 AQI`))

# Web format
verify_pm <- datatable(verify_pm, rownames = FALSE, options = list(searching=F, paging=F, scrollX=T))

verify_pm <- formatStyle(verify_pm, 
                         c("PM2.5 AQI", "PM2.5 forecast"),
                         fontWeight = styleInterval(c(50), c("normal","bold")),
                         backgroundColor =  styleInterval(c(0,50,100,150,200), aqi_colors))

saveRDS(verify_pm, "pm_table.rdata")

#-- Create AQI table
setwd("../Verification/")

aqi_message <- c("---",
                 "title: ''",
                 "output:", 
                 "  html_fragment:",
                 "    keep_md: false",
                 "---",  
                 "\n",

"## AQI Report for `r format(Sys.Date() - 1, '%A %b %d, %Y')` \n",
"<i>Preliminary results</i> \n\n", 
"### Ozone",
#paste0(kable(verify_o3), collapse="\n"),
#paste0(verify_o3, collapse="\n"),


"### PM2.5",
#paste0(kable(verify_pm), collapse="\n"))
#paste0(verify_pm, collapse="\n"))
"`r pm_df <- readRDS('pm_table.rdata'); pm_df`"
)

writeLines(aqi_message, "aqi_message.Rmd")
  
rmarkdown::render("aqi_message.Rmd")

#knit2html("aqi_message.Rmd", force_v1 = T)


#-- Create e-mail message
html_msg <- mime() %>%
            to(aqi_subsribers) %>%
            from("mpca.aqi@gmail.com") %>%
            subject("AQI report") %>%
            html_body(paste0(readLines("aqi_message.html"), collapse = "\n"))
                  

#-- Send e-mail
send_message(html_msg)


##
