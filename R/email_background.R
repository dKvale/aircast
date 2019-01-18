#! /usr/bin/env Rscript

library(R.utils)
library(dplyr)
library(readr)
library(mailR)  # mailR reference - https://github.com/rpremraj/mailR


aircast_path  <- "https://raw.githubusercontent.com/dKvale/aircast/master/"
aqiwatch_path <- "https://raw.githubusercontent.com/dKvale/aqi-watch/master/"
results_path  <- "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/"


#Java path
Sys.setenv(JAVA_HOME="C:/Program Files (x86)/Java/jre1.8.0_181")


aqi_team <- c("aqi.pca@state.mn.us", "dorian.kvale@state.mn.us", "sirwin.mobile@gmail.com")

#aqi_team <- "dorian.kvale@state.mn.us"


#-- Load today's AQI background file
setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff folders/Dorian/AQI/Current forecast")


#-- Create e-mail message
msg_body <- readLines(paste0(Sys.Date(), "_17z_AQI_background.csv"))

msg_body <- paste0(msg_body, collapse = "\n")



#-- Send message function
send_msg <- function(x) {
  

  smtp = list(host.name = "smtp.gmail.com", 
              port      = 465,
              ssl       = TRUE, 
              user.name = "mpca.aqi@gmail.com",
              passwd    = creds$mpca_aqi_pwd)
  
  send.mail(from         = "mpca.aqi@gmail.com",
            to           = x,
            subject      = paste("Background for", Sys.Date()),
            body         = msg_body,
            html         = FALSE,
            attach.files = paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff folders/Dorian/AQI/Current forecast/", Sys.Date(), "_17z_AQI_background.csv"),
            #file.name   = ,
            #file.descriptions = c("Description for download log"),
            smtp         = smtp,
            authenticate = TRUE,
            send         = TRUE)
  
  
}
    
              
#-- Send e-mail one person at a time
for(i in aqi_team) {

  #i <- aqi_team
  print(i)
  
  setwd("~")
  setwd("../Desktop")

  run_count <- 0

  send_fail <- NA
  
  while(is.na(send_fail) & run_count < 1) {
    
    #-- Set time limit on run time
    send_fail <- tryCatch(withTimeout(send_msg(i), timeout = 5, onTimeout = "error"), TimeoutException = function(ex) NA, error = function(e) NA)
    
    Sys.sleep(1)
    
    run_count <- run_count + 1

  }

  }
       


##
