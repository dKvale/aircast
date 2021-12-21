#! /usr/bin/env Rscript


#Java path
Sys.setenv(JAVA_HOME="C:/Program Files (x86)/Java/jre1.8.0_181")

library(R.utils)
library(dplyr)
library(readr)
library(rJava)
library(mailR)  #mailR reference - https://github.com/rpremraj/mailR

#"C:\Users\dkvale\Documents\R\R-3.5.2\bin\i386\Rscript.exe" --no-save --no-restore "X:\Agency_Files\Outcomes\Risk_Eval_Air_Mod\_Air_Risk_Evaluation\Staff folders\Dorian\AQI\aircast\R\email_background.R"
#"C:\Program Files\R\R-4.1.0\bin\i386\Rscript.exe" --no-save --no-restore "X:\Agency_Files\Outcomes\Risk_Eval_Air_Mod\_Air_Risk_Evaluation\Staff folders\Dorian\AQI\aircast\R\email_background.R"

aircast_path  <- "https://raw.githubusercontent.com/dKvale/aircast/master/"
aqiwatch_path <- "https://raw.githubusercontent.com/dKvale/aqi-watch/master/"

if (T) {
  
  results_path  <- "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Current forecast/"
  results_path  <- "~/../Desktop/aircast/forecast/background/"

  hysplit_path  <- "~/../Desktop/aircast/hysplit/"
  gmail_path    <- "~/../Desktop/credents"
  
  # AirNow credentials
  creds <- read_csv("C:/Users/dkvale/Desktop/credents/credentials.csv")
  
}


aqi_team <- c("aqi.pca@state.mn.us", "dorian.kvale@state.mn.us")

#aqi_team <- "dorian.kvale@state.mn.us"


#-- Load today's AQI background file
setwd(results_path)

todays_path <- paste0(results_path, Sys.Date(), "_17z_AQI_background.csv")

#-- Create e-mail message
msg_body <- readLines(todays_path)

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
            html         = TRUE,
            attach.files = todays_path,
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
  setwd(gmail_path)

  run_count <- 0

  send_fail <- NA
  
  while (is.na(send_fail) & run_count < 2) {
    
    #-- Set time limit on run time
    send_fail <- tryCatch(withTimeout(send_msg(i), timeout = 9, onTimeout = "error"), 
                          TimeoutException = function(ex) NA, error = function(e) NA)
    
    Sys.sleep(1)
    
    run_count <- run_count + 1
    
    if (is.na(send_fail)) {
      
       print("Failed.")
    }
  }
}


# Delete NAMS data
setwd("~")
setwd(hysplit_path)

# Delete old data
unlink(list.files()[grepl("traj-", list.files())], recursive = T)
unlink("__today", recursive = T)


##
