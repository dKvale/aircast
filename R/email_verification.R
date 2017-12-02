#! /usr/bin/env Rscript

library(R.utils)
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(mailR)

# E-mail subscribers
aqi_team <- paste0(c("dorian.kvale",
                     "steve.irwin",
                     "daniel.dix",
                     "monika.vadali",
                     "ruth.roberson",
                     "david.l.brown",
                     "rick.strassman"), "@state.mn.us")

#aqi_team <- aqi_team[1]

#aqi_team <- "frank.kohlasch@state.mn.us"

#aqi_team <- paste0(aqi_team, collapse = ",")


#-- Load e-mail credentials
creds <- read.csv("C:\\Users\\dkvale\\Desktop\\credentials.csv", stringsAsFactors = F)


#-- Knit Rmarkdown document
setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff folders/Dorian/AQI/Verification")

rmarkdown::render("aqi_message.Rmd")


#-- Create e-mail message
msg_body <- readLines("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Verification/aqi_message.html")

#-- Collapse to single line
msg_body <- paste0(msg_body, collapse = "")

msg_body <- paste0('<style type="text/css"> th, td {border-bottom: 1px solid #ddd;} </style>', msg_body)

msg_body <- gsub("<table", 
                 '<table style = "padding-right: 10px; padding-bottom: 2px;', msg_body)


#-- Send message function
send_msg <- function(x) {
  
  smtp = list(host.name = "smtp.gmail.com", 
              port      = 465,
              ssl       = TRUE, 
              user.name = "mpca.aqi@gmail.com",
              passwd    = creds$mpca_aqi_pwd)
  
  send.mail(from         = "mpca.aqi@gmail.com",
            to           = x,
            subject      = "AQI report",
            body         = msg_body,
            html         = TRUE,
            inline       = TRUE,
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
    send_fail <- tryCatch(evalWithTimeout(send_msg(i), timeout = 7, onTimeout = "error"), 
                          TimeoutException = function(ex) NA, 
                          error = function(e) NA)
    
    run_count <- run_count + 1
    
    Sys.sleep(1)

  }
}
       

##
