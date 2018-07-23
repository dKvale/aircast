#! /usr/bin/env Rscript

library(R.utils)
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(mailR)

# E-mail subscribers
aqi_team <- c(paste0(c("dorian.kvale",
                     "steve.irwin",
                     "daniel.dix",
                     "monika.vadali",
                     "helen.waquiu",
                     "david.l.brown",
                     "luke.charpentier"), "@state.mn.us"), "kvaled@gmail.com") 

#aqi_team <- "frank.kohlasch@state.mn.us"
#aqi_team <- aqi_team[1]


# Set Pandoc location
Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/RStudio/bin/pandoc")


#-- Knit Rmarkdown document
md_file <- readLines(paste0(aircast_path, "R/aqi_message.Rmd"))

writeLines(md_file, paste0(results_path, "Verification/daily_results.Rmd"))
  
rmarkdown::render(input = paste0(results_path, "Verification/daily_results.Rmd"))


#-- Create e-mail message
msg_body <- readLines(paste0(results_path, "Verification/daily_results.html"))

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
    send_fail <- tryCatch(withTimeout(send_msg(i), timeout = 8, onTimeout = "error"), 
                          TimeoutException = function(ex) NA, 
                          error = function(e) NA)
    
    run_count <- run_count + 1
    
    Sys.sleep(2)

  }
}
 
#write.csv(data.frame(x = 5), "email_sent.csv")      

##
