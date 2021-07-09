#! /usr/bin/env Rscript

#Java path
Sys.setenv(JAVA_HOME="C:/Program Files (x86)/Java/jre1.8.0_181")
#Sys.getenv()

library(R.utils)
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(mailR)


#"C:\Users\dkvale\Documents\R\R-3.5.2\bin\i386\Rscript.exe" --no-save --no-restore "X:\Agency_Files\Outcomes\Risk_Eval_Air_Mod\_Air_Risk_Evaluation\Staff folders\Dorian\AQI\aircast\R\email_verification.R"
#"C:\Program Files\R\R-4.1.0\bin\i386\Rscript.exe" --no-save --no-restore "X:\Agency_Files\Outcomes\Risk_Eval_Air_Mod\_Air_Risk_Evaluation\Staff folders\Dorian\AQI\aircast\R\email_verification.R"

aircast_path  <- "https://raw.githubusercontent.com/dKvale/aircast/master/"
aqiwatch_path <- "https://raw.githubusercontent.com/dKvale/aqi-watch/master/"
results_path  <- "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/"

# Run local copy
#aircast_path  <- "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff folders/Dorian/AQI/aircast/"

# AirNow credentials
creds <- read_csv("C:/Users/dkvale/Desktop/credents/credentials.csv")


# E-mail subscribers
aqi_team <- c(paste0(c("dorian.kvale",
                       "daniel.dix",
                       "david.l.brown",
                       "michael.smith",
                       "kari.palmer",
                       "monika.vadali",
                       #"helen.waquiu",
                       "nicholas.witcraft",
                       "matthew.taraldsen"), "@state.mn.us"), 
              "kvaled@gmail.com")
              #"sirwin.mobile@gmail.com") 

aqi_team <- aqi_team[1]
#aqi_team <- c("dorian.kvale@state.mn.us", "david.l.brown@state.mn.us", "nicholas.witcraft@state.mn.us")

# Set Pandoc location
#Sys.setenv(RSTUDIO_PANDOC = "C:/Users/dkvale/Documents/RStudio/bin/pandoc")
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
                 '<table style = "padding-right: 10px; padding-bottom: 2px;"', msg_body)


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
  
  if(F) {
  # Outlook
  smtp = list(host.name = "smtp-mail.outlook.com", 
              port      = 465,
              ssl       = TRUE, 
              user.name = "dorian.kvale@state.mn.us",
              passwd    = creds$mpca_pwd)
  
  send.mail(from         = "dorian.kvale@state.mn.us",
            to           = x,
            subject      = "AQI report",
            body         = msg_body,
            html         = TRUE,
            inline       = TRUE,
            smtp         = smtp,
            authenticate = TRUE,
            send         = TRUE)
  }

}
                  
#-- Send e-mail one person at a time
for(i in aqi_team) {

  #i <- aqi_team
  print(i)
  
  setwd("~")
  setwd("../Desktop/credents")

  run_count <- 0

  send_fail <- NA
  
  while( (is.null(send_fail) || is.na(send_fail)) & run_count < 1) {

    #-- Set time limit on run time
    send_fail <- tryCatch(withTimeout(send_msg(i), timeout = 8, onTimeout = "error"), 
                          TimeoutException = function(ex) NA, 
                          error = function(e) NA)
    
    run_count <- run_count + 1
    
    Sys.sleep(2)
    
    if(is.null(send_fail) || !is.na(send_fail)) print("Success!")

  }
}
 

# Update Github table

# Filter verification to last 7 days
verify <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Verification/verification_table2.csv")

verify <- filter(verify, forecast_date > (Sys.Date() - 8), forecast_date < Sys.Date())

# Filter to one forecast per day for each site
verify <- group_by(verify, forecast_date, site_catid) %>%
          mutate(forecast_day = ifelse(forecast_day == 0, 99, forecast_day)) %>%
          arrange(forecast_day) %>%
          slice(1) %>%
          mutate(forecast_day = ifelse(forecast_day == 99, 0, forecast_day))

# Round - set signif digits
verify <- verify %>% mutate_at(vars(matches("mod_")), round, 3)

# Save
write_csv(verify, "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff folders/Dorian/AQI/aircast/data/model_performance.csv")

# Create git commands
git_exe <- "\"C:/Program Files/Git/bin/git.exe\""
  
git <- paste0("C: & X: & CD \"X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/aircast/\" & ",
              git_exe, " ")


# Add account credentials
system(paste0(git, "config --global user.name dkvale"), show.output.on.console = T, invisible = F)

system(paste0(git, "config --global user.email ", creds$email), show.output.on.console = T, invisible = F)


add <- paste0(git, 'add', ' "data/model_performance.csv"')

cat(add)

shell(add)

commit <- paste0(git, 'commit -m ', '"update weekly performance"')

cat(commit)

system(commit)

push <- paste0(commit, " & git push origin master")

cat(push)

shell(push)
##
