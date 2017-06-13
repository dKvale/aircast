#! /usr/bin/env Rscript

library(R.utils)
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(gmailr)
library(mailR)

aqi_team <- paste0(c("dorian.kvale",
                     "steve.irwin",
                     "daniel.dix",
                     "monika.vadali",
                     "ruth.roberson",
                     "david.l.brown"), "@state.mn.us")

#aqi_team <- aqi_team[1]

#aqi_team <- paste0(aqi_team, collapse = ",")



#-- Load credentials
creds <- read.csv("C:\\Users\\dkvale\\Desktop\\credentials.csv", stringsAsFactors = F)


#-- Create AQI table
setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff folders/Dorian/AQI/Verification")

if(FALSE) {   
  verify_o3 <- datatable(verify_o3, rownames = FALSE, options = list(searching=F, paging=F, scrollX=T))
  
  verify_o3 <- formatStyle(verify_o3, 
                           c("Ozone AQI", "Ozone forecast", "Ozone cmaq forecast"),
                           fontWeight      = styleInterval(c(50), c("normal","bold")),
                           backgroundColor = styleInterval(c(-1,0,50,100,150,200), aqi_colors))
}


#use "output: html_fragment:"
  #when javascript not required
#"    theme: null",
#"    highlight: null",
#"    mathjax: null",

aqi_message <- c("---",
                 "title: ''",
                 "output:", 
                 "  html_fragment:",
                 "    keep_md: false",
                 "---",  
                 "\n",

"## Minnesota AQI report  ",
"<i>Preliminary results for `r format(Sys.Date() - 1, '%A %b %d, %Y')`</i>  <br>  ",
"  ",
"```{r echo=FALSE, message =FALSE, warnings =FALSE}",
'library(formattable)
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)


aqi_colors <- c("#FFF",     # White
"#9BF59B",  # Green
"#ffff00",  
"#ff7e00", 
"#ff0000",  
"#99004c")  

days_past <- 0

#-- Load yesterdays observations
setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Verification")

#saveRDS(all_verify, paste0("Archive/test_", Sys.Date() - days_past, "_verification_table.Rdata"))
#verify <- readRDS(paste0("Archive/test_", Sys.Date() - days_past, "_verification_table.Rdata"))

verify <- readRDS(paste0("Archive/", Sys.Date() - days_past, "_verification_table.Rdata"))

verify <- verify[!duplicated(verify$site_catid), ]

names(verify)[grep("site_catid", names(verify))] <- "aqs_id"



#-- Filter to yesterdays results
verify <- filter(verify, forecast_date == Sys.Date() - days_past - 1)

verify <- filter(verify, forecast_day == min(verify$forecast_day, na.rm = T))

verify <- select(verify, short_name, group,  aqs_id,
                 count_ozone_obs, obs_ozone_aqi,  fcst_ozone_aqi, mod_aqi_o3, cmaq_ozone_aqi,
                 count_pm25_obs, obs_pm25_aqi, fcst_pm25_aqi, mod_aqi_pm)


names(verify) <- c("Site", "Group", "AQS ID", 
                   "Ozone obs count", "Ozone AQI", "Ozone forecast", "Ozone model output", "CMAQ forecast",
                   "PM2.5 obs count", "PM2.5 AQI", "PM2.5 forecast", "PM2.5 model output")

# Remove underscores from names
verify$Site <- gsub("_", " ", verify$Site)

verify$Group <- gsub("_", " ", verify$Group)

# Ozone table
verify_o3 <- select(verify, -c(`AQS ID`, `PM2.5 obs count`, `PM2.5 AQI`, `PM2.5 forecast`, `PM2.5 model output`)) %>% 
             filter(!is.na(`Ozone forecast`)) %>%
             arrange(-`Ozone AQI`)

names(verify_o3)[3:7] <- c(#"AQS ID",
                           "Obs count", 
                           "Obs AQI", 
                           "MPCA forecast",
                           "Model output",
                           "CMAQ forecast")

# Drop model output value unless different than MPCA forecast
verify_o3$`Model output` <- ifelse(verify_o3$`MPCA forecast` == verify_o3$`Model output`, " ", verify_o3$`Model output`)

# Add paddding to columns
verify_o3$Site <- paste0(verify_o3$Site, "    ")

#verify_o3$`Obs count` <- paste0(verify_o3$`Obs count`, "             ")

# Web format
margin_left <- function(i) {
  
  formatter("span",
            style = x ~ style("padding-left"  = paste0(i, "px")))
}

margin_right <- function(i) {
  
  formatter("span",
            style = x ~ style("padding-right"  = paste0(i, "px")))
}

col_format <- function(i) {
  
#"margin-left"   = paste0(i, "px"),
#"margin-right"  = paste0(i, "px"),

formatter("span", 
            style = x ~ style("padding-left"  = "36px",
                              "padding-right" = "36px",
                              "margin-right"  = paste0(i, "px"),
                              "font-weight"   = 600,
                              "background-color" = 
                                ifelse(x == " " | is.na(x), "white",
                                ifelse(as.numeric(x) <= 50,  aqi_colors[2], 
                                ifelse(as.numeric(x) <= 100, aqi_colors[3],
                                ifelse(as.numeric(x) <= 150, aqi_colors[4], 
                                ifelse(as.numeric(x) <= 200, aqi_colors[5], "white")))))))
}


verify_o3 <- formattable(verify_o3,
                         list(Site              = margin_right(20),
                              Group             = margin_right(20),
                              #`AQS ID`          = margin_right(20),
                              `Obs count`       = margin_right(65),
                              `Obs AQI`         = col_format(15),
                              `MPCA forecast`   = col_format(15),
                              `Model output`    = col_format(15),
                              `CMAQ forecast`   = col_format(15)),
                          align = c("l","l","l","l","l","l","l","l"))


saveRDS(verify_o3, "o3_table.rdata")


# PM table
verify_pm <- select(verify, -c(`Ozone obs count`, `Ozone AQI`, `Ozone forecast`)) %>%  #`Ozone cmaq forecast`
             filter(!is.na(`PM2.5 AQI`)) %>% 
             arrange(-`PM2.5 AQI`)


names(verify_pm)[3:6] <- c("AQS ID       ", "Obs count", "Obs AQI   ", "Forecast AQI")

# Web format
verify_pm <- formattable(verify_pm, 
                         list(#Site               = margin_left(0),
                              Group               = margin_left(20),
                              #`AQS ID       `     = margin_left(20),
                              `Obs count`         = margin_left(65),
                              `Obs AQI   `        = col_format(50),
                              `Forecast AQI`      = col_format(85)))

if(FALSE) {
  verify_pm <- datatable(verify_pm, rownames = FALSE, options = list(searching=F, paging=F, scrollX=T))
  
  verify_pm <- formatStyle(verify_pm, 
                           c("PM2.5 AQI", "PM2.5 forecast"),
                           fontWeight      = styleInterval(c(50), c("normal","bold")),
                           backgroundColor = styleInterval(c(-1,0,50,100,150,200), aqi_colors))
}

saveRDS(verify_pm, "pm_table.rdata")
#devtools::install_github("renkun-ken/formattable")',
"",
"```",
"  ",
"### Ozone results  ",
#paste0(kable(verify_o3), collapse="\n"),
"```{r, echo=F, warning=F, message=F}",
#"setwd(\"X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Verification\")",
"o3_df <- readRDS('o3_table.rdata')",
"o3_df",
"```",
"  "

#"### PM2.5 results  ",
#paste0(kable(verify_pm), collapse="\n"))
#paste0(verify_pm, collapse="\n"))
#"`r pm_df <- readRDS('pm_table.rdata'); pm_df`  ",
#"  "
)

#-- Knit Rmarkdown document
setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff folders/Dorian/AQI/Verification")
writeLines(aqi_message, "aqi_message.Rmd")

rmarkdown::render("aqi_message.Rmd")


#-- Create e-mail message
msg_body <- readLines("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Verification/aqi_message.html")

#msg_body <- msg_body[(grep("<body>", msg_body) + 1):(grep("</body>", msg_body) - 1)]

msg_body <- paste0(msg_body, collapse = "")

msg_body <- paste0('<style type="text/css"> th, td {border-bottom: 1px solid #ddd;} </style>', msg_body)

msg_body <- gsub("<table", 
                 '<table 
                 style = "padding-right: 10px; padding-bottom: 2px;', msg_body)

#-- Send message function
send_msg <- function(x) {
  
  if(FALSE) {
    #html_msg <- mime(to       = x,
    #                 from     = "mpca.aqi@gmail.com",
    #                 subject  = "AQI report")
  
    #html_msg <- html_body(html_msg, msg_body)
  
    #send_message(html_msg)
  }
  
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
  
  while(is.na(send_fail) & run_count < 12) {
    
    #detach("package:gmailr", unload = TRUE)
    #library(gmailr)
    #gmail_auth(secret = 'gmail-credentials.json', scope = 'compose') 
    #use_secret_file("gmail-credentials.json")
    
    #-- Set time limit on run time
    send_fail <- tryCatch(evalWithTimeout(send_msg(i), timeout = 5, onTimeout = "error"), TimeoutException = function(ex) NA, error = function(e) NA)
    
    Sys.sleep(1)
    
    run_count <- run_count + 1

  }
}
       


##
