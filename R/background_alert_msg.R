library(gmailr)
#library(knitr)


#-- Load e-mail credentials
#creds  <- read_csv("C:/Users/dkvale/Desktop/credentials.csv")

bg_alert_subsribers <- c("dorian.kvale@state.mn.us",
                         "steve.irwin@state.mn.us",
                         "daniel.dix@state.mn.us")

bg_alert_subsribers <- bg_alert_subsribers[2]

#-- Open NAM data folder on desktop
setwd("~")
setwd("../Desktop/hysplit")

# Create minimum exists function
# Checks if file exists and if meets minimum file size
min_exists <- function(file_name, min_size = 1E+8) { 
  
  file.exists(file_name) & file.size(file_name) > min_size
  
}

days_past <- 0

met_list  <- c("__today/hysplit.t12z.namsf", "__today/hysplit.t12z.namsa", 
               "__today/hysplit.t06z.namsf", "__today/hysplit.t06z.namsa",
               "__today/hysplit.t12z.namf", "__today/hysplit.t12z.nama", 
               "__today/hysplit.t06z.namf", "__today/hysplit.t06z.nama")

# Drop missing met data
met_list      <- met_list[min_exists(met_list)]


#-- Check if the 4 required files exist: c("namf", "nama", "namsf", "namsa")
back_missing  <- max(grepl("namf", met_list), na.rm = T) +
                 max(grepl("nama", met_list), na.rm = T) +
                 max(grepl("namsf", met_list), na.rm = T) +
                 max(grepl("namsa", met_list), na.rm = T)   < 4


if(back_missing) {
  
  #-- Create email text
  date <- format(Sys.Date(), "%Y%m%d")
  
  message_txt <- c("---",
                   "title: ''",
                   "output:", 
                   "  html_fragment:",
                   "    keep_md: false",
                   "---",  
                   "\n",
  
  "### NAMs Background data missing for `r format(Sys.Date(), '%A %b %d, %Y')` \n\n\n",
  "Download these files to the folder: `Documents/background script/hysplit/__today`  \n",
  paste0("1. ftp://arlftp.arlhq.noaa.gov/forecast/", date, "/hysplit.t12z.", "namf  "),
  paste0("1. ftp://arlftp.arlhq.noaa.gov/forecast/", date, "/hysplit.t12z.", "nama  "),
  paste0("1. ftp://arlftp.arlhq.noaa.gov/forecast/", date, "/hysplit.t12z.", "namsf  "),
  paste0("1. ftp://arlftp.arlhq.noaa.gov/forecast/", date, "/hysplit.t12z.", "namsa  "),
  "  \n ",
  "  \n",
  "  \n ",
  "<br>  \n",
  "  ",
  "### Run the HYSPLIT model for today:   \n",
  "1. Open the folder: `Documents/Background script`",
  "1. Open the file: <i> 2 - run_hysplit.R </i> (it should open in R-Studio)",
  "1. Click the <i> [-> Source] </i> button near the top middle of the screen   \n",
  "  \n",
  "  ",
  "### Attach the air monitoring observations:   \n",
  "1. Open the folder: `Documents/background script` ",
  "1. Open the file: <i> download_background_aqi.R </i>",
  "1. Click the <i> [-> Source] </i> button near the top middle of the screen   \n"
  )
  
  #-- Write html file
  writeLines(message_txt, "alert_message.Rmd")
  
  rmarkdown::render("alert_message.Rmd")
  
  
  #-- Create e-mail message
  html_msg <- mime() %>%
              to(bg_alert_subsribers) %>%
              from("mpca.aqi@gmail.com") %>%
              subject("AQI: NAM data download for HYSPLIT background") %>%
              html_body(paste0(readLines("alert_message.html"), collapse = "\n"))
                    
  #-- Send e-mail
  send_message(html_msg)
  
}

##
