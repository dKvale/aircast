
get_airnow <- function(dates      = c(Sys.Date(), Sys.Date()-1),
                       sites      = "270530954",
                       pollutants = "OZONE-8HR") {
  
  library(dplyr)
  
  # Convert date to text
  txt_dates  <- as.character(dates) %>% gsub("-", "", .)
  
  
  # Get relevant dates
  years   <- format(as.Date(dates), "%Y")


  # Connect to AirNow data site
  #https://files.airnowtech.org/

  all_results <- tibble()
  
  for (i in seq_along(dates)) {
  
    print(dates[i])
    
    airnow_link <- paste0("https://s3-us-west-1.amazonaws.com//files.airnowtech.org/airnow/",
                          years[i], "/",
                          txt_dates[i],
                          "/daily_data.dat")
    
    aqi   <- try(readr::read_delim(airnow_link, "|", 
                                   col_names = F, 
                                   col_types = c('cccccdic')), 
                 silent = T)
  
    closeAllConnections()
  
    
    # Create blank table it empty
    if (class(aqi) == "try-error") aqi <- tibble("date" = as.character(NA),
                                                 "aqsid" = as.character(NA),3,4,5,6,7,8)[0, ]
  
    # Clean
    names(aqi) <- c("date", "aqsid", "City", "Parameter", "Units", "Concentration", "Hours", "Agency")
  
    
    # Filter sites
    aqi <- filter(aqi, aqsid %in% site_list, Parameter %in% pollutants)
    
    # Join all
    all_results <- bind_rows(aqi, all_results)

  }
  
  return(all_results)
  
}

#