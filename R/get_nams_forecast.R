#! /usr/bin/env Rscript

library(downloader)

get_nams_forecast <- function(date        = Sys.Date(), 
                              hour        = 12, 
                              folder      = date, 
                              resolution  = "12km",  # "12km" or "3km"
                              type        = "a",
                              time_step   = 1        # 1 or 3 hrs
                              ) {
  
  
  if(!is.null(folder)) dir.create(as.character(folder))
  
  if(!is.null(time_step) & time_step == 1) {
    time_step <- "s"
  } else {
    
    time_step <- NULL
  }
  
  date <- gsub("-", "", date)
  
  file_names   <- ifelse(resolution == "12km", 
                         paste0("hysplit.t", hour, "z.nam", time_step, type),
                         sapply(c("00","06","12","18","24","30","36","42"), 
                                function(x) paste0("hysplit.t", hour, "z.namsf", x, ".CONUS")))
                        
  
  for(file in file_names) {
    
    print(file)
    
    start <- Sys.time()
    
    url   <- paste0("ftp://arlftp.arlhq.noaa.gov/forecast/", date, "/", file)
    
    file_loc <- paste0(folder, "/", file)
    
    ## Working
    #data  <- httr::content(GET(paste0("ftp://arlftp.arlhq.noaa.gov/forecast/", date, "/", file)))
    
    #save_data <- file(file_loc, "wb")
    
    #writeBin(a, save_data)

    #data  <- getURLContent(paste0("ftp://arlftp.arlhq.noaa.gov/forecast/", date, "/", file))

    
    #writeLines(data, paste0(folder, "/", file))
    
    if (T) {
      download(url      = url,
               destfile = file_loc,
               method   = "internal",
               quiet    = FALSE,
               mode     = "wb")
    }
    
    print(round(Sys.time() - start))
  }
  
  closeAllConnections()
  
} 
