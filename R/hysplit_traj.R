#! /usr/bin/env Rscript

library(SplitR)
library(lubridate)


source(paste0(aircast_path, "R/hysplit_support_functions.R"))
source(paste0(aircast_path, "R/traj_read.R"))


# Test extended_met
if (FALSE) {
  
  a = hysplit_trajectory(height = c(10),
                         duration = 24,
                         run_period = "2016-10-05",
                         daily_hours = 12,
                         direction = "backward",
                         met_type = "reanalysis",
                         extended_met = T) 
}

hysplit_traj <- function(lat            = 44.88,  # Minneapolis
                         lon            = -93.22,
                         height         = 10,
                         duration       = 24,
                         run_period     = "2017-03-23",
                         daily_hours    = 17,
                         direction      = "backward",
                         met_type       = "narr",
                         met_dir        = "C:/Users/dkvale/Desktop/aircast/hysplit/archive_hysplit/NARR",
                         extended_met   = TRUE,
                         vert_motion    = 0,
                         model_height   = 20000,
                         return_traj_df = TRUE,
                         traj_name      = as.character(round(runif(1), 5)),
                         exec_dir       = NULL,
                         binary_path    = NULL,
                         os             = "win",
                         met_files      = met_list) {
  
  if (is.null(exec_dir)) exec_dir <- getwd()
  
  if (is.null(met_dir)) met_dir   <- getwd()
  
  binary_path <- system.file("win/hyts_std.exe", package = "SplitR")
  
  # Generate name of output folder
  folder_name <- paste0("traj-", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"))
  
  if (length(run_period) == 1 &
      class(run_period) == "character" &
      all(grepl("[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]",
                run_period))) {
    
    run_type <- "day"
    run_day  <- run_period
  }
  
  if (length(run_period) == 1 &
      class(run_period) == "numeric") {
       run_type <- "years"
       run_years <- run_period
  }
  
  # Write default versions of the SETUP.CFG and
  # ASCDATA.CFG files in the working directory
  hysplit_config_init(dir = exec_dir)
  
  
  if (extended_met) {
    #setup_cfg <- readLines('SETUP.CFG')
    #setup_cfg <- gsub("(tm_.* )(0),", "\\11,", setup_cfg)
    #cat(setup_cfg,
    #    sep = "\n",
    #    file = paste0(exec_dir, "/", "SETUP.CFG"))
    
    # dswf = DOWNWARD SHORT WAVE RADIATION FLUX
    # tppa = TOTAL ACCUMULATED PRECIPITATION
    
    cat(c("&SETUP","kmsl = 0,","tm_rain = 1,", "tm_relh = 1,", "tm_dswf = 1,", "/ "),
        sep = "\n",
        file = paste0(exec_dir, "/", "SETUP.CFG"))
  }
  
  
  # Stop function if there are vectors of different
  # length for `lat` and `lon`
  if (length(lat) != length(lon)) {
    stop("The coordinate vectors are not the same length.")
  }
  
  # Create a coordinates list
  coords <- list(lat = lat, lon = lon)
  
  z <- 1

  ensemble_df <- data.frame()
    
  lat <- coords$lat[z]
  lon <- coords$lon[z]
    
  # Determine whether the run_years input is a single
  # year or a range
  if (exists("run_years")) run_years_single_range <- ifelse(nchar(run_years) == 4, "single", "range")
    
  # Make a vector list of run days in POSIXct format
  if (run_type == "day") {
      list_run_days <- 
        as.POSIXct(run_day,
                   origin = "1970-01-01",
                   tz = "UTC")
    } else if (run_type == "range") {
      list_run_days <- 
        seq(as.POSIXct(run_range[1],
                       origin = "1970-01-01",
                       tz = "UTC"),
            as.POSIXct(run_range[2],
                       origin = "1970-01-01",
                       tz = "UTC"),
            by = 86400)
    } else if (run_type == "years") {
      list_run_days <- 
        seq(
          as.POSIXct(paste0(substr(run_years, 1, 4),
                            "-01-01"), 
                     origin = "1970-01-01",
                     tz = "UTC"),
          as.POSIXct(
            ifelse(run_years_single_range == "single",
                   paste0(substr(run_years, 1, 4),
                          "-12-31"),
                   paste0(substr(run_years, 6, 9),
                          "-12-31")), 
            origin = "1970-01-01",
            tz = "UTC"),
          by = 86400)
    } else {
      stop("A run type has not been selected")
    }
    
    # Initialize a vector that will contain names for
    # all files generated
    all_trajectory_files <-  vector(mode = "character", length = 0)
    
    # Make loop with all run days
    i  <- 1
      
    # Define starting time parameters
      start_year_GMT <- 
        substr(as.character(year(list_run_days[i])), 3, 4)
      
      start_month_GMT <-
        formatC(as.numeric(month(list_run_days[i])),
                width = 2, format = "d", flag = "0")
      
      start_day_GMT <-
        formatC(as.numeric(day(list_run_days[i])),
                width = 2, format = "d", flag = "0")
      
      start_hour_GMT <-  daily_hours[[1]]
      
      print(met_files)
        
      met <- met_files
          
         
      # Construct the output filename string for this
      # model run
      output_filename <-
          paste0("traj-",
                 ifelse(is.null(traj_name), 
                        "", traj_name),
                 "-",
                 ifelse(direction == "backward",
                        "bwd", "fwd"), "-",
                 start_year_GMT, "-",
                 start_month_GMT, "-",
                 start_day_GMT, "-",
                 start_hour_GMT, "-",
                 z,
                 "lat_", gsub("\\.", "p", as.character(lat)), "_",
                 "lon_", gsub("\\.", "p", as.character(lon)), "-",
                 "hgt_", height, "-",
                 duration, "h")

        
        all_trajectory_files <- c(all_trajectory_files, output_filename)
        
        # Write start year, month, day, hour to
        # 'CONTROL'
          cat(start_year_GMT, " ", 
              start_month_GMT, " ",
              start_day_GMT, " ",
              start_hour_GMT, "\n",
              file = paste0(exec_dir, "/CONTROL"),
              sep = '', append = FALSE)
          
        # Write number of starting locations
        # to 'CONTROL'
          cat("1\n",
              file = paste0(exec_dir, "/CONTROL"),
              sep = '', append = TRUE)
          
        # Write starting latitude, longitude, height
        # AGL to 'CONTROL'
          cat(coords$lat[z], " ", 
              coords$lon[z], " ", 
              height, "\n",
              file = paste0(exec_dir, "/CONTROL"),
              sep = '', append = TRUE)
          
        # Write direction and number of simulation
        # hours to 'CONTROL'
          cat(ifelse(direction == "backward", "-", ""),
              duration, "\n",
              file = paste0(exec_dir, "/CONTROL"),
              sep = '', append = TRUE)
          
        # Write vertical motion option to 'CONTROL'
          cat(vert_motion, "\n",
              file = paste0(exec_dir, "/CONTROL"),
              sep = '', append = TRUE)
          
        # Write top of model domain in meters to
        # 'CONTROL'
          cat(model_height, "\n",
              file = paste0(exec_dir, "/CONTROL"),
              sep = '', append = TRUE)
          
        # Write number of met files used to 'CONTROL'
          cat(length(met), "\n",
              file = paste0(exec_dir, "/CONTROL"),
              sep = '', append = TRUE)
          
        # Write met file paths to 'CONTROL'
          for (i in 1:length(met)) {
            cat(met_dir, "/\n", met[i], "\n",
                file = paste0(exec_dir, "/CONTROL"),
                sep = '', append = TRUE)
            }
          
        # Write path for trajectory output files to
        # 'CONTROL'
          cat(exec_dir, "/\n",
              file = paste0(exec_dir, "/CONTROL"),
              sep = '', append = TRUE)
          
        # Write name of output filename to 'CONTROL'
          
        # Edit to always save to same file to limit new folders  - Dorian
          if (FALSE) {cat("temp_traj_file", "\n",
              file = paste0(exec_dir, "/CONTROL"),
              sep = '', append = TRUE)
          }
          
           cat(output_filename, "\n",
               file = paste0(exec_dir, "/CONTROL"),
               sep = '', append = TRUE)
          
        
        # The CONTROL file is now complete and in the
        # working directory, so, execute the model run
        shell(paste0('C: &&', 'cd "', exec_dir, '" && "', binary_path, '"'))
    
    # Create the output folder if it doesn't exist
    if (!dir.exists(paste0(exec_dir, "/", folder_name))) {
      dir.create(path = paste0(exec_dir, "/", folder_name))
    }
    
    # Perform the movement of all trajectory files
    # into a folder residing to the output directory
    for (i in 1:length(all_trajectory_files)) {
         shell(paste0("(cd \"", exec_dir, "\" && move \"",
                      all_trajectory_files[i], "\" \"",
                      paste0(exec_dir, "/",
                             folder_name),
                      "\")"))
    }

    
    # Obtain a trajectory data frame
    traj_df <- traj_read(output_folder = paste0(exec_dir, "/", folder_name))
    
    col_names   <- colnames(traj_df)
    
    ensemble_df <- data.frame(mat.or.vec(nr = 0, nc = length(col_names)))
    
    colnames(ensemble_df) <- col_names
    
    traj_df[ , 1] <- z
    
    ensemble_df <- rbind(ensemble_df, traj_df)
  
  return(ensemble_df)
}


