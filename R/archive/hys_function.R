library(SplitR)
library(lubridate)

source("support_functions.R")

# Minneapolis
hysplit_traj <- function(lat      = 44.88, 
                     lon          = -93.22,
                     height       = 10,
                     duration     = 4,
                     run_period   = "2017-03-23",
                     daily_hours  = c(18),
                     direction    = "backward",
                     met_type     = "reanalysis",
                     met_dir      = "C:/Users/dkvale/Desktop/hysplit",
                     extended_met = F,
                     vert_motion  = 0,
                     model_height = 20000,
                     return_traj_df = TRUE,
                     traj_name      = NULL,
                     exec_dir       = NULL,
                     binary_path    = NULL,
                     os             = "win",
                     met_files      = met) {
  
  if (is.null(exec_dir)) exec_dir <- getwd()
  
  if (is.null(met_dir)) met_dir <- getwd()
  
  if (is.null(binary_path)) {
    
    if (os == "mac") {
      binary_path <-
        system.file("osx/hyts_std",
                    package = "SplitR")
    }
    
    if (os == "unix") {
      binary_path <-
        system.file("linux-amd64/hyts_std",
                    package = "SplitR")
    }
    
    if (os == "win") {
      binary_path <- system.file("win/hyts_std.exe", package = "SplitR")
    }
  }
  
  # Generate name of output folder
  if (is.null(traj_name)) {
    folder_name <- 
      paste0("traj-",
             format(Sys.time(),
                    "%Y-%m-%d-%H-%M-%S"))
  } else if (!is.null(traj_name)) {
    folder_name <- traj_name
  }
  
  if (length(run_period) == 1 &
      class(run_period) == "character" &
      all(grepl("[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]",
                run_period))) {
    run_type <- "day"
    run_day  <- run_period
  }
  
  if (length(run_period) == 2 &
      class(run_period) == "character" &
      all(grepl("[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]",
                run_period))) {
    run_type <- "range"
    run_range <- run_period
  }
  
  if (length(run_period) == 1 &
      class(run_period) == "numeric") {
    run_type <- "years"
    run_years <- run_period
  }
  
  if (length(run_period) == 2 &
      class(run_period) == "numeric") {
    run_type <- "years"
    run_years <- paste0(run_period[1], "-", run_period[2])
  }
  
  # Write default versions of the SETUP.CFG and
  # ASCDATA.CFG files in the working directory
  hysplit_config_init(dir = exec_dir)
  
  if (extended_met) {
    setup_cfg <- readLines('SETUP.CFG')
    setup_cfg <- gsub("(tm_.* )(0),", "\\11,", setup_cfg)
    cat(setup_cfg,
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
  
  # For every set of coordinates, perform a set of model runs
  for (z in 1:length(coords$lat)) {
    if (z == 1) ensemble_df <- data.frame()
    
    lat <- coords$lat[z]
    lon <- coords$lon[z]
    
    # Determine whether the run_years input is a single
    # year or a range
    if (exists("run_years")) run_years_single_range <-
      ifelse(nchar(run_years) == 4, "single", "range")
    
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
    for (i in 1:length(list_run_days)) {
      
      # Define starting time parameters
      start_year_GMT <- 
        substr(as.character(year(list_run_days[i])), 3, 4)
      
      start_month_GMT <-
        formatC(as.numeric(month(list_run_days[i])),
                width = 2, format = "d", flag = "0")
      
      start_day_GMT <-
        formatC(as.numeric(day(list_run_days[i])),
                width = 2, format = "d", flag = "0")
      
      # Sort daily starting hours if given as
      # numeric values
      if (class(daily_hours) == "numeric") {
        daily_hours <-
          formatC(sort(daily_hours),
                  width = 2,
                  flag = 0)
      }
      
      # Make nested loop with daily beginning hours
      for (j in daily_hours) {    
        start_hour_GMT <- j
        
        #--- Determine which met files are required for run
        
        # Determine the start time of the model run
        start_time_GMT <-
          ymd_hms(paste0(ifelse(start_year_GMT > 40,
                                paste0("19",
                                       start_year_GMT),
                                start_year_GMT), "-",
                         start_month_GMT, "-",
                         start_day_GMT, " ",
                         start_hour_GMT, ":00:00"))
        
        # Determine the end time of the model run
        end_time_GMT <-
          as.POSIXct(
            ifelse(direction == "backward", 
                   start_time_GMT -
                     (duration * 3600),
                   start_time_GMT +
                     (duration * 3600)),
            origin = "1970-01-01",
            tz = "UTC")
        
        # Determine whether the start year is a leap year
        leap_year <-
          leap_year(ymd(paste0(start_year_GMT, "-",
                               start_month_GMT, "-",
                               start_day_GMT)))
        
        # Determine whether the beginning and end of the
        # current run crosses over a calendar year
        number_of_calendar_years <-
          ifelse(year(start_time_GMT) == 
                   year(end_time_GMT), 1, 2)
        
        # Determine whether the beginning and end of the
        # current run crosses over a calendar month
        number_of_calendar_months <-
          ifelse(month(start_time_GMT) == 
                   month(end_time_GMT), 1, 2)
        
        #--- Divide different requirements for met files
        #    into different cases
        
        # Set the different cases to FALSE by default
        case_within_month <- FALSE
        case_over_year <- FALSE
        case_over_month <- FALSE
        
        # Determine which of the three cases is true
        if (number_of_calendar_years == 1 & 
            number_of_calendar_months == 1) {
          case_within_month <- TRUE
        } else if (number_of_calendar_years > 1) {
          case_over_year <- TRUE
        } else if (number_of_calendar_months > 1) {
          case_over_month <- TRUE
        } else { NULL }
        
        #--- Get vector lists of met files applicable to
        # run from GDAS 1-degree dataset
        
        # Trap leap-year condition of missing .w5 met
        # file for February in a '0' list value
        if (case_within_month &
            met_type == "gdas1") met <- 
          c(paste0(
            "gdas1.",
            substr(tolower(format(start_time_GMT,
                                  "%B")), 1, 3),
            substr(year(start_time_GMT), 3, 4), ".w1"),                                      
            paste0(
              "gdas1.",
              substr(tolower(format(start_time_GMT,
                                    "%B")), 1, 3),
              substr(year(start_time_GMT), 3, 4), ".w2"),
            paste0(
              "gdas1.",
              substr(tolower(format(start_time_GMT,
                                    "%B")), 1, 3),
              substr(year(start_time_GMT), 3, 4), ".w3"),
            paste0(
              "gdas1.",
              substr(tolower(format(start_time_GMT,
                                    "%B")), 1, 3),
              substr(year(start_time_GMT), 3, 4), ".w4"),
            ifelse(month(start_time_GMT) == 2 &
                     leap_year == TRUE, 0,
                   paste0(
                     "gdas1.",
                     substr(
                       tolower(format(start_time_GMT,
                                      "%B")), 1, 3),
                     substr(
                       year(start_time_GMT), 3, 4),
                     ".w5")))
        
        if (case_over_year &
            met_type == "gdas1") met <- 
          c(paste0(
            "gdas1.dec",
            substr(year(end_time_GMT), 3, 4), ".w3"),                                      
            paste0(
              "gdas1.dec",
              substr(year(end_time_GMT), 3, 4), ".w4"),
            paste0(
              "gdas1.dec",
              substr(year(end_time_GMT), 3, 4), ".w5"),
            paste0(
              "gdas1.jan",
              substr(year(start_time_GMT), 3, 4), ".w1"),
            paste0(
              "gdas1.jan",
              substr(year(start_time_GMT), 3, 4), ".w2"),
            paste0(
              "gdas1.jan",
              substr(year(start_time_GMT), 3, 4), ".w3"))
        
        if (case_over_month &
            met_type == "gdas1") met <-
          c(paste0(
            "gdas1.",
            substr(tolower(format(end_time_GMT,
                                  "%B")), 1, 3),
            substr(year(end_time_GMT), 3, 4), ".w3"),                                      
            paste0(
              "gdas1.",
              substr(tolower(format(end_time_GMT,
                                    "%B")), 1, 3),
              substr(year(end_time_GMT), 3, 4), ".w4"),
            ifelse(
              month(end_time_GMT) == 2 &
                leap_year == TRUE, 0,
              paste0("gdas1.",
                     substr(tolower(format(end_time_GMT,
                                           "%B")), 1, 3),
                     substr(year(end_time_GMT), 3, 4),
                     ".w5")),
            paste0(
              "gdas1.",
              substr(tolower(format(start_time_GMT,
                                    "%B")), 1, 3),
              substr(year(start_time_GMT), 3, 4), ".w1"),
            paste0(
              "gdas1.",
              substr(tolower(format(start_time_GMT,
                                    "%B")), 1, 3),
              substr(year(start_time_GMT), 3, 4), ".w2"),
            paste0(
              "gdas1.",
              substr(tolower(format(start_time_GMT,
                                    "%B")), 1, 3),
              substr(year(start_time_GMT), 3, 4), ".w3"))
        
        # Get vector lists of met files applicable to run
        # from the NCEP/NCAR reanalysis dataset
        if (met_type == "reanalysis") met <- 
          c(paste0(
            "RP",
            ifelse(start_month_GMT == "01",
                   year(start_time_GMT) - 1,
                   year(start_time_GMT)),
            ifelse(start_month_GMT == "01", "12",
                   formatC(month(start_time_GMT) - 1,
                           width = 2,
                           format = "d",
                           flag = "0")),
            ".gbl"),
            paste0(
              "RP",
              year(start_time_GMT),
              start_month_GMT, ".gbl"),
            paste0(
              "RP",
              ifelse(start_month_GMT == "12",
                     year(start_time_GMT) + 1,
                     year(start_time_GMT)),
              ifelse(start_month_GMT == "12", "01",
                     formatC(month(start_time_GMT) + 1,
                             width = 2,
                             format = "d",
                             flag = "0")),
              ".gbl"))
        
        # Get vector lists of met files applicable to run
        # from the NARR dataset
        if (met_type == "narr") met <- 
          c(paste0(
            "NARR",
            ifelse(start_month_GMT == "01",
                   year(start_time_GMT) - 1,
                   year(start_time_GMT)),
            ifelse(start_month_GMT == "01", "12",
                   formatC(month(start_time_GMT) - 1,
                           width = 2,
                           format = "d",
                           flag = "0"))),
            paste0(
              "NARR",
              year(start_time_GMT),
              start_month_GMT),
            paste0(
              "NARR",
              ifelse(start_month_GMT == "12",
                     year(start_time_GMT) + 1,
                     year(start_time_GMT)),
              ifelse(start_month_GMT == "12", "01",
                     formatC(month(start_time_GMT) + 1,
                             width = 2,
                             format = "d",
                             flag = "0"))))
        
        # Remove list values containing '0' (representing
        # missing .w5 data files for Feb in leap years)
        if (exists("met")) met <- met[!met %in% c(0)]
        
        print(met)
        
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
        #formatC(z, width = 5, format = "d", flag = "0"))
        
        all_trajectory_files <- 
          c(all_trajectory_files, output_filename)
        
        if (any(c("mac", "unix") %in% get_os())) {
          
          # Write start year, month, day, hour to
          # 'CONTROL'
          cat(start_year_GMT, " ", 
              start_month_GMT, " ",
              start_day_GMT, " ",
              start_hour_GMT, "\n",
              file = paste0(exec_dir, "/CONTROL"),
              sep = '', append = FALSE)
          
          # Write number of starting locations to
          # 'CONTROL'
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
                sep = '', append = TRUE)}
          
          # Write path for trajectory output files to
          # 'CONTROL'
          cat(exec_dir, "/\n",
              file = paste0(exec_dir, "/CONTROL"),
              sep = '', append = TRUE)
          
          # Write name of output filename to 'CONTROL'
          cat(output_filename, "\n",
              file = paste0(exec_dir, "/CONTROL"),
              sep = '', append = TRUE)
        }
        
        if (get_os() == "win") {
          
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
                sep = '', append = TRUE)}
          
          # Write path for trajectory output files to
          # 'CONTROL'
          cat(exec_dir, "/\n",
              file = paste0(exec_dir, "/CONTROL"),
              sep = '', append = TRUE)
          
          # Write name of output filename to 'CONTROL'
          cat(output_filename, "\n",
              file = paste0(exec_dir, "/CONTROL"),
              sep = '', append = TRUE)
        }
        
        # The CONTROL file is now complete and in the
        # working directory, so, execute the model run
        if (any(c("mac", "unix") %in% get_os())) {
          
          system(paste0("(cd ", exec_dir, " && ",
                        binary_path,
                        " >> /dev/null 2>&1)"))
        }
        
        if (get_os() == "win") {
          shell(paste0("(cd \"", exec_dir, "\" && \"",
                       binary_path,
                       "\")"))
        }
      }
    }
    
    # Create the output folder if it doesn't exist
    if (!dir.exists(paste0(exec_dir, "/",
                           folder_name))) {
      dir.create(
        path = paste0(exec_dir, "/",
                      folder_name))
    }
    
    if (any(c("mac", "unix") %in% get_os())) {
      
      # Perform the movement of all trajectory files
      # into a folder residing to the output directory
      for (i in 1:length(all_trajectory_files)) {
        system(paste0("(cd ", exec_dir, " && mv '",
                      all_trajectory_files[i], "' ",
                      paste0(exec_dir, "/",
                             folder_name),
                      ")"))
      }
    }
    
    if (get_os() == "win") {
      
      # Perform the movement of all trajectory files
      # into a folder residing to the output directory
      for (i in 1:length(all_trajectory_files)) {
        shell(paste0("(cd \"", exec_dir, "\" && move \"",
                     all_trajectory_files[i], "\" \"",
                     paste0(exec_dir, "/",
                            folder_name),
                     "\")"))
      }
    }
    
    # Obtain a trajectory data frame
    if (return_traj_df) {
      traj_df <-
        trajectory_read(output_folder =
                          paste0(exec_dir, "/",
                                 folder_name))
    }
    
    if (z == 1) {
      col_names <- colnames(traj_df)
      ensemble_df <-
        data.frame(mat.or.vec(nr = 0,
                              nc = length(col_names)))
      colnames(ensemble_df) <- col_names
    }
    
    traj_df[,1] <- z
    
    ensemble_df <- rbind(ensemble_df, traj_df)
  }
  
  return(ensemble_df)
}

