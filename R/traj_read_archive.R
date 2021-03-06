traj_read <- function(output_folder,
                      year = NULL,
                      start_height_m_AGL = NULL) {  
  
  # List files from the specified archive folder
  trajectory_file_list <- list.files(output_folder)
  
  # Optionally filter list of files by using 'year' or
  # 'start_height_m_AGL' arguments, or both
  if (is.null(year) &
      is.null(start_height_m_AGL)) {
    trajectory_file_list <- 
      list.files(
        path = output_folder,
        pattern = "^traj.*")
  } else if (!is.null(year) &
             is.null(start_height_m_AGL)) {
    trajectory_file_list <- 
      list.files(
        path = output_folder,
        pattern = paste0("^traj.*?-",
                         gsub("^[0-9][0-9]", "",
                              as.character(year)),
                         ".*$"))
  } else if (is.null(year) &
             !is.null(start_height_m_AGL)) {
    trajectory_file_list <- 
      list.files(
        path = output_folder,
        pattern = paste0("^.*?height_",
                         gsub("^[0-9][0-9]", "",
                              as.character(year)),
                         ".*$"))
  } else if (!is.null(year) &
             !is.null(start_height_m_AGL)) {
    trajectory_file_list <- 
      list.files(
        path = output_folder,
        pattern = paste0("^traj.*?-",
                         gsub("^[0-9][0-9]", "",
                              as.character(year)),
                         ".*?height_",
                         as.character(start_height_m_AGL),
                         "-.*$"))
  }
  
  # Initialize empty data frame with 12 named columns
  traj_df <- 
    setNames(data.frame(mat.or.vec(nr = 0, nc = 12)),
             nm = c("receptor", "year", "month", "day",
                    "hour", "hour.inc", "lat", "lon", 
                    "height", "pressure", "date2",
                    "date"))
  
  # Process all trajectory files
  for (i in 1:length(trajectory_file_list)) {
    
    # For each trajectory file, read each line and
    # determine where the variable-length header ends
    column_widths <- 92
    
    widths <- 
      nchar(
        readLines(
          paste0(path.expand(output_folder),
                 "/", trajectory_file_list[i])))
    
    if (any(widths == 18)) {
      
      filename <- paste0(
        path.expand(output_folder),
        "/", trajectory_file_list[i])
      
      read_characters <-
        readChar(filename, file.info(filename)$size)
      
      cat(gsub("([-0-9\\. ]{19}[-0-9\\. ])\n",
               "\\1", read_characters),
          file = filename,
          sep = "\n")
    }
    
    traj_temp <- 
      read.fwf(paste0(path.expand(output_folder),
                      "/", trajectory_file_list[i]),
               widths = column_widths)
    
    for (j in 1:nrow(traj_temp)) {
      if (length(grep("PRESSURE",
                      traj_temp[j,1])) != 0) {
        skip_up_to_line <- j
      } 
    }
    
    column_widths <- 
      c(6, 6, 6, 6, 6, 6, 6, 6, 8, 9, 9, 9, 9)
    
    traj <- 
      read.fwf(paste0(path.expand(output_folder),
                      "/", trajectory_file_list[i]),
               skip = skip_up_to_line,
               widths = column_widths)
    
    names(traj) <- 
      c("first", "receptor", "year", "month",
        "day", "hour", "zero1", "zero2", "hour.inc",
        "lat", "lon", "height", "pressure")
    
    traj$first <- traj$zero1 <- traj$zero2 <- NULL
    
    date2 <- mat.or.vec(nr = nrow(traj), nc = 1)
    
    for (k in 1:nrow(traj)) {
      date2[k] <- 
        ISOdatetime(ifelse(traj[1,2] < 50,
                           traj[1,2] + 2000,
                           traj[1,2] + 1900),
                    traj[1,3], traj[1,4], traj[1,5],
                    min = 0, sec = 0, tz = "GMT") +
        traj$hour.inc[k] * 3600
    }
    
    traj$date2 <-
      as.POSIXct(date2,
                 origin = "1970-01-01",
                 tz = "GMT")
    
    traj$date <-
      ISOdatetime(ifelse(traj[1,2] < 50,
                         traj[1,2] + 2000,
                         traj[1,2] + 1900),
                  traj[1,3], traj[1,4], traj[1,5],
                  min = 0, sec = 0, tz = "GMT")
    
    if (any(is.na(traj[,1]))) {
      traj <- traj[-which(is.na(traj[,1])),]
    }
    
    # Continuously bind data frames together to make
    # a large df from all trajectory files
    traj_df <- rbind(traj_df, traj)
  }
  
  widths <- 
    nchar(
      readLines(
        paste0(path.expand(output_folder),
               "/", trajectory_file_list[1])))
  
  if (any(widths > 92 )) {
    
    # Initialize empty data frame with 9 named columns
    traj_extra_df <- 
      setNames(data.frame(mat.or.vec(nr = 0, nc = 3)),
               nm = c("rainfall", "rh", "sunflux"))
    
    extra_column_widths <- 
      c(6, 6, 6, 6, 6, 6, 6, 6, 8, 9, 9, 9, 9,
        9, 9, 9, 9, 9, 9, 9, 9, 9)
    
    for (i in 1:length(trajectory_file_list)) {
      
      traj_extra <- 
        read.fwf(paste0(path.expand(output_folder),
                        "/", trajectory_file_list[i]),
                 skip = skip_up_to_line,
                 widths = extra_column_widths)[ ,14:16]
      
      if (any(is.na(traj_extra[,1]))) {
        traj_extra <- traj_extra[-which(is.na(traj_extra[,1])),]
      }
      
      names(traj_extra) <- c("rainfall", "rh", "sunflux")
      
     
      # Continuously bind data frames together to make
      # a large df from all trajectory files
      traj_extra_df <- rbind(traj_extra_df, traj_extra)
    }
    
    traj_df <- cbind(traj_df, traj_extra_df)
  }
  
  return(traj_df)
}
