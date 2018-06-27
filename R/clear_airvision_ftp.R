#! /usr/bin/env Rscript


#-------------------------------#
#-- Clear outdated FTP files
#-------------------------------#
  
# Read .sh file
sh <- readLines("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/aircast/_sh scripts/clear_ftp.sh")
  
# Update file to search for dates from two days ago
new_line <- grep("grep", sh)
  
sh[new_line] <- paste0("for i in `curl -s -l ftp://\"$ftp_username\":\"$ftp_password\"@$ftp_ip/$ftp_path/ | grep ", "_840.0700", "`; do")
  
writeLines(sh, "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/aircast/_sh scripts/clear_ftp.sh")
  
# Run .sh file in command line to delete files from yesterday
shell(paste0('C: & "C:/Users/dkvale/Documents/Git/bin/sh" ',
             '"X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/aircast/_sh scripts/clear_ftp.sh"'))


##