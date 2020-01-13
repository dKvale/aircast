#! /usr/bin/env Rscript

#"C:\Users\dkvale\Documents\R\R-3.5.2\bin\i386\Rscript.exe" --no-save --no-restore "X:\Agency_Files\Outcomes\Risk_Eval_Air_Mod\_Air_Risk_Evaluation\Staff folders\Dorian\AQI\aircast\R\run_current_hysplit_pt2.R"

library(dplyr)
library(readr)
library(tidyr)
library(here)


days_past <- 0

# Load HYSPLIT results
setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Current forecast")

back_forecast_pt1 <- read_csv(paste0(Sys.Date() - days_past, "_AQI_raw_HYSPLIT_pt1.csv"))

back_forecast_pt2 <- read_csv(paste0(Sys.Date() - days_past, "_AQI_raw_HYSPLIT_pt2.csv"))

back_forecast <- bind_rows(back_forecast_pt1, back_forecast_pt2)


# Filter to start location
back_forecast <- filter(back_forecast, 
                        as.Date(date2) %in% c(Sys.Date() - days_past, Sys.Date() - 1 - days_past))


# Save 
write_csv(back_forecast, paste0(Sys.Date() - days_past, "_AQI_raw_HYSPLIT.csv"))



##

