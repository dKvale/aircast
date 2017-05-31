library(RPostgreSQL)

sites <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff folders/Dorian/AQI/MET data/Monitors and Rep Wx Stations.csv")

sites$`Upper Air` <- paste0("K", sites$`Upper Air`)

con <-  dbConnect('PostgreSQL',  dbname='wair', host='eiger', port=5432, user='dkvale', password='double_909')
ms  <- dbGetQuery(con, statement = paste("SELECT * FROM aqs.v_sites_and_parameters"))
ms  <- unique(ms[ ,c(c("site_catid", "site_name", "lat", "lon"))])

sites <- left_join(sites, ms)

sites[sites$site_catid == "27-137-9000", c("lat", "lon")] <- c(48.4128, -92.8292)
sites[sites$site_catid == "27-137-9000", c("lat", "lon")] <- c(47.878, -95.029)

names(sites)[9:10] <- c("monitor_lat", "monitor_long")

write_csv(sites[ , c(1:3,9:10,4:7)], "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff folders/Dorian/AQI/MET data/Monitors and Rep Wx Stations.csv")
