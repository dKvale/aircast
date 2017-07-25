#! /usr/bin/env Rscript

library(R.utils)
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(mailR)
#library(gmailr)

# E-mail subscribers
aqi_team <- paste0(c("dorian.kvale",
                     "steve.irwin",
                     "daniel.dix",
                     "monika.vadali",
                     "ruth.roberson",
                     "david.l.brown",
                     "rick.strassman"), "@state.mn.us")

aqi_team <- aqi_team[1]

#aqi_team <- paste0(aqi_team, collapse = ",")



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
if(FALSE) {
aqi_message <- c(
"---",
"title: ''",
"output:", 
"  html_fragment:",
"    keep_md: false",
"    self_contained: no",
"---",  
"\n",

"## Minnesota AQI report  ",
'<div style="margin-top: -15px;"> <i>Preliminary results for `r format(Sys.Date() - 1, "%A %b %d, %Y")`</i></div>  ',
"  ",
"```{r echo=FALSE, message =FALSE, warning =FALSE}",
'
library(formattable)
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(ggrepel)

aqi_colors <- c("#FFF",     # White
                "#9BF59B",  # Green
                "#ffff00",  # Yellow 
                "#ff7e00", 
                "#ff0000",  
                "#99004c")  

days_past <- 0

#-- Load yesterdays observations
#saveRDS(all_verify, paste0("Archive/test_", Sys.Date() - days_past, "_verification_table.Rdata"))
#verify <- readRDS(paste0("Archive/", Sys.Date() - days_past, "_verification_table.Rdata"))

verify <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Verification/2017_verification_table.csv")


#-- Filter to yesterdays results
verify <- filter(verify, forecast_date == Sys.Date() - days_past - 1)


#-- Remove day0 forecast from report
verify <- filter(verify, forecast_day != 0)


#-- Filter to day1 forecast, or most recent forecast if day1 not available
verify <- filter(verify, forecast_day == min(verify$forecast_day, na.rm = T))


#-- Select one forecast per site
verify <- verify[!duplicated(verify$site_catid), ]

names(verify)[grep("site_catid", names(verify))] <- "aqs_id"


#-- Select columns for report
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


# Plot accuracy
# Flip to tall table
tall_o3 <- tidyr::gather(data = verify_o3,
                         key = model, 
                         value = aqi, 
                         na.rm = FALSE, 
                         `Obs AQI`, `MPCA forecast`, `Model output`, `CMAQ forecast`)

# Set colors
tall_o3$background_color = ifelse(tall_o3$aqi == " " | is.na(tall_o3$aqi), "white",
                                ifelse(as.numeric(tall_o3$aqi) <= 50,  aqi_colors[2], 
                                ifelse(as.numeric(tall_o3$aqi) <= 100, aqi_colors[3],
                                ifelse(as.numeric(tall_o3$aqi) <= 150, aqi_colors[4], 
                                ifelse(as.numeric(tall_o3$aqi) <= 200, aqi_colors[5], "white")))))

# Convert to concentration
tall_o3 <- tall_o3 %>% 
           rowwise() %>% 
           mutate(conc = aqi2conc(aqi, "OZONE")) %>%
           group_by(Site) %>%
           mutate(`conc_diff (ppb)` = conc - conc[model == "Obs AQI"],
                  fcst_correct = identical(background_color[model == "Obs AQI"], background_color[model == "MPCA forecast"]))
            

# Performance metrics
mean_o3_diff   <- mean(abs(filter(tall_o3, !is.na(aqi), model == "MPCA forecast")$`conc_diff (ppb)`), na.rm = T) %>%
               round(1)

o3_cat_frx <- (sum(filter(tall_o3, !is.na(aqi), model == "Obs AQI")$fcst_correct, na.rm = T) / 
               nrow(filter(tall_o3, !is.na(aqi), model == "Obs AQI"))) %>% round(2)

```',
"  ",
"```{r echo=FALSE, message=FALSE, warning=FALSE, results='hide', fig.width=6, fig.height=6, fig.show='hide'}",
'
# Order sites
tall_o3$Site <- factor(tall_o3$Site, levels = rev(arrange(filter(tall_o3, model == "Obs AQI"), -conc)$Site))


# Add point labels
labels_df <- data_frame(model   = c("Obs AQI", "Model output", "MPCA forecast", "CMAQ forecast"),
                        m_label = c("X", "m", "m-", "c*") )

tall_o3 <- left_join(tall_o3, labels_df)

if(FALSE) {

# Plots
ggplot(tall_o3, aes(x = `conc_diff (ppb)`, y = Site, shape = model)) + 
   geom_text(aes(label = m_label), colour = "grey50", size = 5.8) +
   geom_text(aes(label = m_label, color = factor(background_color)), size = 5.2) + 
   scale_color_manual(values = levels(factor(tall_o3$background_color)), guide = F) +
   theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
   guides(label = guide_legend()) + 
   labs(y = "", title = "c* = CMAQ      m = model output     m- = steve-daniel")

}

# Regression line
verify_o3$background_color <- ifelse(verify_o3$`Obs AQI` == " " | is.na(verify_o3$`Obs AQI`), "white",
                                ifelse(as.numeric(verify_o3$`Obs AQI`) <= 50,  aqi_colors[2], 
                                  ifelse(as.numeric(verify_o3$`Obs AQI`) <= 100, aqi_colors[3],
                                  ifelse(as.numeric(verify_o3$`Obs AQI`) <= 150, aqi_colors[4], 
                                  ifelse(as.numeric(verify_o3$`Obs AQI`) <= 200, aqi_colors[5], "white")))))


line_plot <- filter(verify_o3, !is.na(`Obs AQI`)) %>%
             ggplot(aes(x = `Obs AQI`, y = `MPCA forecast`)) + 
               geom_abline(intercept = 0, size = 0.25, color = "grey50") +
               geom_abline(intercept = 5, size = 0.25, color = "grey50", linetype =3) +
               geom_abline(intercept = -5, size = 0.25, color = "grey50", linetype =3) +
               geom_point(colour = "grey50", size = 1.8) +
               geom_point(aes(color = factor(background_color)), size = 1.6) + 
               geom_text_repel(aes(label = Site), size = 1.5) + 
               scale_color_manual(values = levels(factor(tall_o3$background_color)), guide = F) +
               ylim(range(c(verify_o3$`Obs AQI`, verify_o3$`MPCA forecast`), na.rm = T)) +
               xlim(range(c(verify_o3$`Obs AQI`, verify_o3$`MPCA forecast`), na.rm = T)) +
               theme_bw() + 
               theme(aspect.ratio = 1)
               #labs(title = "MPCA forecast vs Ozone actuals")

png("C:/Users/dkvale/Desktop/fcst_regression.png", width = 700, height = 700, res = 300)
print(line_plot + theme_gray(base_size = 4.5))
dev.off()

png("fcst_regression.png", width = 700, height = 700, res = 300)
print(line_plot + theme_gray(base_size = 4.5))
dev.off()

#ggsave("fcst_regression.png")
#ggsave("C:/Users/dkvale/Desktop/fcst_regression.png")

verify_o3$background_color <- NULL

```',
"  ",
"```{r echo=FALSE, message =FALSE, warning =FALSE, results='hide'}",
'
# Leader board
#lead_board <- data_frame(Model           = c("Staniel Dirwin", "Dr Robot", "CMAQ"),
#                         `Accuracy (%)`  = c(100,100,100),
#                         `Bias (ppb)`    = c(5,5,5),
#                         `forecasts (n)` = c(0,0,0))

# Load past results
lead_board <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff folders/Dorian/AQI/Verification/lead_board.csv")
#lead_board <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff folders/Dorian/AQI/Verification/lead_board_bkup.csv")

# New performance metrics
## Dr. Robot
robot_cat_frx <- tall_o3 %>% 
                 filter(model %in% c("Obs AQI","Model output")) %>%
                 group_by(Site) %>%
                 mutate(fcst_correct = identical(background_color[model == "Obs AQI"], 
                                                 background_color[model == "Model output"])) %>%
                 ungroup() %>%
                 summarize(n_obs    = sum(!is.na(aqi[model == "Obs AQI"])),
                           accuracy = 100 * sum(fcst_correct[model == "Obs AQI"], na.rm = T) / n_obs) %>%
                 .[1, "accuracy"] %>% 
                 round()

robot_mean_o3_diff <- mean(abs(filter(tall_o3, !is.na(aqi), model == "Model output")$`conc_diff (ppb)`), na.rm = T) %>%
                   round(1)

## CMAQ
cmaq_cat_frx <- tall_o3 %>% 
                filter(model %in% c("Obs AQI","CMAQ forecast")) %>%
                group_by(Site) %>%
                mutate(fcst_correct = identical(background_color[model == "Obs AQI"], 
                background_color[model == "CMAQ forecast"])) %>%
                ungroup() %>%
                summarize(n_obs    = sum(!is.na(aqi[model == "Obs AQI"])),
                accuracy = 100 * sum(fcst_correct[model == "Obs AQI"], na.rm = T) / n_obs) %>%
                .[1, "accuracy"] %>% 
                round()

cmaq_mean_o3_diff <- mean(abs(filter(tall_o3, !is.na(aqi), model == "CMAQ forecast")$`conc_diff (ppb)`), na.rm = T) %>%
                  round(1)

## New performance table
new_stats <- data_frame(Model              = c("Staniel Dirwin", "Dr Robot", "CMAQ"),
                        `new Accuracy (%)` = c(round(o3_cat_frx * 100), robot_cat_frx$accuracy, cmaq_cat_frx$accuracy),
                        `new Bias (ppb)`   = c(mean_o3_diff, robot_mean_o3_diff, cmaq_mean_o3_diff),
                        new_obs            = c(1, 1, 1))

# Update performance
lead_board <- left_join(new_stats, lead_board)

lead_board <- lead_board %>% 
              mutate(`forecasts (n)` = `forecasts (n)` + new_obs,
                     `Accuracy (%)`  = ((`Accuracy (%)` * (`forecasts (n)` - 1) + `new Accuracy (%)` * new_obs) / max(1, `forecasts (n)`)) %>% 
                                       round(),
                     `Bias (ppb)`    = ((`Bias (ppb)` * (`forecasts (n)` - 1) + `new Bias (ppb)` * new_obs) / max(1, `forecasts (n)`)) %>% 
                                       round(1))

# Create web table
margin_right <- function(i) {
    formatter("span", style = x ~ style("padding-right"  = paste0(i, "px")))
}


yest_board <- select(lead_board, c(Model, `new Accuracy (%)`, `new Bias (ppb)`))

names(yest_board) <- gsub("new ", "", names(yest_board))

yest_board$Date   <- Sys.Date() - 1

yest_board <- formattable(yest_board,
                          list(Model             = margin_right(24),
                               `O3 Accuracy (%)` = margin_right(84),
                               `PM Accuracy (%)` = margin_right(84),
                               `O3 Bias (ppb)`   = margin_right(70)),
                               `PM Bias (ppb)`   = margin_right(70)),
                          align = c("l","l","l"))

lead_board <- select(lead_board, c(Model, `Accuracy (%)`, `Bias (ppb)`, `forecasts (n)`))
lead_board <- formattable(lead_board,
                          list(Model           = margin_right(24),
                               `O3 Accuracy (%)` = margin_right(84),
                               `PM Accuracy (%)` = margin_right(84),
                               `O3 Bias (ppb)`   = margin_right(70)),
                               `PM Bias (ppb)`   = margin_right(70)),
                          align = c("l","l","l","l"))


# Save tables
saveRDS(yest_board, "yesterday_board.rdata")
saveRDS(lead_board, "leader_board.rdata")

# Archive updated performance
#lead_board <- select(lead_board, -c(`Accuracy (%)`, `Bias (ppb)`))
lead_board  <- as_data_frame(lead_board)

class(lead_board) <- class(lead_board)[-1]

#write_csv(lead_board, "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff folders/Dorian/AQI/Verification/lead_board.csv")

```',
"  ",
"```{r echo=FALSE, message =FALSE, warning =FALSE}",
'
# Drop model output value unless different than MPCA forecast
verify_o3$`Model output` <- ifelse(verify_o3$`MPCA forecast` == verify_o3$`Model output`, " ", verify_o3$`Model output`)

# Add paddding to columns
verify_o3$Site <- paste0(verify_o3$Site, "    ")

#verify_o3$`Obs count` <- paste0(verify_o3$`Obs count`, " ")

# Web format
margin_left <- function(i) {
  
  formatter("span",
            style = x ~ style("padding-left"  = paste0(i, "px")))
}


col_format <- function(i) {
  

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


names(verify_o3)[5] <- "MPCA fcast"

verify_o3 <- formattable(verify_o3,
                         list(Site              = margin_right(20),
                              Group             = margin_right(20),
                              #`AQS ID`         = margin_right(20),
                              `Obs count`       = margin_right(65),
                              `Obs AQI`         = col_format(15),
                              `MPCA fcast`      = col_format(15),
                              `Model output`    = col_format(15),
                              `CMAQ forecast`   = col_format(5)),
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
'<div style="margin-top: -6px;">',
#"### Yesterday&#39;s performance  ",
"</div>",
'<div style="margin-top: 4px;">',
"Yesterday&#39;s next-day forecast had a category accuracy of __`r round(100 * o3_cat_frx)`%__
(`r sum(filter(tall_o3, !is.na(aqi), model == 'Obs AQI')$fcst_correct, na.rm = T)`/`r nrow(filter(tall_o3, !is.na(aqi), model == 'Obs AQI'))` sites)
for ozone, and a category accuracy of for PM-2.5.", 
"</div>  ",
'<div style="margin-top: -8px; margin-left: 23px;">',
"```{r, echo = F, warning = F, message = F}",
"yest_board <- readRDS('yesterday_board.rdata')",
"yest_board",
"```",
"</div>",
"  ",
"### Overall model performance for `next-day` forecasts",
'<div style="margin-top: -10px; margin-left: 20px;">',
"```{r, echo = F, warning = F, message = F}",
"lead_board <- readRDS('leader_board.rdata')",
"lead_board",
"```",
"</div>",
"  ",
"### MPCA forecast AQI vs Monitored actuals _(- -5 AQI buffer- -)_ ",
'<div style = "margin-top: -8px; margin-left: 4px;">
<img src="fcst_regression.png" width="500" height="500" align = "left"> 
<img src="fcst_regression.png" width="500" height="500" align = "left"> 
</div>   ',
"  ",
"### Yesterday&#39;s results",
"##### Ozone",
#paste0(kable(verify_o3), collapse="\n"),
'<div style="margin-top: -6px; margin-left: 15px;">',
"```{r, echo = F, warning = F, message = F}",
"o3_df <- readRDS('o3_table.rdata')",
"o3_df",
"```",
'</div>',
"  ",
"##### PM-2.5",
#paste0(kable(verify_o3), collapse="\n"),
'<div style="margin-top: -6px; margin-left: 15px;">',
"```{r, echo = F, warning = F, message = F}",
"pm_df <- readRDS('pm_table.rdata')",
"pm_df",
"```",
'</div>',
"  "
)
}

#-- Knit Rmarkdown document
setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff folders/Dorian/AQI/Verification")
#writeLines(aqi_message, "aqi_message.Rmd")

rmarkdown::render("aqi_message.Rmd")


#-- Create e-mail message
msg_body <- readLines("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/Verification/aqi_message.html")

#-- Collapse to single line
msg_body <- paste0(msg_body, collapse = "")

msg_body <- paste0('<style type="text/css"> th, td {border-bottom: 1px solid #ddd;} </style>', msg_body)

msg_body <- gsub("<table", 
                 '<table style = "padding-right: 10px; padding-bottom: 2px;', msg_body)

#-- Send message function
send_msg <- function(x) {
  
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
            inline       = TRUE,
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
  
  while(is.na(send_fail) & run_count < 3) {
    
    #detach("package:gmailr", unload = TRUE)
    #library(gmailr)
    #gmail_auth(secret = 'gmail-credentials.json', scope = 'compose') 
    #use_secret_file("gmail-credentials.json")
    
    #-- Set time limit on run time
    send_fail <- tryCatch(evalWithTimeout(send_msg(i), timeout = 5, onTimeout = "error"), 
                          TimeoutException = function(ex) NA, 
                          error = function(e) NA)
    
    run_count <- run_count + 1
    
    Sys.sleep(1)
    
  

  }
}
       


##
