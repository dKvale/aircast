

json_key <- "b86a846b8f240223b1950a89660cc1cf7fb3f26560748d7d145848cec2f6b3bd"

db <- "met17"

db_table <- 1

db_table <- format(Sys.Date(), "%j")  #Day of year

data <- read_csv('"age","name"
                 32,"Lamb chop"
                 32,"Dill"')


json_post(data, db, db_table, json_key)


json_post <- function(data = NULL, db, db_table, json_key) {

   if (is.null(data)) return("Data is null")
  
   url <- paste("https://www.jsonstore.io", json_key, db, db_table, sep = "/")
  
   #data_js <- jsonlite::toJSON(data, flatten = T)
   
   resp <- httr::POST(url, body = data, encode = "json")
     
   # Check success
   print(httr::http_status(resp))

}
