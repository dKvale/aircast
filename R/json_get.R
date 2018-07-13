
a <- json_get(db, db_table, json_key)

db_table <- format(Sys.Date(), "%j")  #Yesterday's day of year

json_get <- function(db, db_table, json_key) {
  
  url <- paste("https://www.jsonstore.io", json_key, db, db_table, sep = "/")
  
   resp <- httr::GET(url)
  
  # Check success
  #httr::stop_for_status(resp)
  print(httr::http_status(resp))
  
  # Convert to data frame
  data_js <- httr::content(resp, "text")
    
  data_df <- jsonlite::fromJSON(data_js, flatten = T)$result

  return(data_df)
  
}
