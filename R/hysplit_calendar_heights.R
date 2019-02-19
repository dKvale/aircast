# Function to get HYSPLIT receptor height
# based on Month of year

get_hysplit_height <- function(month) {
  
  month <- format(as.Date(month), "%b")
  
  hy_hts <- data.frame(months  = format(seq(as.Date("2010-01-01"), as.Date("2010-12-31"), "month"), "%b"),
                       heights = c(200, 200, 250, 300, 400, 500, 500, 500, 400, 300, 250, 200),
                       stringsAsFactors = F)
  
  ht <- hy_hts[hy_hts$months == month, ]$heights
  
  return(ht)
  
  # Plot heights
  #hy_hts$months <- factor(hy_hts$months, levels = hy_hts$months)
  
  #ggplot2::ggplot(hy_heights, aes(x = factor(months, levels = hy_hts$months), y = heights)) + 
  #  geom_point() +
  #  geom_line()
}
