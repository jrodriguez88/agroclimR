daily_to_monthly <-
function(wth_data, ...){
  
  
  var_names <- colnames(wth_data)
  
  #  stopifnot(require(lubridate))
  stopifnot(class(wth_data$date)=="Date")
  
  #  wth_vars <- var_names[var_names != "date"]
  
  
  wth_data %>% 
    group_by(year = year(date), month = month(date)) %>%
    summarise(
      across(matches("rain|prec"), sum, ...), 
      across(!matches("rain|prec"), mean, ...), .groups = 'drop') %>%
    dplyr::select(-c(date))
  
  
}
