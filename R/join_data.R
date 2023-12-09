join_data <-
function(obs_data, fill_data, iyear = 1990, fyear = 2019){
  
  date_control <- seq.Date(make_date(iyear), make_date(fyear, 12, 31), by = "days") %>%
    enframe(value = "date", name = NULL) 
  
  list_data <- list(date_control, obs_data, fill_data) %>%  
    reduce(left_join, by="date") 
  
  
  
  list_data %>%
    mutate(tmin = if_else(is.na(tmin.x), tmin.y, tmin.x),
           tmax = if_else(is.na(tmax.x), tmax.y, tmax.x),
           rain = if_else(is.na(rain.x), rain.y, rain.x)) %>%
    dplyr::select(date, rain, tmax, tmin, srad, wspd, rhum) %>%
    impute_mean_wth()
  
  
  
}
