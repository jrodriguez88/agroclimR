tidy_wth_dssat <-
function(wth_data){
  
  var_names <- colnames(wth_data)
  
  stopifnot(require(sirad))
  stopifnot(class(wth_data$date)=="Date" & all(c("tmax", "tmin", "rain", "srad") %in%  var_names))
  
  if (!"rhum" %in% var_names) {
    wth_data <- mutate(wth_data, rhum = " ")
    #    message("Wind Speed is not Available - Set as NA: -99")
  }  
  
  if (!"wspd" %in% var_names) {
    wth_data <- mutate(wth_data, wspd = " ")
#    message("Wind Speed is not Available - Set as NA: -99")
  } else {wth_data <- mutate(wth_data, wspd = wspd*3.6*24)}  #from wind speed mean (m/s) to (km/day)
  
  tav <- wth_data %>% 
    mutate(tmean = (tmax + tmin)/2 ) %>% 
    summarise(tav = mean(tmean)) %>% pull(tav)
  
  
  amp <- wth_data %>% 
    group_by(month(date)) %>%
    summarise(tmax = mean(tmax), tmin = mean(tmin)) %>%
    mutate(amp = tmax - tmin) %>% pull(amp) %>% mean
  
  return(list(wth_data = wth_data, tav = tav, amp = amp))
  
}
