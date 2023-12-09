make_sim_dates <-
function(initial_date, planting_before, number_days, freq_sim){
  
  #  require(tidyverse)
  #  require(lubridate)
  #  require(magrittr)
  
  
  start_date <- seq.Date(initial_date, initial_date + days(number_days), by = freq_sim)
  
  plantig_date <- start_date + days(planting_before)
  
  dates <- list(start_date = start_date, planting_date = plantig_date)
  
  return(dates)
  
  
}
