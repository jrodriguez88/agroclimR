impute_mean_wth <-
function(wth_data, temp = "daily"){
  
  stopifnot(require(naniar))
  
  if(temp == "daily"){
    
    imp_data <- seq.Date(min(wth_data$date), max(wth_data$date), by = "1 day") %>% 
      enframe(name =  NULL, value = "date") %>% left_join(wth_data, by = "date") %>%  
      mutate(year = year(date), month = month(date)) %>% 
      nest(data = -c(year, month)) %>%
      mutate(data = map(data, ~.x %>% 
                          impute_mean_at(.vars = vars(-date)))) %>% 
      unnest(data) %>% dplyr::select(-c(year, month))
    
    
  } else if(temp == "monthly"){
    
    imp_data <- wth_data %>% 
      nest(data = -c(month)) %>%
      mutate(data = map(data, ~.x %>% 
                          impute_mean_at(.vars = vars(-year)))) %>% 
      unnest(data) %>% dplyr::select(c(year, month), everything())
    
  }
  
  
  return(imp_data)
  
  
}
