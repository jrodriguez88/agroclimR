remote_data_correction <-
function(obs_data, target_data, wth_vars = c("tmax", "tmin")) {
  
  # varsW  = colnames(obs_data)
  
  obs <-  daily_to_monthly(obs_data, na.rm = T) %>% group_by(month) %>% 
    summarise(across(!matches("year|month"), mean, na.rm = T))
  
  sim <- daily_to_monthly(target_data, na.rm = T) %>% 
    dplyr::select(year, month, everything()) %>% group_by(month) %>% 
    summarise(across(!matches("year|month"), mean, na.rm = T))
  
  
  monthly_diff <- left_join(obs, sim, by = "month") %>% 
    mutate(tmax = tmax.x - tmax.y,
           tmin = tmin.x - tmin.y,
           rain = case_when("rain" %in% wth_vars ~ rain.x/rain.y,
                            TRUE ~ 1)) %>%
    dplyr::select(month, tmax, tmin, rain) %>%
    pivot_longer(cols = -c(month), names_to = "var", values_to = "corr")
  
  
  target_data %>% 
    mutate(month = month(date)) %>% 
    pivot_longer(cols = -c(date, month), names_to = "var") %>%
    left_join(monthly_diff, by = c("month", "var")) %>%
    mutate(value = case_when(var == "tmax" | var == "tmin" ~ value + corr,
                             var == "rain" ~ value*corr, 
                             TRUE ~ value)) %>%
    dplyr::select(-c(month, corr)) %>%
    pivot_wider(names_from = var)
  
  
}
