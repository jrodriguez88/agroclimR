cal_metrics_wth <-
function(obs_data, sim_data, time = "monthly"){
  
  if(time == "monthly"){
    
    wth_data_test <- left_join(
      daily_to_monthly(obs_data), 
      daily_to_monthly(sim_data), 
      by = c("year", "month")) %>%
      pivot_longer(-c(year, month), names_to = "var")
    
  } else if(time == "daily") {
    
    wth_data_test <- left_join(
      obs_data, 
      sim_data, 
      by = c("date")) %>%
      pivot_longer(-c(date), names_to = "var")
    
  }
  
  
  wth_data_test %>%
    mutate(source = case_when(str_detect(var, "[.x]$") ~ "obs",
                              str_detect(var, "[.y]$") ~ "sim")) %>%
    drop_na() %>%
    mutate(
      var = str_sub(var, 1, -3)) %>%
    pivot_wider(names_from = source) %>% nest(-var) %>%
    mutate(metrics = map(data, ~get_metrics(.x))) %>%
    unnest(metrics)
  
  
  
}
