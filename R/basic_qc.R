basic_qc <-
function(df, fill=NA_real_, IQR_times = 5, max_temp_diff = 15, by_var = T){
  
  # change col data 
  if(any(str_detect(colnames(df), "Date"))){df <- rename(df, date = Date)}
  
  
  ## detect column names 
  var_names <- colnames(df)
  
  
  # detect consecutive values
  detect_consecutive <- function(data, n, exclude_zero = F){
    
    repet <- rle(data)
    
    if(exclude_zero == F){
      rep(repet$lengths >= n , times = repet$lengths)
    } else {
      rep(repet$lengths >= n & repet$values != 0, times = repet$lengths)
    }
    
    
  }

  # detect maximun temperature diference by day  
  detect_difference <- function(data, max_diff = max_temp_diff){
    
    c(0, abs(diff(data))) >= max_diff
    
  }
  
  
  
  if (class(df$date)=="Date" & all(c("tmax", "tmin", "rain") %in%  var_names)  & !isTRUE(by_var)){
    
    ## Calcula ranges by variable
    dat <- df
    
    range_tmin <- boxplot(dat$tmin~month(dat$date), plot = F, range = IQR_times)$stats
    range_tmax <- boxplot(dat$tmax~month(dat$date), plot = F, range = IQR_times)$stats
    #  range_rhum <- boxplot(dat$rhum~month(dat$date), plot = F, range = IQR_times)$stats
    #range_sbright <- boxplot(dat$sbright~month(dat$date), plot = F, range = IQR_times + 2)$stats
    
    
    ## base tb
    dat <- dat %>%
      mutate(tmin_min = range_tmin[1,month(date)],
             tmin_max = range_tmin[5,month(date)], 
             tmax_min = range_tmax[1,month(date)],
             tmax_max = range_tmax[5,month(date)]) %>% 
      #           rhum_min = range_rhum[1,month(date)],
      #           rhum_max = range_rhum[5,month(date)]) %>%
      #              sbright_min = range_sbright[1,month(date)],
      #              sbright_max = range_sbright[5,month(date)]) %>%
      mutate(tmax_consecutive = detect_consecutive(tmax, 5),
             tmax_difference = detect_difference(tmax, 15),
             tmin_consecutive = detect_consecutive(tmin, 5),
             tmin_difference = detect_difference(tmin, 15),
             rain_consecutive = detect_consecutive(rain, 5, T),
             tmax2 = case_when(tmax>48|tmax<15 ~ fill,
                               tmax > tmax_max|tmax < tmax_min ~ fill,
                               tmax < tmin ~ fill,
                               tmax == tmin ~ fill, 
                               tmax_consecutive == T ~ fill,
                               tmax_difference == T ~ fill,
                               TRUE ~ tmax),
             tmin2 = case_when(tmin>35|tmin<10 ~ fill,
                               tmin > tmin_max|tmin < tmin_min ~ fill,
                               tmin > tmax ~ fill,
                               tmin == tmax ~ fill,
                               tmin_consecutive == T ~ fill,
                               tmin_difference == T ~ fill,
                               TRUE ~ tmin),
             rain = case_when(rain>150|rain<0 ~ fill,
                              rain_consecutive == T ~ fill,
                              TRUE ~ rain),
             #         srad = if_else(srad>32|srad<4,  fill, srad),
             tmin = tmin2,
             tmax = tmax2) %>%
      dplyr::select(date, tmax, tmin, rain)
    
    
  
    
    } else if (class(df$date)=="Date" & any(str_detect(var_names, "rain|prec"))) {
      
      dat <- df
    
    # change col data 
      if(any(str_detect(colnames(dat), "prec"))){dat <- rename(dat, rain = prec)}
    
    ## base tb
    dat <- dat %>%
      mutate(rain_consecutive = detect_consecutive(rain, 5, T),
             rain = case_when(rain>150|rain<0 ~ fill,
                              rain_consecutive == T ~ fill,
                              TRUE ~ rain)) %>%
      dplyr::select(date, rain)
    
  
    } else if (class(df$date)=="Date" & any(str_detect(var_names, "tmax")) & isTRUE(by_var)) {
    
      dat <- df
      
     # range_tmin <- boxplot(dat$tmin~month(dat$date), plot = F, range = IQR_times)$stats
      range_tmax <- boxplot(dat$tmax~month(dat$date), plot = F, range = IQR_times)$stats
      #  range_rhum <- boxplot(dat$rhum~month(dat$date), plot = F, range = IQR_times)$stats
      #range_sbright <- boxplot(dat$sbright~month(dat$date), plot = F, range = IQR_times + 2)$stats
      
      
      ## base tb
      dat <- dat %>%
        mutate(tmax_min = range_tmax[1,month(date)],
               tmax_max = range_tmax[5,month(date)]) %>% 
        #           rhum_min = range_rhum[1,month(date)],
        #           rhum_max = range_rhum[5,month(date)]) %>%
        #              sbright_min = range_sbright[1,month(date)],
        #              sbright_max = range_sbright[5,month(date)]) %>%
        mutate(tmax_consecutive = detect_consecutive(tmax, 5),
               tmax_difference = detect_difference(tmax, 15),
               tmax2 = case_when(tmax>48|tmax<15 ~ fill,
                                 tmax > tmax_max|tmax < tmax_min ~ fill,
                                 #tmax < tmin ~ fill,
                                 #tmax == tmin ~ fill, 
                                 tmax_consecutive == T ~ fill,
                                 tmax_difference == T ~ fill,
                                 TRUE ~ tmax),
               tmax = tmax2) %>%
        dplyr::select(date, tmax)
    
    
    
    
    
    
    
  
    } else if (class(df$date)=="Date" & any(str_detect(var_names, "tmin")) &  isTRUE(by_var) ) {
      
      dat <- df
      
      range_tmin <- boxplot(dat$tmin~month(dat$date), plot = F, range = IQR_times)$stats
       #  range_rhum <- boxplot(dat$rhum~month(dat$date), plot = F, range = IQR_times)$stats
      #range_sbright <- boxplot(dat$sbright~month(dat$date), plot = F, range = IQR_times + 2)$stats
      
      
      ## base tb
      dat <- dat %>%
        mutate(tmin_min = range_tmin[1,month(date)],
               tmin_max = range_tmin[5,month(date)]) %>% 
        #           rhum_min = range_rhum[1,month(date)],
        #           rhum_max = range_rhum[5,month(date)]) %>%
        #              sbright_min = range_sbright[1,month(date)],
        #              sbright_max = range_sbright[5,month(date)]) %>%
        mutate(tmin_consecutive = detect_consecutive(tmin, 5),
               tmin_difference = detect_difference(tmin, 15),
               tmin2 = case_when(tmin>35|tmin<10 ~ fill,
                                 tmin > tmin_max|tmin < tmin_min ~ fill,
                                 #tmin > tmax ~ fill,
                                 #tmin == tmax ~ fill,
                                 tmin_consecutive == T ~ fill,
                                 tmin_difference == T ~ fill,
                                 TRUE ~ tmin),
               tmin = tmin2) %>%
        dplyr::select(date, tmin)
      
      
      
      
      
      
      
      
    } else {print("no data")}
  
  
  
  return(dat)
  
  
}
