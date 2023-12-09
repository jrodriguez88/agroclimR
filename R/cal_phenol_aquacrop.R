cal_phenol_aquacrop <-
function(phen_data, wth_data, tbase = 8, Fleng = 10){
  
  
  
  #extract phenological dates
  
  PDAT <- phen_data %>% filter(var == "PDAT") %>% pull(value)
  EDAT <- phen_data %>% filter(var == "EDAT") %>% pull(value)
  IDAT <- phen_data %>% filter(var == "IDAT") %>% pull(value)
  FDAT <- phen_data %>% filter(var == "FDAT") %>% pull(value)
  MDAT <- phen_data %>% filter(var == "MDAT") %>% pull(value)
  
  clim_data <- wth_data %>%
    dplyr::filter(date >= PDAT,
                  date <= MDAT ) %>% mutate(HUH = ((tmax + tmin)/2) - tbase) %>% 
    dplyr::select(-c(tmax:rhum))
  
  #functions to filter
  
  filter_data_gdd <- function(clim_data, date1, date2, adj_d = 0){
    clim_data%>%
      dplyr::filter(date >= date1,
                    date < date2 + days(adj_d)) %>%
      summarise(sum_gdd = sum(HUH)) %>% pull(sum_gdd)
    
  }
  
  filter_data_days <- function(clim_data, date1, date2, adj_d = 0){
    clim_data%>%
      dplyr::filter(date >= date1,
                    date < date2 + days(adj_d)) %>%
      summarise(sum_days = n()) %>% pull(sum_days)
    
  }
  
  
  #make a param data.frame   
  data.frame(
    GDD_emergence   =  clim_data %>% filter_data_gdd(., PDAT, EDAT),
    GDD_CCx         =  clim_data %>% filter_data_gdd(., EDAT, IDAT),
    GDD_M           =  clim_data %>% filter_data_gdd(., EDAT, MDAT),
    GDD_FL          =  clim_data %>% filter_data_gdd(., EDAT, FDAT-5),
    GDD_FLL         =  clim_data %>% filter_data_gdd(., FDAT, FDAT, Fleng), # se estima como una duracion aproximada, necesita ser evaluado
    GDD_HI          =  clim_data %>% filter_data_gdd(., FDAT, MDAT -5),
    days_sow_eme    =  clim_data %>% filter_data_days(., PDAT, EDAT),
    days_sow_maxroot=  clim_data %>% filter_data_days(., PDAT, IDAT),
    days_sow_senesc =  clim_data %>% filter_data_days(., PDAT, FDAT, 14),
    days_sow_mat    =  clim_data %>% filter_data_days(., PDAT, MDAT),
    days_sow_flow   =  clim_data %>% filter_data_days(., PDAT, FDAT),
    days_flowlen    =  clim_data %>% filter_data_days(., FDAT-5, FDAT+5),
    days_buildHI    =  clim_data %>% filter_data_days(., FDAT, MDAT-7)
  )  
  
}
