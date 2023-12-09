write_files_oryza <-
function(path_proj, test_data){
  
  
  ### Directorio de salidas (OUTPUTS)
  dir.create(path_proj)
  
  ##Crear Archivos climaticos
  dir_wth <- paste0(path_proj, "/WTH/")
  dir.create(dir_wth)
  
  test_data$data %>% 
    mutate(path = dir_wth , id_name = site) %>%
    dplyr::select(path, id_name, wth_data = wth, lat, lon, elev, stn) %>%
    mutate(wth_data = map(wth_data, ~.x %>% impute_mean_wth),
           pwalk(., write_wth_oryza, multiyear = F, tag = F))
  
  
  #crear archivos experimentales
  dir_exp <- paste0(path_proj, "/EXP/")
  dir.create(dir_exp, showWarnings = T)
  map(test_data$data$input_data, ~write_exp_oryza(.x, dir_exp, ET_mod = "PRIESTLY TAYLOR"))
  
  #crear archivos suelo
  dir_soil <- paste0(path_proj, "/SOIL/")
  dir.create(dir_soil, showWarnings = T)
  
  soil_data <- test_data$data$soil %>% 
    map(~.x %>%
          mutate(SOC = case_when(LOC_ID == "YOCS" ~ SOC/5,
                                 TRUE ~ SOC),
                 OM = (100/58)*SOC/10) %>% 
          mutate(SSKS = pmap_dbl(.l = list(SAND, CLAY, OM, SBDM), SSKS_cal)*2.4))   #multimodel bootstrapping + from mm/h to  cm/day)
  
  
  map2(soil_data, test_data$data$site,  ~write_soil_oryza(dir_soil, .y, .x, SATAV = 25, RIWCLI = 'NO'))
  
  
  
  
}
