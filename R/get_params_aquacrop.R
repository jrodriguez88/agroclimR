get_params_aquacrop <-
function(test_data, exp_set, tbase = 8, stat = "mean"){
  
  
  ## Exploratory Data Analysis - Experimental data - Seleccionar o descartar exp 
  
  exp_filter <- str_remove(exp_set, ".exp|.EXP")
  
  
  # Crop Phenology
  phen <- test_data$phen
  #plot_phen_obs(phen) #%>% ggplotly()
  
  #Leaf Area Index
  lai <- test_data$lai
  #plot_lai_obs(lai) #%>% ggplotly() 
  
  #Shoot Dry Matter
  dry_matter <- test_data$dry_matter %>% filter(var == "WAGT") %>% 
    nest(data  = - exp_file) %>% 
    mutate(data  =  map(data, ~.x %>% dplyr::filter(date == max(date)))) %>% 
    unnest(data) %>% dplyr::select(exp_file, WAGT = value, se)
  #plot_drymatter_obs(dry_matter) #%>% ggplotly()
  
  #Yield dry matter
  yield <- test_data$yield
  

  
  # Harvest Index calculation
  
  HI <- yield %>% left_join(dry_matter, by = join_by(exp_file)) %>%
    mutate(HI = value/WAGT) %>% dplyr::select(exp_file, HI) %>% 
    dplyr::filter(exp_file %in% exp_filter) 
  

  
  #funcion para remover separadores "_" de las variables a analizar
  remove_unders <- function(var){str_replace_all(var, "_", "")}
  
  
  
  #Agronomic data - Plant populations
  agro_data <- test_data$data$input_data %>% map(~.x[["AGRO_man"]]) %>% bind_rows() %>%
    mutate_at(.vars = vars(LOC_ID, CULTIVAR, PROJECT, TR_N), .funs = remove_unders) %>%
    mutate(PDAT = as.Date(PDAT), exp_file  = paste(LOC_ID, CULTIVAR, PROJECT, TR_N, sep = "_")) %>%
    dplyr::select(exp_file, PDAT:NPLDS) #%>% set_names(~tolower(.x))
  
  
  # Daily Weather data
  wth_list <- test_data$data %>% 
    mutate(id_name = site) %>%
    dplyr::select(id_name, wth, lat, lon, elev) %>%
    mutate(wth = map(wth, ~.x %>% impute_mean_wth))
  
  

  # Join data to parameter estimation
  data_param_aquacrop <- phen %>% dplyr::select(exp_file, data) %>% 
    dplyr::distinct() %>% rename(phen = data) %>% 
    dplyr::filter(exp_file %in% exp_filter) %>%
    left_join(
      lai %>% nest(data = date:se) %>% rename(lai=data), by = join_by(exp_file)) %>%
    mutate(id_name = word(exp_file, 1, sep = "_")) %>% 
    left_join(wth_list, by = join_by(id_name)) %>% left_join(agro_data, by = join_by(exp_file)) %>% 
    dplyr::select(id_name, exp_file, phen, lai, everything())
  
  
  #safe function ( TryCash)
  safe_canopy_params <- possibly(cal_canopycover_params, NULL)
  
  
  ## Test data 
  test_data2 <- data_param_aquacrop %>% 
    mutate(phen_params = map2(phen, wth, cal_phenol_aquacrop),
           canopy_params = pmap(list(wth_data = wth, phen_data = phen, lai_data = lai),  safe_canopy_params))
  
  ## Phen Params
  phen_params <- dplyr::select(test_data2, exp_file, phen_params) %>% 
    deframe() %>% compact() %>% enframe(name = "exp_file") %>% unnest(value) 
  
  
  ## Canopy Params
  canopy_params <- dplyr::select(test_data2, exp_file, canopy_params) %>% 
    deframe() %>% compact() %>% enframe(name = "exp_file") %>% unnest(value) 
  
  

  ##Genera un indice para calcular y filtrar resultados
  metric <- switch (stat,
                    "mean" = 1,
                    "min" = 2,
                    "max" = 3
  )
  
  

  
  ## MAke param List fron TEST_DATA
  param_list <- list(  
    

  #from sowing to emergence
  GDD_emergence = bootstrap_param(phen_params$GDD_emergence, stat = stat),
    
    
  #from emergence to maximum canopy cover
  GDD_CCx = bootstrap_param(phen_params$GDD_CCx, stat = stat),  
    
  #from emergence to flowering
  GDD_FL = bootstrap_param(phen_params$GDD_FL, stat = stat),
  
  #Length of the flowering stage
  GDD_FLL = bootstrap_param(phen_params$GDD_FLL, stat = stat),
  
  #from emergence to maturity
  GDD_M = bootstrap_param(phen_params$GDD_M, stat = stat),
  
  #GDD from emergence to start senescence
  GDD_senecence = bootstrap_param(canopy_params$GDD_senescence, stat = stat),
  
  #Canopy Growth Coefficient
  CGC = bootstrap_param(canopy_params$CGC, stat = stat),
  
  #Maximun canopy cover
  CCx = bootstrap_param(canopy_params$CCx, stat = stat),
  
  #Plant population per hectare 
  NPLDS = bootstrap_param(data_param_aquacrop$NPLDS, stat = stat)*10000,
  
  #Crop Water Productivity
#  WP
  
  #Reference Harvest Index
  HIo = bootstrap_param(HI$HI, stat = stat)*100,
  
  #Building-up of Harvest Index during yield formation
  GDD_HI = bootstrap_param(phen_params$GDD_HI, stat = stat),
  
  #Canopy Decline Coefficient
  CDC = bootstrap_param(canopy_params$CDC, stat = stat))
  
  #Maximum effective rooting depth
#  Zr
  
  #Crop coefficient when canopy is complete
#  Kc
  
  #Soil water depletion factor for canopy expansion- Lower threshold
#  Ks_exp
  
  #Minimum air temperature below which pollination starts to fail
#  Ks_polc
  
  #Maximum air temperature above which pollination starts to fail
#  Ks_polh
  
  
  
  
 
  ##Retorna lista de parametros para ingresar a archivo de cultivo 
  
  return(param_list)  
  
  
  
  
}
