cal_oryza_global <-
function(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20,  params_to_cal, calibration_path, cultivar, input_data, exp_files, test_params_model, basedata_path, res_var = c("yield"), phenol = NULL){
  
  
  ##Crea set de parametros  a calibrar 
  #x1 <- 20          #"SLATB"
  #x2 <- 10          #"FSHTB"
  #x3 <- 15          #"DRLVT"
  #x4 <- 12          #"BFTB" 
  #x5 = 0.0008554438  #"DVRJ"
  #x6 = 0.0007576     #"DVRI"
  #x7 = 0.0005704062  #"DVRP"
  #x8 = 0.002219568   #"DVRR"
  #x9 = 0.0085      #"RGRLMX
  #x10 = 0.004       #"RGRLMN
  #x11 = 0.004337274 #"SLAMAX
  #x12 = 0.2453043   #"FSTR"
  #x13 = 64900      #"SPGF"  
  #x14 = 0.0000249  #"WGRMX"  
  #x15 = 0.4        #"ZRTMCD"  
  #x16 = 1.45       #"ULLE"   
  #x17 = 1404       #"LLLE"   
  #x18 = 0.4        #"FSWTD"    
  #x19 = 21         #"COLDREP"
  #x20 = 36.5       #"CTSTER" 
  
  

    
    pgparams <- list(
      
      otherparams =  tibble(Parameter = params_to_cal$Parameter,
                            Set_cal = list(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20)) %>% 
        dplyr::filter(str_detect(Parameter, "SLATB|FSHTB|DRLVT|BFTB", negate = T)),
      
      
      BPF =  params_to_cal %>% dplyr::filter(Parameter == "BFTB") %>% pull(tables) %>% pluck(1) %>%
        dplyr::filter( id == as.integer(x4)) %>% 
        pull(data) %>% pluck(1) %>% enframe(name = "Parameter", value = "Set_cal"),
      
      SLA = params_to_cal %>% dplyr::filter(Parameter == "SLATB") %>% pull(tables) %>% pluck(1) %>%
        dplyr::filter( id == as.integer(x1)) %>% 
        pull(data) %>% pluck(1) %>% mutate(Parameter = "SLATB") %>% nest(Set_cal = -Parameter),
      
      
      FSH = params_to_cal %>% dplyr::filter(Parameter == "FSHTB") %>% pull(tables) %>% pluck(1) %>%
        dplyr::filter( id == as.integer(x2)) %>% 
        pull(data) %>% pluck(1) %>% mutate(Parameter = "FSHTB") %>% nest(Set_cal = -Parameter),
      
      DRL = params_to_cal %>% dplyr::filter(Parameter == "DRLVT") %>% pull(tables) %>% pluck(1) %>%
        dplyr::filter( id == as.integer(x3)) %>% 
        pull(data) %>% pluck(1) %>% mutate(Parameter = "DRLVT") %>% nest(Set_cal = -Parameter))
    
    
  
  
  params_to <- bind_rows(pgparams) 
  
  crop_params_oryza <- params_to$Set_cal %>% set_names(params_to$Parameter)
  
  
  ##Setting folder
  
  id_run <- as.integer(runif(1) * 10000000)
  dir_run <- make_dir_run(calibration_path, id_run)
  cultivar <- paste0(cultivar, "_", id_run)
  copy_inputs_oryza(dir_run, basedata_path)
  
  
  ### Write crop file 
  
  write_crop_oryza(dir_run, cultivar, crop_params_oryza)
  
  
  ### Run model Oryza
  run_model_oryza(dir_run, cultivar, exp_files)
  
  
  
  res_file <- str_subset(list.files(dir_run, full.names = T), "_res.dat") %>% 
    str_subset(str_to_lower(cultivar))
  
  sim_data_cal <- read_res_exp(res_file)
  
  
  
  metrics_cal <- map(res_var, 
                     ~eval_sim_oryza(input_data, sim_data_cal, exp_files, .x, T)) %>% bind_rows() %>% 
    dplyr::filter(var %in% c("IDAT", "FDAT", "MDAT", "LAI", "WAGT", "YIELD"))
  
  
  # files_remove <- list.files(dir_run, full.names = T) %>% str_subset(pattern = ".crp$|.dat$", negate = T)
  
  
  #map(files_remove, ~unlink(.x, recursive = T))
  
  unlink(dir_run, recursive = T)
  
  
  
  return(mean(metrics_cal$NRMSE))
  
  
  
}
