cal_growth_oryza <-
function(x1, x2, x3, x4, x5, x6, x7, x8,  params_to_cal, phen_params, calibration_path, cultivar, input_data, exp_files, test_params_model, basedata_path){
  
  safe_bind <- purrr::possibly(bind_rows, otherwise = NULL) 
  
  ## Oryza "Leaf and stem growth|Growth AGB-RZ"
  
  #x1 = 20          "SLATB"
  #x2 = 10          "FSHTB"
  #x3 = 15          "DRLVT"
  #x4 = 12          "BFTB" 
  #x5 = 0.0085      "RGRLMX
  #x6 = 0.004       "RGRLMN
  #x7 = 0.004337274 "SLAMAX
  #x8 = 0.2453043   "FSTR"
  
  pgparams <- list(
    
  otherparams =  tibble(Parameter = params_to_cal$Parameter,
                         Set_cal = list(x1, x2, x3, x4, x5, x6, x7, x8)) %>% 
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
 
      
     
  

  
  
  test_params_growth <-  safe_bind(pgparams) %>% bind_rows(phen_params)
  
  
  
  
  params_to <- test_params_growth %>% right_join(test_params_model, by = "Parameter") %>%
    mutate(to_test = map2(Base, Set_cal, function(x, y) if(is.null(y)){x} else {y}))
  
  crop_params_oryza <- params_to$to_test %>% set_names(params_to$Parameter)
  
  
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
  
  
  
  metrics_cal <- map(c("dry_matter", "lai"), 
                     ~eval_sim_oryza(input_data, sim_data_cal, exp_files, .x, T)) %>% 
                        bind_rows() %>% filter(var %in% c("WAGT", "LAI"))
  
  
  # files_remove <- list.files(dir_run, full.names = T) %>% str_subset(pattern = ".crp$|.dat$", negate = T)
  
  
  #map(files_remove, ~unlink(.x, recursive = T))
  
  unlink(dir_run, recursive = T)
  
  
  
  return(mean(metrics_cal$NRMSE))
  
  
  
}
