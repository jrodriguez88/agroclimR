cal_phen_oryza <-
function(x1, x2, x3, x4, params_to_cal, calibration_path, cultivar, input_data, exp_files, test_params_model, basedata_path){
  
  ## Oryza Phenological parameters
  #x1 <- 0.0008554438  #"DVRJ"
  #x2 <- 0.0007576     #"DVRI"
  #x3 <- 0.0005704062  #"DVRP"
  #x4 <- 0.002219568   #"DVRR"
  
  params_to_cal <- tibble(Parameter = params_to_cal$Parameter,
                          Set_cal = list(x1, x2, x3, x4)) %>% right_join(test_params_model, by = "Parameter") %>%
    mutate(to_test = map2(Base, Set_cal, function(x, y) if(is.null(y)){x} else {y}))
  
  crop_params_oryza <- params_to_cal$to_test %>% set_names(params_to_cal$Parameter)
  
  
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
  
  
  
  metrics_cal <- map(c("phen"), 
                     ~eval_sim_oryza(input_data, sim_data_cal, exp_files, .x, T)) %>% bind_rows()
  
  
  #files_remove <- list.files(dir_run, full.names = T) %>% str_subset(pattern = ".crp$|.dat$", negate = T)
  
  
  #map(files_remove, ~unlink(.x, recursive = T))
  
  unlink(dir_run, recursive = T)
  
  
  
  return(mean(metrics_cal$NRMSE))
  
  
  
}
