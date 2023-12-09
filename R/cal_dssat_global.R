cal_dssat_global <-
function(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, params_to_cal, calibration_path, cultivar, input_data, exp_files, test_params_model, basedata_path, res_var = c("yield"), phenol = NULL){
  
  ## DSSAT global parameters
  #x1 <- 500  #"P1"
  #x2 <- 12     #"P2O"
  #x3 <- 100  #"P2R"
  #x4 <- 450   #"P5"
  #x5 <- 83  #"PHINT"
  #x6 <- 55   #"G1"
  #x7 <- 0.025     #"G2"
  #x8 <- 1     #"G3"
  #x9 <- 28  #"THOT"
  #x10 <- 15   #"TCLDP"
  
  params_to_cal <- tibble(Parameter = params_to_cal$Parameter,
                          to_test = list(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)) 
  
  crop_params_dssat <- params_to_cal$to_test %>% set_names(params_to_cal$Parameter)
  
  
  ##Setting folder
  
  id_run <- as.integer(runif(1) * 10000000)
  dir_run <- make_dir_run(calibration_path, id_run)
  
  
  
  # COpy neccesary files
  
  copy_inputs_dssat(dir_run, basedata_path, crop = "rice")
  
  
  ### Write crop file 
  write_crop_dssat(dir_run, cultivar, crop_params_dssat, ecotype = "IB0001")
  
  ### Run model dssat
  
  run_model_dssat(dir_run, "rice", exp_files)
  
  
  
  sim_data_cal <- read_plantgro(paste0(dir_run, "/PlantGro.OUT"))
  
  
  
  metrics_cal <- map(res_var, 
                     ~eval_sim_dssat(input_data, sim_data_cal, .x, T)) %>% bind_rows() %>% 
    dplyr::filter(var %in% c("FDAT", "MDAT", "LAI", "WAGT", "YIELD"))
  
  
  #files_remove <- list.files(dir_run, full.names = T) %>% str_subset(pattern = ".crp$|.dat$", negate = T)
  
  
  #map(files_remove, ~unlink(.x, recursive = T))
  
  unlink(dir_run, recursive = T)
  #  file.remove(paste0(calibration_path, cultivar2, ".CRO"))
  
  
  return(mean(metrics_cal$NRMSE))
  
  
  
}
