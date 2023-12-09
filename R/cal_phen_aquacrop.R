cal_phen_aquacrop <-
function(x1, x2, x3, x4, x5, x6, x7, params_to_cal, calibration_path, cultivar, input_data, exp_files, test_params_model, basedata_path){
  
  ## Aquacrop Phenological parameters
  #x1 <- 150  #"GDD_emergence"
  #x2 <- 1150     #"GDD_FL"
  #x3 <- 350  #"GDD_FLL"
  #x4 <- 1900   #"GDD_M"
  
  params_to_cal <- tibble(Parameter = params_to_cal$Parameter,
                          Set_cal = list(x1, x2, x3, x4, x5, x6, x7)) %>% right_join(test_params_model, by = "Parameter") %>%
    mutate(to_test = map2(Base, Set_cal, function(x, y) if(is.null(y)){x} else {y}))
  
  crop_params_aquacrop <- params_to_cal$to_test %>% set_names(params_to_cal$Parameter)
  
  
  ##Setting folder
  
  id_run <- as.integer(runif(1) * 10000000)
  dir_run <- make_dir_run(calibration_path, id_run)
  cultivar2 <- paste0(cultivar, id_run)
  copy_inputs_aquacrop(dir_run, basedata_path)
  
  
  filesX <- list.files(paste0(calibration_path, "/LIST"), pattern = "PRM", full.names = T) %>%
    str_subset(pattern = paste(exp_files, collapse = "|"))
  
  list_run <- paste0(dir_run, "/LIST/")
  
#  dir.create(list_run)
  
 # file.copy(filesX, paste0(dir_run, "/LIST/"))
  
 # new_proj <- list.files(paste0(dir_run, "/LIST/"), full.names = T)
  
  projects <-  filesX  %>% map(read_lines) %>% 
    map(function(x) {
      x[44] <- paste0(cultivar2, ".CRO")
      x})
  
  exp_names <- paste0(list_run, list.files(paste0(calibration_path, "/LIST"), pattern = "PRM") %>%
                        str_subset(pattern = paste(exp_files, collapse = "|")))
  
  map2(.x = exp_names, .y = projects, .f = function(a,b){
    sink(file = a,  append = F)
    writeLines(b)
    sink()}
    )
  

  ### Write crop file 
  
  write_crop_aquacrop(calibration_path, cultivar2, crop_params_aquacrop)
  

  ### Run model aquacrop
  run_model_aquacrop(dir_run, cultivar2, exp_files)
  
  list_op <- paste0(dir_run, "OUTP/")
  
  sim_data_cal <- map(.x = list.files(list_op, pattern = "day", full.names = T), 
                      ~read_aquacrop_day(file = .x)) %>% 
    set_names(list.files(list_op, pattern = "day") %>% str_remove_all("PRMday.OUT"))
  
  
  
  metrics_cal <- map(c("phen"), 
                     ~eval_sim_aquacrop(input_data, sim_data_cal, exp_files, .x, T)) %>% bind_rows()
  
  
  #files_remove <- list.files(dir_run, full.names = T) %>% str_subset(pattern = ".crp$|.dat$", negate = T)
  
  
  #map(files_remove, ~unlink(.x, recursive = T))
  
  unlink(dir_run, recursive = T)
  file.remove(paste0(calibration_path, cultivar2, ".CRO"))
  
  
  return(mean(metrics_cal$NRMSE))
  
  
  
}
