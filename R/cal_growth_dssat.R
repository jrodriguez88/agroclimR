cal_growth_dssat <-
function(x1, x2, params_to_cal, phen_params, calibration_path, cultivar, input_data, exp_files, test_params_model, basedata_path){
    
  # Parameters to  growth calibration 
  
  
  #  x1 <- 83  #"PHINT"
  #  x2 <- 1  #"G3"

  
  test_params_growth  <-  tibble(Parameter = params_to_cal$Parameter,
                                 Set_cal = list(x1, x2)) 
  
  
  
  
  params_to_cal <- test_params_growth  %>% 
    bind_rows(phen_params) %>% right_join(test_params_model, by = "Parameter") %>%
    mutate(to_test = map2(Base, Set_cal, function(x, y) if(is.null(y)){x} else {y}))
  
  
  crop_params_dssat <- params_to_cal$to_test %>% set_names(params_to_cal$Parameter)


##Setting folder

id_run <- as.integer(runif(1) * 10000000)
dir_run <- make_dir_run(calibration_path, id_run)





copy_inputs_dssat(dir_run, basedata_path, crop = "rice")


### Write crop file 
write_crop_dssat(dir_run, cultivar, crop_params_dssat, ecotype = "IB0001")

### Run model dssat

run_model_dssat(dir_run, "rice", exp_files)



sim_data_cal <- read_plantgro(paste0(dir_run, "/PlantGro.OUT"))



metrics_cal <- map(c("dry_matter", "lai"), 
                   ~eval_sim_dssat(input_data, sim_data_cal, .x, T)) %>%  bind_rows() %>% 
  filter(var %in% c("WAGT", "LAI"))


#files_remove <- list.files(dir_run, full.names = T) %>% str_subset(pattern = ".crp$|.dat$", negate = T)


#map(files_remove, ~unlink(.x, recursive = T))

unlink(dir_run, recursive = T)
#  file.remove(paste0(calibration_path, cultivar2, ".CRO"))


return(mean(metrics_cal$NRMSE))

}
