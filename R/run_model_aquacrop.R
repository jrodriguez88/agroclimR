run_model_aquacrop <-
function(path, cultivar, exp_set, tag = NULL, timeout = 60){
  
  # set dir of simulation
  wd <- getwd()
  setwd(path)
  
#  tag <- ifelse(is.null(tag), "", paste0("_", tag))  
  
  ## Write project
  
  
  exp_patt <- paste(exp_set, collapse = "|")
  
  
  remove_exp <- list.files(paste0(path, "/LIST"), full.names = T) %>%
    str_subset(pattern = exp_patt, negate = T)
  
  file.remove(remove_exp)
  
  
#  message(paste("Simulation Projects ", cultivar, " :", path))
  
  system("ACsaV60.exe", timeout = timeout)
  
  
  

  
  
  
  ## Run Aquacrop plugin
  
  
  
  
  
  setwd(wd)
  
}
