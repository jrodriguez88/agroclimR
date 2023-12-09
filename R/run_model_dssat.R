run_model_dssat <-
function(path, crop, exp_set, dssat_exe = "C:/DSSAT48/", tag = NULL){
  
  # set dir of simulation
  wd <- getwd()
  setwd(path)
  
  #tag <- ifelse(is.null(tag), "", paste0("_", tag))  
  
  # write DSSAT Batch file 
  batch_filename <- paste0(path, "/", "DSSBatch.v48")
  
  xfile <- map(exp_set, ~crop_name_setup(.x, crop)) %>% map_chr("ext")
  
  #treatments_number <- length(exp_set)    # number of escenaries
  
  write_batch_dssat(crop, xfile, batch_filename)
  
  
  
  
  # run DSSAT - OS
#  print(paste('Simulation: ', path, " - ", crop))
  
#  if (Sys.info()['sysname'] == 'Windows'){ 
    
    model <- paste0(crop_name_setup("CIAT0001", crop)[["model"]], 048) 
    system(paste0(dssat_exe, "DSCSM048.EXE " , model," B ", "DSSBatch.v48"), ignore.stdout = F, show.output.on.console = T)
#  }
#  else{
#    system(paste0('dssat ',  path,' B DSSBatch.v48'))
    
#  }
  
  
  setwd(wd)
  
  
}
