make_dir_run <-
function(dir_run_main, id_run){
  
  
  #  require(stringr)
  dir <- paste0(dir_run_main, id_run, '/')
  dir <- stringr::str_replace(dir, "ñ", "n")
  
  if (!dir.exists(dir)) { 
    
    dir.create(dir, showWarnings = F, recursive = TRUE, mode = "777")
    # system('chmod 777 *.*')
    # paste0(dir_base, region, '/', cultivar,  '/', select_day)
    
  }
  
  return(paste0(dir))
}
