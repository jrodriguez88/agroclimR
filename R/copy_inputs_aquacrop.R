copy_inputs_aquacrop <-
function(path_proj, basedata_path){
  
  # ruta con los archivos necesarios para 
  files_default <- list.files(basedata_path, recursive = T, full.names = T)
  
  file.copy(files_default, path_proj, recursive = T)
  
  
  dir.create(paste0(path_proj, "/OUTP"))
  dir.create(paste0(path_proj, "/SIMUL"))
  dir.create(paste0(path_proj, "/LIST"))
  
  
  file.copy(list.files(path_proj, pattern = "CO2", full.names = T), paste0(path_proj, "/SIMUL/"))
  file.copy(list.files(path_proj, pattern = "DailyResults", full.names = T), paste0(path_proj, "/SIMUL/"))
  file.copy(list.files(path_proj, pattern = "DailyResults", full.names = T), paste0(path_proj, "/SIMUL/"))
  
  
}
