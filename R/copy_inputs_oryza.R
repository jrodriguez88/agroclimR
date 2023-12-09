copy_inputs_oryza <-
function(path_proj, basedata_path){
  
  dir_files <- list.files(basedata_path, full.names = T) %>%
    str_subset("ORYZA3|WTH|SOIL|EXP")
  
  file.copy(dir_files, path_proj, recursive = T)
  
  
  # walk2(.x = c(".sol", ".crp", ".exp"), 
  #       .y = paste0("standard", c(".sol", ".crp", ".exp")), 
  #       ~file.rename(
  #         from = list.files(dir_run, pattern = .x, full.names = T), 
  #         to = paste0(dir_run, .y)))
  # 
  
  
}
