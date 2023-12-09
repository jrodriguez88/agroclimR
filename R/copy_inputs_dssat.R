copy_inputs_dssat <-
function(path_proj, basedata_path, crop = "rice"){
  
  CR <- crop_name_setup("CIAT0001", crop)[["CR"]]
  
  
  #  gen_files <- list.files(basedata_path, full.names = T, pattern = "ECO|SPE|CUL") %>%
  #   str_subset(CR)
  
  wth_files <- list.files(paste0(basedata_path, "/WTH"), full.names = T, pattern = paste0(".WTH"))
  
  
  exp_files <- list.files(paste0(basedata_path, "/EXP"), full.names = T, pattern = paste0(".", CR, "X$"))
  
  soil_files <- list.files(paste0(basedata_path, "/SOIL"), full.names = T, pattern = ".SOL$") %>% 
    map(~read_lines(.)[-1]) %>% unlist() 
  
  sink(file = paste0(path_proj, "/SOIL.SOL"), append = F)
  cat("*SOILS: AgroclimR DSSAT Soil Input File - by https://github.com/jrodriguez88/agroclimR", sep = "\n")
  cat("\n")
  writeLines(soil_files)
  sink()
  
  # Copy files in folder project
  file.copy(c(wth_files, exp_files), path_proj)
  
  #  map2(.x = c("*.SPE", "*.ECO", "*.CUL"), 
  #       .y = paste0("standard", c("*.SPE", "*.ECO", "*.CUL")), 
  #       ~file.rename(
  #         from = list.files(dir_run, pattern = .x, full.names = T), 
  #         to = paste0(dir_run, .y)))
  
  
}
