read_plantgro <-
function(file) {
  
  # Read PlanstGro.OUT lines
  plangro_raw <- read_lines(file)
  

 # detect start data.frame
  skip <- plangro_raw  %>% str_detect("@YEAR") %>% which()-1 
  
  # experimental names 
  exp_names <- plangro_raw %>% 
    str_subset(fixed("EXPERIMENT")) %>% 
    map_chr(~str_split(., " ") %>% unlist() %>% pluck(-1))
  
 # id_name <- plantgro_raw %>% 
 #   str_subset(fixed("EXPERIMENT")) %>% 
 #   map_chr(~str_split(., " ") %>% unlist() %>% pluck(8))
  

  data_plangro <- suppressWarnings(map(skip, ~fread(file, skip = .x))) %>% 
    set_names(exp_names) %>%
    map(~.x %>% mutate(across(where(is.character), as.numeric))) %>% 
    bind_rows(.id = "exp_file") %>% 
    mutate(date = lubridate::make_date(`@YEAR`)+DOY-1)
    

  
  return(data_plangro)
    
}
