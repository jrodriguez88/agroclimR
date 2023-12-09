read_wth <-
function(dir_run){
  
  
  file <- paste0(dir_run, "Weather.OUT")
  skip <- read_lines(file)  %>% str_detect("@YEAR") %>% which()-1 
  
  cal_summ <- function(data){
    
    data %>% tibble %>% mutate(across(.fns = as.numeric)) %>%
      summarise(t_max_acu = sum(TMXD), t_min_acu = sum(TMND), srad_acu = sum(SRAD))
    
  }
  
  data_wth <- suppressWarnings(map(skip, ~fread(file, skip = .x))) #%>% 
 #   map(cal_summ)
  
  
  data_wth %>% bind_rows(.id = "scenario")
  
}
