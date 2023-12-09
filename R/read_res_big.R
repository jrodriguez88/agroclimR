read_res_big <-
function(path, res_filename){
  stopifnot(require(tidyverse))
  
  res_file <- read_lines(paste0(path, "/",  res_filename))
  
  res_colnames <- res_file %>%
    str_subset("TIME") %>%
    str_split("\t") %>%
    .[[1]]
  
  res_file %>%
    tibble() %>%
    filter(!str_detect(.,pattern = "^[ \t]*$|^[*]"), 
           !str_detect(.,pattern = "WARNING|Please check"))%>%
    mutate(id=cumsum(str_detect(.,"TIME"))) %>%
    filter(!str_detect(.,"TIME")) %>%
    separate(data=., col = ., into =  res_colnames, sep = '[\\t]') %>%
    mutate_if(is.character, as.numeric)
  
}
