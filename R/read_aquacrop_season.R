read_aquacrop_season <-
function(file){
  
  names <- read_lines(file)[[3]] %>%
    str_trim() %>%
    str_split(pattern = "[ ]+") %>%
    flatten_chr() %>% 
    c(., "File")
  
  data <- fread(file, skip = 4) %>%
    setNames(names)
  
  return(data)
  
}
