read_aquacrop_day <-
function(file){
  
  day_file <- read_lines(file)
  
  var_names <- read_lines(file)[[4]] %>% 
    str_replace_all("(?<=[[:alpha:]]) (?=\\d+)", "") %>% 
    str_trim() %>%
    str_split(pattern = "[ ]+") %>%
    flatten_chr() %>% c("Run", .)
  
  find_run <- day_file %>%
    str_detect("Run:") %>%
    which()+2
  
  nlines <- c(find_run[-1], length(day_file))-find_run-2
  
  arg_list <- list(file= file,
                   skip = find_run, 
                   nrows = nlines)
  
  # Read_pmap
  dat <- suppressWarnings(pmap(arg_list, fread)) %>%
    bind_rows(.id = "run") %>%
    setNames(var_names)
  
  return(dat)
  
}
