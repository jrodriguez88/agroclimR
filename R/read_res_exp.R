read_res_exp <-
function (file) {
  res_file <- read_lines(file) #%>%
  #as_tibble() %>% filter(nchar(value)>100)
  
  #res_file %>% 
  #   .[str_detect(., pattern = "[\n /w ]+$")] %>%
  #  str_detect(., pattern = "")
  rescolnames <- res_file %>%
    str_subset("TIME") %>%
    str_split("\t")
  
  #res_file %>% read_table(skip = 24, n_max = 100)
  
  find_tbi <- res_file %>%
    str_detect(pattern ="TIME") %>%
    which()
  
  nlines <- c(find_tbi[-1], length(res_file))-find_tbi
  
  
  # Argument list 
  arg_list <- list(file= file,
                   skip = find_tbi, 
                   nrows = nlines,
                   na.string="-", 
                   col.names= rescolnames)
  
  # Read_pmap
  dat <- suppressWarnings(pmap(arg_list, fread)) %>%
    map(., ~mutate(.,date = lubridate::make_date(YEAR)+DOY-1))
  
  return(dat)
  
}
