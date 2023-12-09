read_summary <-
function(file){
  
 col_names <-  read_lines(file) %>% str_subset("RUNNO") %>%
    str_split_1("\\s+") %>% str_subset("@", negate = T)
  
 summary_out <- fread(file, skip = 4, na.strings = "-99") %>% 
   set_names(col_names) %>% 
    mutate(SDAT = as.Date(as.character(SDAT), format("%Y%j")), 
           PDAT = as.Date(as.character(PDAT), format("%Y%j")), 
           EDAT = as.Date(as.character(EDAT), format("%Y%j")),
           ADAT = as.Date(as.character(ADAT), format("%Y%j")),
           MDAT = as.Date(as.character(MDAT), format("%Y%j")),
           HDAT = as.Date(as.character(HDAT), format("%Y%j"))) %>%
    dplyr::select(SDAT, PDAT, everything())
  
  return(summary_out)


}
