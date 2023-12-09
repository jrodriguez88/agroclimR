extract_sim_dssat <-
function(sim_data, variable) {
  
  vars <- switch(variable, 
                 dry_matter = c("date", "LWAD", "SWAD", "EWAD",  "CWAD"), 
                 lai = c("date", "LAID"),
                 yield = c("date", "GSTD", "GWAD"), 
                 phen = c("date", "GSTD", "DAP"))
  
  
  
  sim_dat <- sim_data %>% 
    dplyr::select(exp_file, one_of(vars)) #%>% #set_names(exp_set) %>%
  
  
  
  date_to_dae <- function(data) {
    
    edate <- data %>% filter(GSTD == 1) %>% pull(min_dap)
    
    data %>% mutate(value = as.numeric(value - edate)) 
    
  }
  
  sim_select <- switch(variable, 
                       dry_matter = sim_dat %>% 
                         rename(WAGT = CWAD, WLVG = LWAD, WST  = SWAD, WSO = EWAD) %>% 
                         pivot_longer(cols = -c(exp_file, date), names_to = "var") %>%
                         dplyr::select(exp_file, date, var, value), 
                       lai = sim_dat %>%
                         mutate(var = "LAI") %>%
                         dplyr::select(exp_file, date, var, value = LAID),
                       yield = sim_dat %>%
                         dplyr::filter(GSTD > 6) %>%
                         #summarise(date = max(date), value = max(GWAD), .groups = 'drop')  %>%
                         mutate(var = "YIELD", value = GWAD) %>%
                         dplyr::select(exp_file, var, date, value), 
                       phen = sim_dat %>% group_by(exp_file, GSTD) %>% 
                         dplyr::summarise(n = n(), 
                                          min_dap = min(DAP),
                                          max_dap = max(DAP), .groups = 'drop') %>%
                         mutate(var = case_when(
                           GSTD == 1 ~ "EDAT",
                           GSTD == 3 ~ "IDAT",
                           GSTD == 4 ~ "FDAT",
                           GSTD == 20  ~ "MDAT"),
                           value = case_when(
                             var == "EDAT" ~ max_dap,
                             var == "IDAT" ~ min_dap,
                             var == "FDAT" ~ max_dap,
                             var == "MDAT" ~ max_dap)
                         ) %>% 
                         drop_na() %>% 
                         nest(data = -exp_file) %>% 
                         mutate(phen_dae = map(data, date_to_dae)) %>%
                         unnest(phen_dae) %>% dplyr::filter(var != "EDAT") %>%
                         dplyr::select(exp_file, data, var, value))
  
  
  return(dplyr::as_tibble(sim_select))
  
  
}
