extract_sim_aquacrop <-
function(sim_data, exp_set, variable) {
  
  vars <- switch(variable, 
                 dry_matter = c("date", "Stage", "Biomass"), 
                 lai = c("date", "Stage", "CC" ),
                 yield = c("date", "Stage", "YieldPart"), 
                 phen = c("date", "Stage", "DAP"))
  
  
  
  sim_dat <- sim_data %>% map(., ~.x %>% mutate(date = make_date(Year, Month, Day))) %>%
    map(., ~dplyr::select(., one_of(vars))) %>% #set_names(exp_set) %>%
    bind_rows(.id = "exp_file") %>% filter(Stage>0) %>%
    dplyr::filter(exp_file %in% exp_set)
  
  
  date_to_dae <- function(data) {
    
    edate <- data %>% filter(Stage == 1) %>% pull(n)
    
    data %>% mutate(value = as.numeric(value - edate)) 
    
  }
  
  sim_select <- switch(variable, 
                       dry_matter = sim_dat %>% 
                         mutate(var = "WAGT", value = Biomass*1000) %>%
                         dplyr::select(exp_file, date, var, value), 
                       lai = sim_dat %>%
                         mutate(var = "LAI") %>%
                         dplyr::select(exp_file, date, var, value = CC),
                       yield = sim_dat %>%
                         group_by(exp_file) %>%
                         summarise(date = max(date), value = max(YieldPart)*1000, .groups = 'drop')  %>%
                         mutate(var = "YIELD") %>%
                         dplyr::select(exp_file, var, date, value), 
                       phen = sim_dat %>% group_by(exp_file, Stage) %>% 
                         dplyr::summarise(n = n(), 
                                          min_dap = min(DAP),
                                          max_dap = max(DAP), .groups = 'drop') %>%
                         mutate(var = case_when(
                           Stage == 1 ~ "EDAT",
                           Stage == 3 ~ "FDAT",
                           Stage == 4  ~ "MDAT"),
                           value = case_when(
                             var == "EDAT" ~ max_dap,
                             var == "FDAT" ~ max_dap,
                             var == "MDAT" ~ max_dap)
                         ) %>% 
                         drop_na() %>% 
                         nest(data = -exp_file) %>% 
                         mutate(phen_dae = map(data, ~date_to_dae(.x))) %>%
                         unnest(phen_dae) %>% dplyr::filter(value>0) %>%
                         dplyr::select(exp_file, data, var, value))
  
  
  return(dplyr::as_tibble(sim_select))
  
  
}
