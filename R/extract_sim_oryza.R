extract_sim_oryza <-
function(sim_data, exp_set, variable) {
    
    vars <- switch(variable, 
                   dry_matter = c("date", "DVS", "WAGT", "WLVG", "WST", "WSO", "WLVD"), 
                   lai = c("date", "DVS", "LAI"),
                   yield = c("date", "DVS", "WRR14"), 
                   phen = c("date", "DVS"))
    
    date_to_dae <- function(data) {
        
        edate <- data %>% filter(var == "EDAT") %>% pull(value)
        
        data %>% mutate(value = as.numeric(value - edate)) %>%
            dplyr::filter(var != "PDAT", var != "EDAT")
        
    }
    
    sim_dat <- sim_data %>%
        map(., ~dplyr::select(., one_of(vars))) %>% 
        set_names(str_sub(exp_set, 1, -5)) %>%
        bind_rows(.id = "exp_file") %>% drop_na()
    
    sim_select <- switch(variable, 
                         dry_matter = sim_dat %>% 
                             gather(var, value, -(1:3)), 
                         lai = sim_dat %>%
                             rename(value = LAI) %>%
                             mutate(var = "LAI") %>%
                             dplyr::select(exp_file, date, DVS, var, value),
                         yield = sim_dat %>%
                             group_by(exp_file) %>%
                             summarise(date = max(date), value = max(WRR14)*0.86) %>%  # grain to 0 % moisture
                             mutate(var = "YIELD") %>%
                             dplyr::select(exp_file, var, date, value), 
                         phen = sim_dat %>%
                             mutate(var = case_when(
                                 DVS == 0 ~ "EDAT",
                                 DVS >= 0.649 & DVS <=0.67 ~ "IDAT",
                                 DVS >= 1 & DVS <= 1.2 ~ "FDAT",
                                 DVS >= 2 ~ "MDAT")) %>% 
                             drop_na() %>%
                             nest(data = -c(exp_file, var)) %>%
                             mutate(ref = case_when(var == "EDAT" ~ 0.00,
                                                    var == "IDAT" ~ 0.65,
                                                    var == "FDAT" ~ 1.00,
                                                    var == "MDAT" ~ 2.00),
                                    date = map2_dbl(data, ref, ~.x %>% 
                                                    mutate(diff = DVS - .y) %>%
                                                    dplyr::slice(which.min(diff)) %>%
                                                    pull(date)) %>% as.Date(., origin = "1970-01-01")) %>%
                             dplyr::select(exp_file, var, date) %>% 
                             rename(value = date) %>% 
                             nest(data = -exp_file) %>% 
                             mutate(phen_dae = map(data, ~date_to_dae(.x))) %>%
                             unnest(phen_dae))
                                    
    
    return(dplyr::as_tibble(sim_select))
    
    
}
