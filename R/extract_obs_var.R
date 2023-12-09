extract_obs_var <-
function(obs_data, variable, model = "oryza") {
  
  # vars select shet names required
  vars <- switch(variable, 
                 dry_matter = "PLANT_gro", 
                 lai = "PLANT_gro",
                 yield = "YIELD_obs", 
                 phen = "PHEN_obs")
  
  date_to_dae <- function(data) {
    
    edate <- data %>% filter(var == "EDAT") %>% pull(value)
    
    data %>% mutate(value = as.numeric(value - edate)) %>%
      dplyr::filter(var != "PDAT", var != "EDAT")
    
  }
  
  #   set <- exp_set %>%
  #       str_sub(1,-5) %>% enframe(name = NULL, value = "exp_file") %>%
  #       separate(exp_file, c("LOC_ID", "CULTIVAR","PROJECT", "TR_N"), sep = "_", remove = F) %>%
  #       mutate(ID = paste0(LOC_ID, TR_N, PROJECT))
  
  remove_unders <- function(var){str_replace_all(var, "_", "")}
  
  set <- obs_data %>% 
    map(., ~.[["AGRO_man"]]) %>% bind_rows() %>% 
    mutate_at(.vars = vars(LOC_ID, CULTIVAR, PROJECT, TR_N), .funs = remove_unders) %>%
    mutate(exp_file = paste(LOC_ID, CULTIVAR, PROJECT, TR_N, sep = "_")) %>% 
    dplyr::select(c(ID,	exp_file, LOC_ID,	PROJECT,	CULTIVAR,	TR_N))
  
  
  
  
  
  obs_data2 <- obs_data %>%
    map(., ~.[[vars]]) %>%
    bind_rows() %>% 
    dplyr::select(-LOC_ID, -CULTIVAR) %>%
    nest(data = -c(ID)) %>% right_join(set, by= "ID") %>% unnest(data) %>%
    select(-c(LOC_ID, CULTIVAR, PROJECT, TR_N)) 
  
  
  
  
  
  
  op <- switch(variable, 
               dry_matter = obs_data2 %>%
                 mutate(SAMPLING_DATE =  as.Date(SAMPLING_DATE)) %>%
                 rename(date = SAMPLING_DATE) %>%
                 select(ID:WAGT_SE, exp_file, -contains("LAI")) %>% 
                 gather(var, value, -c(ID, exp_file, date)) %>%
                 separate(var, c("var", "metric"), sep = "_") %>%
                 spread(metric, value) %>% 
                 rename(value = OBS, se = SE) %>% 
                 dplyr::select(exp_file, date, var, value, se), 
               lai = obs_data2 %>%
                 mutate(SAMPLING_DATE =  as.Date(SAMPLING_DATE)) %>% 
                 rename(date=SAMPLING_DATE) %>%
                 select(exp_file, date, contains("LAI")) %>% 
                 mutate(var = "LAI") %>%
                 rename(value=LAI_OBS, se=LAI_SE) %>%
                 dplyr::select(exp_file, date, var, value, se),
               yield = obs_data2 %>%
                 dplyr::select(exp_file, YIELD_AVG, YIELD_MIN, YIELD_MAX) %>%
                 rename(value = YIELD_AVG, ymin = YIELD_MIN, ymax = YIELD_MAX) %>%
                 mutate(var = "YIELD", diff_min = value - ymin,
                        diff_max = ymax - value,
                        se = (diff_max+diff_min)/2) %>%
                 dplyr::select(exp_file, var, value, se),
               phen = obs_data2 %>% 
                 dplyr::select(-ID) %>%
                 gather(var, value, -exp_file) %>%
                 mutate(value = as.Date(value)) %>%
                 nest(data = -exp_file) %>% 
                 mutate(phen_dae = map(data, ~date_to_dae(.x))) %>%
                 unnest(phen_dae) %>%
                 mutate(value = if_else(var == "MDAT", value - 7 , value)))
  
  
  
  if(all(variable == "lai", model == "aquacrop")){
    
    phen_data <- obs_data %>%
      map(., ~.[["PHEN_obs"]]) %>%
      bind_rows() %>% left_join(set, by = join_by(ID, LOC_ID, CULTIVAR)) %>% 
      dplyr::select(exp_file, IDAT, FDAT) %>% mutate(across(contains("DAT"), as.Date))
    
    
    
    # canopy cover data.frame
    # Convert CC = 1 - exp(-k*LAI))
    
    op <- op %>% na.omit() %>% nest(data = -exp_file) %>% left_join(phen_data, by = join_by(exp_file)) %>%
      mutate(
        data = pmap(list(data, IDAT, FDAT), function(a,b,c){
          a %>% 
            mutate(
              k = case_when(
                date <= b ~ 0.4,
                date >= c ~ 0.6,
                TRUE ~ 0.5),
              canopy = (1 - exp(-k*value))*100,
              se = canopy*(se/value))})) %>% unnest(data) %>%
      dplyr::select(exp_file, date, var, value = canopy, se)
    
  }
  
  
  
  
  return(op)
  
  
}
