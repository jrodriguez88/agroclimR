tidy_soil_oryza <-
function(soil_data, sn = c(3,1,1)){
  
  var_names <- colnames(soil_data)
  if(all(any(c("depth", "DEPTH", "SLB") %in% var_names) & 
         any(c("clay", "CLAY", "C", "SLCL") %in%  var_names) &
         any(c("sand", "SAND", "S", "silt", "SILT", "SLSI", "Si") %in%  var_names) &
         any(c("sbdm", "SBDM", "BD") %in% var_names) &
         any(c("soc", "SOC", "OM") %in% var_names))){
    
    message("Minimun data are available")
    
  } else {stop(message("NO data")) }
  
  
  soil_test %>% rename_with(tolower)
    rename_with(~ stringr::str_replace(.x, 
                                       pattern = c("depth", "DEPTH", "SLB"), 
                                      replacement = "depth")) %>% 
 #   rename_with(~ stringr::str_replace(.x, 
 #                                      pattern = "clay|CLAY|C|SLCL", 
 #                                      replacement = "clay")) %>% 
 #   rename_with(~ stringr::str_replace(.x, 
 #                                      pattern = fixed("sand|SAND|S"), 
 #                                      replacement = "sand")) %>% 
 #   rename_with(~ stringr::str_replace(.x, 
 #                                      pattern = "sbdm|SBDM|BD", 
 #                                      replacement = "sbd")) %>% 
 #   rename_with(~ stringr::str_replace(.x, 
 #                                      pattern = "soc|SOC|OM", 
 #                                      replacement = "soc")) %>% 
 #   rename_with(~ stringr::str_replace(.x, 
 #                                     pattern = fixed("silt|SILT|SLSI|Si"), 
 #                                      replacement = "silt"))
  
  
  data <- soil_data %>%
    mutate(SOC = DEPTH*SBDM*100*SOC/0.58,  #(kg C/ha) https://www.agric.wa.gov.au/measuring-and-assessing-soils/what-soil-organic-carbon
           SON = DEPTH*SBDM*SLNI/10,       #(kg C/ha) 
           SNH4X = DEPTH*SBDM*SNH4/10,      #(kg C/ha) 
           SNO3X = DEPTH*SBDM*SNO3/10)      #(kg C/ha) 
  
  
  list(soil_data, CN)
  
}
