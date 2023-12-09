srad_cal <-
function(data, lat, sh_name =  "sbri", A = 0.29, B = 0.45, kRs = 0.175, fill = F){
  
  stopifnot(require(sirad))
  
  
  if (sh_name %in% colnames(data)){
    
    data <- data %>% 
      mutate(
        extraT = extrat(lubridate::yday(date), radians(lat))$ExtraTerrestrialSolarRadiationDaily, # Calcula la radiacion extraterrestre
        srad = ap(date, lat = lat, lon = lon,    # aqui aplica Angstrom-Prescott
                  extraT, A, B, sbri))
    
    
    
    message("Method for estimate Solar Radiation: Angstrom-Prescott (A-P) Model")
    
  } else if (all(c("tmax", "tmin") %in%  colnames(data))){
    
    data <- mutate(data, sbri = NA_real_)   #Aqui crea la variable brillo por si no existe
    message("Method for estimate Solar Radiation: Hargreaves Model")
    
    data <- data %>% 
      mutate(
        extraT = extrat(lubridate::yday(date), radians(lat))$ExtraTerrestrialSolarRadiationDaily, # Calcula la radiacion extraterrestre
        srad = kRs*sqrt(tmax - tmin)*extraT) ## kRs adjustment coefficient (0.16.. 0.19) -- for interior (kRs = 0.16) and coastal (kRs = 0.19) regions
    
  } else {
    
    message("No data to calculate Solar Radiation!.")
    
  }
  
  
  
  
  
  if (isTRUE(fill)) {
    
    max_srad <- mean(data$extraT)*0.80     # calcula el maximo teorico de radiacion
    
    data <- data %>% 
      mutate(
        srad = if_else(is.na(srad), kRs*sqrt(tmax - tmin)*extraT, srad),
        srad = if_else(srad>max_srad|srad<0|is.na(srad),  median(step1$srad, na.rm = T), srad))
    
    
  }
  
  
  return(pull(data, srad))   # retorna la radiacion en MJ/m2*dia
  
  
}
