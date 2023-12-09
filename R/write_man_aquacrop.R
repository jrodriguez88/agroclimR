write_man_aquacrop <-
function(path_proj, man_agro = "rice", bund_height = 25, mulches = 0,  fert_stress = 7, weeds = 3){
  
  #Soil bunds, 0.25 m height
  #     6.1       : AquaCrop Version (May 2018)
  #     0         : percentage (%) of ground surface covered by mulches IN growing period
  #    50         : effect (%) of mulches on reduction of soil evaporation
  #     7         : Degree of soil fertility stress (%) - Effect is crop specific
  #     0.25      : height (m) of soil bunds
  #     1         : surface runoff affected or completely prevented by field surface practices
  #     0         : N/A (surface runoff is not affected or completely prevented)
  #     3         : relative cover of weeds at canopy closure (%)
  #     0         : increase of relative cover of weeds in mid-season (+%)
  #    -4.00      : shape factor of the CC expansion function in a weed infested field
  
  
  sink(file = paste(path_proj, paste0(man_agro, ".MAN"), sep = "/"), append = F)
  cat(paste0("Soil bunds, 0.", bund_height, " m height"))
  cat('\n')
  cat("     6.1       : AquaCrop Version (May 2018)", sep = '\n')
  cat(paste0(sprintf("%6.0f", mulches), "         : percentage (%) of ground surface covered by mulches IN growing period"), sep = '\n')
  cat("    50         : effect (%) of mulches on reduction of soil evaporation", sep = '\n')
  cat(paste0(sprintf("%6.0f", fert_stress), "         : Degree of soil fertility stress (%) - Effect is crop specific"), sep = '\n')
  cat(paste0("     0.", bund_height, "      : height (m) of soil bunds"), sep = '\n')
  cat("     1         : surface runoff affected or completely prevented by field surface practices", sep = '\n')
  cat("     0         : N/A (surface runoff is not affected or completely prevented)", sep = '\n')
  cat(paste0(sprintf("%6.0f", weeds), "         : relative cover of weeds at canopy closure (%)"), sep = '\n')
  cat("     0         : increase of relative cover of weeds in mid-season (+%)", sep = '\n')
  cat("    -4.00      : shape factor of the CC expansion function in a weed infested field", sep = '\n')
  sink() 
  
}
