write_irri_aquacrop <-
function(path_proj, irri_name = "irrigated", depletion = 10) {
  
  #Net irrigation requirement (allowable depletion 10 % RAW)
  #   6.1   : AquaCrop Version (May 2018)
  #   1     : Sprinkler irrigation
  # 100     : Percentage of soil surface wetted by irrigation
  #   3     : Determination of Net Irrigation requirement
  #  20     : Allowable depletion of RAW (%)
  
  
  sink(file = paste(path_proj, paste0(irri_name, ".IRR"), sep = "/"), append = F)
  cat(paste0("Net irrigation requirement (allowable depletion ", depletion, " % RAW)"))
  cat('\n')
  cat("   6.1   : AquaCrop Version (May 2018)", sep = '\n')
  cat("   1     : Sprinkler irrigation", sep = '\n')
  cat(" 100     : Percentage of soil surface wetted by irrigation", sep = '\n')
  cat("   3     : Determination of Net Irrigation requirement", sep = '\n')
  cat(paste0("  ", depletion, "     : Allowable depletion of RAW (%)"), sep = '\n')
  sink()   
  
  
  
}
