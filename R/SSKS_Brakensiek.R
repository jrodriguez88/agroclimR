SSKS_Brakensiek <-
function(S,C, SBDM, SPOR=NULL) {
  #SPOR = as fraction    
  if(is.null(SPOR)){
    #        message("Porosity was estimated using 2.65g/cm3 as particle density")
    SPOR <- (1-(SBDM/2.65))*100
  }
  SSKS_Br <- 24*exp(19.52348*(SPOR/100) - 8.96847 - 0.028212*C + 0.00018107*(S^2) - 0.0094125*(C^2) - 8.395215*((SPOR/100)^2) + 0.077718*((SPOR/100)*S) - 0.00298*((SPOR/100)^2)*(S^2) -0.019492*((SPOR/100)^2)*(C^2) + 0.0000173*(C*S^2) + 0.02733*((SPOR/100)*C^2) + 0.001434*((SPOR/100)*S^2)- 0.0000035*(S*C^2))
  return(SSKS_Br*10/24)
}
