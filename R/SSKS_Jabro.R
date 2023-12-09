SSKS_Jabro <-
function(S, C, SBDM){
  Si <- 100-C-S
  SSKS_Ja <- exp(9.56 - 0.81*log(Si) - 1.09*log(C) - 4.64*SBDM)
  return(SSKS_Ja*10)
}
