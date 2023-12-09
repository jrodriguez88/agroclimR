SSKS_Vereecken <-
function(S, C, OM, SBDM) {
  x <- 20.62- 0.96*log(C)- 0.66*log(S) - 0.46*log(OM) - 8.43*SBDM
  SSKS_Ve <- exp(x)*10/24
  
  return(SSKS_Ve)
  
}
