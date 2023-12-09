SSKS_Rawls <-
function(S,C){
  SST_Rawls <- 0.332 - (7.251*(10^-4)*S) + 0.1276*log10(C)
  SSKS_R <- 24*exp(12.012 - (7.55*(10^-2)*S) + (-3.8950 + (0.03671*S) - (0.1103*C) + 8.7546*(10^-4)*(C^2))*(1/(SST_Rawls)))
  
  return(SSKS_R*10/24)
}
