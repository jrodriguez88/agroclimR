SSKS_Wosten99 <-
function(S,C,OM,SBDM, tops=0) {
  Si=100-C-S
  x <- 7.755 + 0.0352*Si + 0.93*tops - 0.967*(SBDM^2)- 0.000484*(C^2) - 0.000322*(Si^2) + 
    0.001/Si - 0.0748/OM - 0.643*log(Si) - 0.01398*SBDM*OM - 0.1673*SBDM*OM + 
    0.02986*tops*C - 0.03305*tops*Si
  
  SSKS_W <- 1.15741*(10^-7)*exp(x)*3600000  #conversion to mm/h
  return(SSKS_W)
}
