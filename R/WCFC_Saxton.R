WCFC_Saxton <-
function(S,C,OM) {
  WCFC_i <- -0.251*S/100 + 0.195*C/100 + 0.011*OM +
    0.006*(S*OM/100) - 0.027*(C*OM/100) +
    0.452*(S/100)*(C/100) + 0.299
  
  WCFC <- WCFC_i + (1.283*(WCFC_i^2) - (0.374*WCFC_i) - 0.015)
  
  return(WCFC*100)
  
}
