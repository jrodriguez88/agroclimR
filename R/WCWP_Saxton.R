WCWP_Saxton <-
function(S, C, OM) {
  WCWP_i <- -0.024*S/100 + 0.487*C/100 + 0.006*OM +
    0.005*(S*OM/100) - 0.013*(C*OM/100) +
    0.068 *(S/100)*(C/100) + 0.031
  
  WCWP <- WCWP_i + (0.14*WCWP_i - 0.02)
  
  return(WCWP*100)             
}
