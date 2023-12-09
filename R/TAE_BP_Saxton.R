TAE_BP_Saxton <-
function(S,C,OM) {
  WCSAT <- WCSAT_Saxton(S,C,OM)
  TAE_BPi <- -2.27 -(27.93*C/100) -(81.97*WCSAT) + (71.12*S*WCSAT/100) + (8.29*C*WCSAT/100) + (14.05*(S/100)*(C/100)) + 27.16
  
  TAE_BP <- TAE_BPi + (0.02*(TAE_BPi^2) -0.113*TAE_BPi -0.70)
  
  return(TAE_BP)
  
}
