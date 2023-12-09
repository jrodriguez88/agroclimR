WCSAT_Saxton <-
function(S,C,OM) {
  
  WCST_i <- 0.278*(S/100) + 0.034*(C/100) + 0.022*OM -
    0.018*(S/100)*OM - 0.027*(C/100)*OM +
    0.584 *(S/100)*(C/100) + 0.078
  
  WCSAT <- WCST_i + (0.6360*WCST_i) - 0.107
  
  return(WCSAT*100)
  
}
