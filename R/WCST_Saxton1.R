WCST_Saxton1 <-
function(S,C,OM) {
  
  WCFC <- WCFC_Saxton(S,C,OM)    
  WCSAT <- WCSAT_Saxton(S,C,OM)
  WCST <- WCFC + WCSAT -0.097*(S/100) + 0.043
  
  return(WCST)
  
}
