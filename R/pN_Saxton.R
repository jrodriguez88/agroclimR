pN_Saxton <-
function(S,C,OM) {
  WCST <- WCSAT_Saxton(S,C,OM)
  pN <- (1 - WCST)*2.65
  return(pN)
}
