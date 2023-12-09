pN_Saxton <-
function(S,C,OM) {
  WCST <- WCST_Saxton(S,C,OM)
  pN <- (1 - WCST)*2.65
  return(pN)
}
