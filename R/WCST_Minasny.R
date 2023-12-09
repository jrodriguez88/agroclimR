WCST_Minasny <-
function(S, SBDM){
  WCST <- 59.9 - (8.78*SBDM) - (0.31*S)
  return(WCST)
}
