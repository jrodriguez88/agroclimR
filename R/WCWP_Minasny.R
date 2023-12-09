WCWP_Minasny <-
function(C, OM){
  WCWP <- 7.95 + (0.86*OM) + (0.4*C) - 0.004*((C-37.7)^2)
  return(WCWP)
}
