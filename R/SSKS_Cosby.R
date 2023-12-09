SSKS_Cosby <-
function(S,C){
  SSKS_Co <- 60.96*10^((-0.6+0.01268*S) - (0.0064*C))
  return(SSKS_Co*10/24)
}
