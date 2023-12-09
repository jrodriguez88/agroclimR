SSKS_Campbell <-
function(S,C){
  SSKS_Ca <- 129.6*exp(-0.07*S - 0.167*C)
  return(SSKS_Ca*10/24)
}
