SSKS_Puckett <-
function(C){
  SSKS_Pu <- 376.7*exp(-0.1975*C)
  return(SSKS_Pu*10/24)
}
