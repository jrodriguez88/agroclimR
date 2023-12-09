SDB_Minasny <-
function(DEPTH, S, OM, BD_OM=0.224){
  SBDmin <- 0.93 + (0.049*log(DEPTH)) + (0.005*S) + 0.000065*((S-38.96)^2)
  SBD_M <- 100/((OM/BD_OM)+((100-OM)/SBDmin))
  return(SBD_M)
}
