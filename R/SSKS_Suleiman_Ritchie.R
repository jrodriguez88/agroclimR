SSKS_Suleiman_Ritchie <-
function(S, C, OM, SBDM, SPOR=NULL, WCFC=NULL){
  #    stopifnot(SPOR)
  if(is.null(WCFC)){
    message("WCFC was estimated using Saxton-PTF")
    WCFC <- WCFC_Saxton(S,C,OM)}
  
  if(is.null(SPOR)){
    message("Porosity was estimated using 2.65g/cm3 as particle density")
    SPOR <- (1-(SBDM/2.65))*100
  }
  
  SSKS <- 75*((SPOR-WCFC)^2/(WCFC)^2)
  
  return(SSKS*10/24)
  
}
