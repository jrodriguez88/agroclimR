SSKS_Saxton <-
function(S, C, OM, SBDM, WCFC=NULL, WCWP=NULL, WCST=NULL){
  if(is.null(WCFC)){
    #        message("Water Content PTF used")
    WCFC <- WCFC_Saxton(S,C,OM)} 
  
  
  
  if(is.null(WCWP)){
    #        message("Water Content PTF used")
    WCWP <- WCWP_Saxton(S,C,OM)}
  if(is.null(WCST)){
    #        message("Water Content PTF used")
    WCST <- WCST_Saxton(SBDM)}
  
  B <- (log(1500) - log(33))/(log(WCFC/100) - log(WCWP/100))  #Eq 15
  alp <- 1/B                #Eq 18
  
  SSKS <- 1930*abs((WCST/100 - WCFC/100))^(3-alp)   # Eq 16
  
  return(SSKS)
  
}
