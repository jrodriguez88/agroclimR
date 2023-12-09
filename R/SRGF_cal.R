SRGF_cal <-
function(z, zmax, wcg){
  
  
  SRGF_ <- ifelse(z <=15 , 1, (1 - (z/zmax))^wcg)
  
  return(ifelse(SRGF_ < 0, 0, SRGF_))
  
  
}
