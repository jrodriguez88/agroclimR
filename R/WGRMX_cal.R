WGRMX_cal <-
function(GW1000, min =  0.000019 , max = 0.000040){
  
  wgrmx <- bootstrap_param(GW1000)
  
  #(kg grain-1)
  WGRMX <-  wgrmx[[1]]/(1000000)
  
  if (WGRMX < min) {
    
    message(paste("WGRMX calculated:", WGRMX, "(kg grain-1) - is a low value.", "- Use default"))
    return(min)
    
  } else if (WGRMX > max){
    
    message(paste("WGRMX calculated:", WGRMX, "(kg grain-1) - is a high value.", "- Use default"))
    return(max)
    
  } else {
    
    message(paste("WGRMX calculated:", WGRMX, "(kg grain-1) - is a normal value.", "- Use data"))
    return(WGRMX)
    
  }
  

}
