FSTR_cal <-
function(FSTR_df, stat = "mean", min = 0.14, max = 0.24){
  
  FSTR <- bootstrap_param(FSTR_df$FSTR, stat = stat)
  
  
  if (FSTR < min) {
    
    message(paste("FSTR calculated:", FSTR, "- is a low value.", "- Use default"))
    return(min)
    
  } else if (FSTR > max){
    
    message(paste("FSTR calculated:", FSTR, "- is a high value.", "- Use default"))
    return(max)
    
  } else {
    
    message(paste("FSTR calculated:", FSTR, "- is a normal value.", "- Use data"))
    return(FSTR)
    
  }
  
}
