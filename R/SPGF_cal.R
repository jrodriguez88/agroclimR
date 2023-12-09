SPGF_cal <-
function(SPGF_df, out="", min = 35000, max = 67000) {
  
  
  SPGF_df <- SPGF_df %>% filter(ID != out)
  
  
  ## Linear model between Spikelet number (number/ m²) and crop growth between panicle initiation and flowering (g/m²)
  lm_spgf <- lm(formula = SPIK_M2_max ~ diff_pi_fl, data = SPGF_df)
  
  ## Print SPGF compute from lm-slope
  spgf <-  sprintf("%.1f", coef(lm_spgf)[[2]]*1000) # Spikelet growth factor (no kg-1)"))
  
  
  
  if (as.numeric(spgf) < min) {
    
    message(paste("SPGF calculated:", spgf, "- is a low value.", "- Use default"))
    return(min)
    
  } else if (as.numeric(spgf) > max){
    
    message(paste("SPGF calculated:", spgf, "- is a high value.", "- Use default"))
    return(max)
    
  } else {
    
    message(paste("SPGF calculated:", spgf, "- is a normal value.", "- Use data"))
    return(as.numeric(spgf))
    
  }
  
}
