write_crop_dssat <-
function(path, cultivar, crop_params_dssat, ecotype = "IB0001", id_var = "CROP00", new_file = T){
  
  
  
  
  #write ´*.SPE´ file
  
  sink(paste0(path, '/RICER048.SPE'), append = F)
  
  
  cat("*RICE SPECIES COEFFICIENTS: RICER048 MODEL - by https://github.com/jrodriguez88", sep = "\n")
  cat("\n")
  cat("*CHARACTERISTICS", sep = "\n")
  cat("@C X(EN) Y(CH)  YSTD", sep = "\n")
  cat("RI OPT   SHME      1     ! Shock calculation method (1-standard, 2-Salaam)", sep = "\n")
  cat("RI OPT   SHFC    1.0     ! Shock factor", sep = "\n")
  cat("!RI OPT   PHIN   83.0     ! Phyllochron interval", sep = "\n")
  cat("RI OPT   CO2X      0  220  330  440  550  660  770  880  990 9999  ! CO2 X axis", sep = "\n")
  cat("RI OPT   CO2Y   0.00 0.71 1.00 1.08 1.17 1.25 1.32 1.38 1.43 1.50  ! CO2 Y axis", sep = "\n")
  cat("RI OPT   RWEP   1.50", sep = "\n")
  cat("RI OPT   PORM   0.00                   ! Minimum pore space", sep = "\n")
  cat("RI OPT   RWMX   0.03                   ! Max root water uptake", sep = "\n")
  cat("RI OPT   RLWR   1.05                   ! Root length weight ratio", sep = "\n")
  
    
    
 sink()
  
  

  
  
#write_cul <- function(matrix_cul, out_dir){
  
  # matrix_cul <- x
 
 if(isTRUE(new_file)){
   sink(paste0(path, '/RICER048.CUL'), append = F)
   
   cat("*RICE CULTIVAR COEFFICIENTS: RICER048 MODEL - by https://github.com/jrodriguez88", sep = "\n")
   cat("\n")
   
   cat("@VAR#  VAR-NAME........ EXPNO   ECO#    P1   P2R    P5   P2O    G1    G2    G3 PHINT  THOT TCLDP TCLDF")
   # 
   cat("\n")
   
 } else {
   
   sink(paste0(path, '/RICER048.CUL'), append = T)
   cat("\n")
   
 }


  cat(paste(sprintf("%6s", id_var),
            sprintf("%-16s", cultivar), 
            sprintf("%5s", '.'),
            sprintf("%6s", ecotype),
            sprintf("%5.1f", crop_params_dssat$P1),
            sprintf("%5.1f", crop_params_dssat$P2R),
            sprintf("%5.1f", crop_params_dssat$P5),
            sprintf("%5.1f", crop_params_dssat$P2O),
            sprintf("%5.1f", crop_params_dssat$G1), 
            sprintf(".0%3.0f", crop_params_dssat$G2*10000),
            sprintf("%5.2f", crop_params_dssat$G3),
            sprintf("%5.1f", crop_params_dssat$PHINT),
            sprintf("%5.1f", crop_params_dssat$THOT),
            sprintf("%5.1f", crop_params_dssat$TCLDP),
            sprintf("%5.1f", 15)))
  sink()
}
