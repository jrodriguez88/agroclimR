write_batch_dssat <-
function(crop, exp_set, filename = "DSSBatch.v48"){
  
  #treatments_number <- length(exp_set)
  
  
  batchfile <- rbind(
    rbind(
      # Batchfile headers            
      paste0("$BATCH(", toupper(crop), ")"),            
      "!",            
      cbind(sprintf("%6s %89s %6s %6s %6s %6s", "@FILEX", "TRTNO", "RP", "SQ", "OP", 
                    "CO"))),            
    cbind(sprintf("%6s %83s %6i %6i %6i %6i",            
                  paste0(exp_set),
                  1:1,  # Variable for treatment number            
                  1,  # Default value for RP element            
                  0,  # Default value for SQ element            
                  1,  # Default value for OP element            
                  0)))  # Default value for CO element 
  
  # Write the batch file to the selected folder  
  write(batchfile, file = filename, append = F)
  
  
  
}
