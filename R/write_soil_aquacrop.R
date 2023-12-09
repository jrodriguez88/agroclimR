write_soil_aquacrop <-
function(path, id_name, soil_data, model_version = 6.1) {
    
    data <- as.data.frame(soil_data)
    
    #CN: Curve number (dimensionless)
    CN <- data[1,] %>% 
      mutate(CN = case_when(Ksat <= 10 ~ 85,
                            Ksat > 10 & Ksat <=50 ~ 80,
                            Ksat > 50 & Ksat <=250 ~ 75,
                            Ksat > 250 ~ 65)) %>% pull(CN)
    
    
    # REW: Readily Evaporable Water (mm)
    REW <- data[1,] %>%
      mutate(REW_cal = (10*(FC - WP/2)*0.04),
             REW = case_when(REW_cal >=15 ~ 15, 
                             REW_cal < 0 ~ 0,
                             TRUE ~ REW_cal)) %>% pull(REW) %>% sprintf("%1.f", .)

    sink(paste0(path, "/", id_name, ".SOL"), F)    
    cat(paste0(id_name, " AquaCrop soil file - by https://github.com/jrodriguez88"))
    cat('\n')
    cat(paste0("        ", model_version,"                 : AquaCrop Version (May 2018)"), sep = "\n")
    cat(paste0("       ", CN, "                   : CN (Curve Number)") , sep = "\n")
    cat(paste0("       ", REW, "                   : Readily evaporable water from top layer (mm)"), sep = "\n")
    cat(paste0("        ", nrow(data), "                   : number of soil horizons") , sep = "\n")
    cat(paste0("       -9                   : variable no longer applicable"), sep = "\n")
    cat(paste0("  Thickness  Sat   FC    WP     Ksat   Penetrability  Gravel  CRa       CRb           description"), sep = "\n")
    cat(paste0("  ---(m)-   ----(vol %)-----  (mm/day)      (%)        (%)    -----------------------------------------"), sep = "\n")
    write.table(data.frame(Thickness    = sprintf("%8.2f", data[["Thickness"]]    ),
                           Sat          = sprintf("%7.1f", data[["Sat"]]          ),
                           FC           = sprintf("%5.1f", data[["FC"]]           ),
                           WP           = sprintf("%5.1f", data[["WP"]]           ),
                           Ksat         = sprintf("%7.1f", data[["Ksat"]]         ),
                           Penetrability= sprintf("%10.0f",data[["Penetrability"]]),
                           Gravel       = sprintf("%9.0f", data[["Gravel"]]       ),
                           CRa          = sprintf("%13.6f",data[["CRa"]]          ),
                           CRb          = sprintf("%9.6f", data[["CRb"]]          ),
                           description  = sprintf("%16s",  data[["description"]]  )),
                row.names = F, quote = F, col.names = F)
    sink()
    
}
