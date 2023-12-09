SPGF_extract <-
function(INPUT_data, max_diff = 4) {
  
  
  ##Load data for each workbook (XLSX)   
  
  ##Extract Spikelets number from YIELD_obs and join with Phenology observations (PHEN_bs)   
  SPIK_by_EXP <- INPUT_data$YIELD_obs %>%
    mutate(SPIK_M2_avg=PAN_M2*GT_PAN,
           SPIK_M2_min=(PAN_M2-PAN_M2_SE)*(GT_PAN-GT_PAN_SE),
           SPIK_M2_max=(PAN_M2+PAN_M2_SE)*(GT_PAN+GT_PAN_SE))%>%
    left_join(INPUT_data$PHEN_obs, by="ID")%>%
    select(ID, contains("SPIK_M2"), IDAT, FDAT)
  
  ##Extract  Total dry weight at panicle initiation or closest sampling date
  WAGT_PI <- INPUT_data$PLANT_gro %>%
    inner_join(SPIK_by_EXP, by="ID") %>%
    mutate(diff_pi=abs(as.integer(difftime(SAMPLING_DATE, IDAT, units = "days"))),
           WAGT_PI=WAGT_OBS, 
           WAGT_PI_SE=WAGT_SE)%>%
    group_by(ID) %>%
    filter(diff_pi==min(diff_pi))%>%
    select(ID, diff_pi, contains("WAGT_PI"))
  
  ##Extract  Total dry weight at flowering initiation or closest sampling date    
  WAGT_FL <- INPUT_data$PLANT_gro %>%
    inner_join(SPIK_by_EXP, by="ID") %>%
    mutate(diff_fl=abs(as.integer(difftime(SAMPLING_DATE, FDAT, units = "days"))),
           WAGT_FL=WAGT_OBS, 
           WAGT_FL_SE=WAGT_SE)%>%
    group_by(ID) %>%
    filter(diff_fl==min(diff_fl))%>%
    select(ID, diff_fl, contains("WAGT_FL"))
  
  ##Join data and compute variables to SPGF calculation  
  SPIK_by_EXP %>%
    left_join(WAGT_PI, by = "ID")%>%left_join(WAGT_FL, by = "ID") %>%
    mutate(diff_pi_fl=(WAGT_FL-WAGT_PI)/10) %>%#(g/m²)
    filter(diff_fl<max_diff, diff_pi<max_diff) %>%
    mutate(LOC_ID=substr(ID, 1,4))
  
}
