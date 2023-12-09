tidy_params_oryza <-
function(input_data, raw_params, method = 1){
  
  
  
  if(method == 1){
    
    
    ### Method 1. According to ORYZA_V3_User_Manual_2014
    ### Apply filters by DVSM 
    BPF_tb <- raw_params$BPART_df %>% 
      pivot_longer(cols = FLV:FSO, names_to = "Partition_Parameter", values_to = "Value") %>%
      mutate(
        #        Growth_phase = case_when(
        #            DVSM<=0.65 ~ "Vegetative",
        #            DVSM>0.65&DVSM<=1 ~"Reproductive",
        #            DVSM>1 ~"Ripening"),
        Partition_Parameter = factor(Partition_Parameter, c("FLV", "FST", "FSO")),
        Value = case_when(
          DVSM>1.1 &  Partition_Parameter == "FLV" ~ 0,
          DVSM>1.1 &  Partition_Parameter == "FST" ~ 0,
          DVSM>1.1 &  Partition_Parameter == "FSO" ~ 1,
          Value<0 ~ 0,
          Value>1 ~ 1,
          TRUE ~ Value))
    
    
  } else {
    
    ### Method 2. Grafical analysis, logical-physiological filters
    BPF_tb <- raw_params$BPART_df %>%
      filter(FLV>=-0.2, FLV<=1.2,
             FST>=-0.2, FST<=1.2,
             FST>=-0.2, FSO<=1.2) %>%
      pivot_longer(cols = FLV:FSO, names_to = "Partition_Parameter", values_to = "Value") %>%
      mutate(
        Partition_Parameter = factor(Partition_Parameter, c("FLV", "FST", "FSO")),
        Value = case_when(
          Partition_Parameter == "FSO" & DVSM>1 & Value<0.30   ~ NA_real_,
          Partition_Parameter == "FLV" & DVSM>1 & Value>0.25   ~ NA_real_,
          Partition_Parameter == "FLV" & DVSM<0.75 & Value<0.2 ~ NA_real_,
          Partition_Parameter == "FLV" & DVSM<0.75 & Value<0.2 ~ NA_real_,
          Partition_Parameter == "FLV" & DVSM<1 & Value>0.75   ~ NA_real_,
          Partition_Parameter == "FST" & DVSM<0.75 & Value<0.4 ~ NA_real_,
          Partition_Parameter == "FST" & DVSM>1 & Value>0.5    ~ NA_real_,
          Partition_Parameter == "FST" & DVSM<1 & Value>0.80   ~ NA_real_,
          Value<0 ~ 0,
          Value>1 ~ 1,
          TRUE ~ Value))
    
    
  }
  
  
  ##### tidy Specific LEaf Area
  SLA_df <- raw_params$LAI_df %>%
    select(exp_file, DVS, LAI) %>% na.omit() %>%
    left_join(raw_params$BMASS_df) %>%
    mutate(SLA = LAI/WLVG, Value = SLA, Parameter = "SLA") %>%
    filter(SLA < 0.0055) %>%
    dplyr::rename(DVSM=DVS)
  
  
  # DVR_plots
  DVR_tb <- raw_params$DVR_df %>%
    pivot_longer(cols = -exp_file, names_to = "DVR_Parameter", values_to = "Value") %>%
    filter(Value < 0.0046) %>% #DVR!= "DVRI", 
    mutate(DVR_Parameter = factor(DVR_Parameter, c("DVRJ", "DVRI", "DVRP", "DVRR"))) 
  
  
  ## Table of leaf death coefficient 
  DRLV_tb <- tibble(raw_params$DRLV_df %>% dplyr::filter(DRLV>0)) %>% 
    mutate(Value = DRLV, Parameter = "DRLV") %>% dplyr::select(exp_file, DRLV, Parameter, DVSM, Value )
  
  # TAble of Fraction of carbohydrates allocated to stems that is stored as reserves
  FSTR_tb <- tibble(raw_params$FSTR_df) %>% dplyr::filter(FSTR > 0.03)
  
  
  
  # Yield params and components
  filt_exp <- DVR_tb$exp_file %>% unique()
  
  id_input_exp <- input_data %>% 
    map(., ~.[["AGRO_man"]]) %>% bind_rows() %>% 
    mutate_at(.vars = vars(LOC_ID, CULTIVAR, PROJECT, TR_N), .funs = function(x) str_replace_all(x, "_", "")) %>%
    mutate(exp_file = paste(LOC_ID, CULTIVAR, PROJECT, TR_N, sep = "_")) %>% 
    dplyr::select(c(ID,	exp_file, LOC_ID,	CULTIVAR))  %>% 
    filter(exp_file %in% filt_exp)
  
  
  YIELD_tb <- id_input_exp %>% 
    left_join(map(input_data, ~.x$YIELD_obs) %>% bind_rows()) %>%
    dplyr::select(exp_file, YIELD_AVG, GW1000, ST_M2, PAN_M2, GT_PAN, GF_PAN)
  
  ## Spikelet growth factor - yield parameters
  SPGF_tb <- id_input_exp %>% 
    left_join(map(input_data, SPGF_extract) %>% bind_rows()) %>% 
    drop_na() %>%
    dplyr::select(exp_file, everything())
  
  
  
  
  
 ###Retorna una lista con las tablas de parametros del modelo ORYZA 
  
  return(list(DVR_tb = DVR_tb, 
              BPF_tb = BPF_tb, 
              SLA_tb = SLA_df, 
              DRLV_tb = DRLV_tb, 
              FSTR_tb = FSTR_tb,
              SPGF_tb = SPGF_tb,
              YIELD_tb = YIELD_tb))
  
  
}
