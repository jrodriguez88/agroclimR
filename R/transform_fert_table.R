transform_fert_table <-
function(fert_tb){
  
  
  # AP001    Broadcast, not incorporated            
  # AP002    Broadcast, incorporated
  
  # FE005    Urea
  # FE006    Diammonium phosphate (DAP)     
  # FE028    NPK - urea  
  
  
  base_fert <- fert_tb %>% 
    mutate(FMCD = "FE028",
           FACD = case_when(FERT_No == 1 ~ "AP002",
                            TRUE ~ "AP001"),
           FDEP = case_when(FERT_No == 1 ~ 5,
                            TRUE ~ 1),
           N = case_when(N == 0 ~ -99,
                         TRUE ~ N),
           P = case_when(P == 0 ~ -99,
                         TRUE ~ P),
           K = case_when(K == 0 ~ -99,
                         TRUE ~ K),
           FAMC = -99, FAMO = -99, FOCD = -99, FERNAME = "AgroClimR", F = 1) %>%
    rename(FDATE = DDE, FAMN = N, FAMP = P, FAMK = K) %>% 
    dplyr::select(F, FDATE, FMCD, FACD, FDEP, FAMN, FAMP, FAMK, FAMC, FAMO, FOCD, FERNAME)
  
  
  #*FERTILIZERS (INORGANIC)
  #@F FDATE  FMCD  FACD  FDEP  FAMN  FAMP  FAMK  FAMC  FAMO  FOCD FERNAME
  # 1     1 FE006 AP002     5    10    20   -99   -99   -99   -99 fertApp
  # 1     1 FE005 AP002     5    30   -99   -99   -99   -99   -99 fertApp
  # 1    40 FE005 AP001     1    10    30    10   -99   -99   -99 fertApp    
  
  return(base_fert)
  
  
}
