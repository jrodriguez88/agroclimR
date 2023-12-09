get_params_oryza <-
function(path, input_data, exp_files, model_curves = "lm", stat){
  
  
  ### evalua si se encuantran las gherramientas necesarias en la carpeta de trabajo 
  ##Descarga y ejecuta aplicativos DRATES.exe y PARAM.exe para obetener parametros de cultico para ORYZA model
  
#  download_ORYZA_Tools(path)
  run_drates_param(exp_files, path)
  
  ### Extract and import params from PARAM.out
  raw_params <- extract_drates_param(exp_files, path)
  tidy_params <- tidy_params_oryza(input_data, raw_params, method =  1)
  

  
  #Development rates data 
  DVR_data <- tidy_params$DVR_tb %>% pivot_wider(names_from = DVR_Parameter, values_from = Value)
  
  ## Selected tables -- Particion de biomasa --- area foliar especifica y tasa de hojas muertas
  BPF_tb <- make_grow_curves(tidy_params$BPF_tb, dvs = c(0, 0.5, 0.75, 1, 1.5, 2.5),  model = model_curves)
  SLA_tb <- make_grow_curves(tidy_params$SLA_tb, dvs = c(0, 0.16, 0.33, 0.65, 0.79, 2.1, 2.5), model = model_curves)
  DRLV_tb <- make_grow_curves(tidy_params$DRLV_tb, dvs = c(0, 0.6, 1, 1.6, 2.1, 2.5), model = model_curves)

  
  

  ##Genera un indice para calcular y filtrar resultados
  metric <- switch (stat,
                    "mean" = 1,
                    "min" = 2,
                    "max" = 3
  )
  
  #Function to copy partitioning tables in correct format  ---inputs from tidy_params  
  paste_crp <- function(tidy_tb, param, metric){
    
    
    tidy_tb[[metric]] %>%
      spread(Partition_Parameter, Value) %>%
      select(DVS, all_of(param))
    
  }
  

 ### Genera una lista d eparametros con base en la informacion ingresada en input data y extraida en la parametrizacion  
  
 param_list <- list(  
  
#Development rate in juvenile phase                
  
  DVRJ = bootstrap_param(DVR_data$DVRJ, stat = stat),
  
  
#Development rate in photoperiod-sensitive phase    
  
  DVRI = bootstrap_param(DVR_data$DVRI, stat = stat),
  
  
#Development rate in panicle development               
  
  DVRP = bootstrap_param(DVR_data$DVRP, stat = stat),
  
  
#Development rate in reproductive phase   
  
  DVRR = bootstrap_param(DVR_data$DVRR, stat = stat),
  
  
#Maximum relative growth rate of leaf area   
  
#  RGRLMX 
  
  
#Minimum relative growth rate of leaf area  
  
#  RGRLMN
  
  
#Maximum value of SLA   
  
  SLAMAX = SLA_max(tidy_params$SLA_tb),
  
#Give SLA as a function of DVS in the table SLATB
  
  SLATB = SLA_tb[[metric]],
  
  
#Fraction of carbohydrates allocated to stems that is stored as reserves  
  
  FSTR = FSTR_cal(tidy_params$FSTR_tb, stat = stat),
  
  
  
  
#Spikelet growth factor  
  
  SPGF = SPGF_cal_safe(tidy_params$SPGF_tb),
  
  
#Maximum individual grain weight   
  
  WGRMX  = WGRMX_cal(tidy_params$YIELD_tb$GW1000),
  
  
#Table of fraction total dry matter partitioned to the shoot  
  
#  FSHTB = 
  
  
#Table of fraction shoot dry matter partitioned to the leaves  
  
  FLVTB = paste_crp(BPF_tb, "FLV", metric),
  
  
#Table of fraction shoot dry matter partitioned to the stems  
  
  FSTTB = paste_crp(BPF_tb, "FST", metric),
  
  
#Table of fraction shoot dry matter partitioned to the panicles  
  
  FSOTB = paste_crp(BPF_tb, "FSO", metric),
  
  
#Table of leaf death coefficient 
  
  DRLVT = DRLV_tb[[metric]]
  
  
#Maximum depth of roots if drought   
  
#  ZRTMCD
  
  
#Upper limit leaf expansion  
  
#  ULLE
  
  
#Lower limit leaf expansion  
  
#  LLLE
  
  
#Ratio of remaining available water to total water supply capability - transpiration eq  
  
#  FSWTD
  
  
#The threshold temperature for cold caused sterility   
  
#  COLDREP
  
  
#The threshold temperature for heat caused sterility   
  
#  CTSTER
  
)
  
  
  
  
 ##Retorna lista de parametros para ingresar a archivo de cultivo 
  
 return(param_list) 
  
  
  
  
}
