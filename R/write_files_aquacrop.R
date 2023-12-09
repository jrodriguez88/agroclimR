write_files_aquacrop <-
function(path_proj, test_data, cultivar){
  
  
  ### Directorio de salidas (OUTPUTS)
  dir.create(path_proj)
  
  ##Crear Archivos climaticos
  dir_wth <- paste0(path_proj, "/WTH/")
  dir.create(dir_wth)
  
  test_data$data %>% 
    mutate(path = dir_wth , id_name = site) %>%
    dplyr::select(path, id_name, wth = wth, lat, lon, elev) %>%
    mutate(wth = map(wth, ~.x %>% impute_mean_wth)) %>%
    pmap(., write_wth_aquacrop)
  
  
  
  #crear archivos suelo
  dir_soil <- paste0(path_proj, "/SOIL/")
  dir.create(dir_soil, showWarnings = T)
  #test_data$data$input_data[[1]]$Metadata %>% View %>%filter(VAR_NAME == "SC")
  
  ## transform data to aquacrop format
  test_data$data %>% unnest(soil) %>% 
    mutate(SOC = case_when(LOC_ID == "YOCS" ~ SOC/5,
                           TRUE ~ SOC)) %>% 
    mutate(Penetrability = 100, 
           TKL = c(DEPTH/100),
           bdod = SBDM, Gravel = 0,
           OM = (100/58)*SOC/10, # Organic matter (%) = Total organic carbon (%) x 1.72
           SSKS = pmap_dbl(.l = list(SAND, CLAY, OM, SBDM), SSKS_cal)*24,
           CRa = case_when(str_detect(STC, "Sa|LoSa|SaLo") ~ (-0.3112 - SSKS*10^(-5)),
                           str_detect(STC, "Lo|SiLo|Si") ~ (-0.4986 + SSKS*9*10^(-5)),
                           str_detect(STC, "SaCl|SaClLo|ClLo") ~ (-0.5677 - SSKS*4*10^(-5)),
                           str_detect(STC, "SiClLo|SiCl|Cl") ~ (-0.6366 + SSKS*8*10^(-4))),
           CRb = case_when(str_detect(STC, "Sa|LoSa|SaLo") ~ (-1.4936 + 0.2416*log(SSKS)),
                           str_detect(STC, "Lo|SiLo|Si") ~ (-2.1320 + 0.4778*log(SSKS)),
                           str_detect(STC, "SaCl|SaClLo|ClLo") ~ (-3.7189 + 0.5922*log(SSKS)),
                           str_detect(STC, "SiClLo|SiCl|Cl") ~ (-1.9165 + 0.7063*log(SSKS)))) %>%
    dplyr::select(id_name = site, TKL, WCST, WCFC, WCWP, SSKS, Penetrability, Gravel, CRa, CRb, STC) %>%
    setNames(c("id_name", "Thickness", "Sat", "FC", "WP", "Ksat", "Penetrability", "Gravel", "CRa", "CRb", "description")) %>%
    nest(data = -c(id_name)) %>% mutate(map2(id_name, data, ~write_soil_aquacrop(dir_soil, .x, .y)))
  
  
  
  #crear archivos experimentales
  #funcion para remover separadores "_" de las variables a analizar
  remove_unders <- function(var){str_replace_all(var, "_", "")}
  
  # Crop Phenology
  phen <- test_data$phen
  #plot_phen_obs(phen) #%>% ggplotly()
  
  
  
  #Agronomic data - Plant populations
  agro_data <- test_data$data$input_data %>% map(~.x[["AGRO_man"]]) %>% bind_rows() %>%
    mutate_at(.vars = vars(LOC_ID, CULTIVAR, PROJECT, TR_N), .funs = remove_unders) %>%
    mutate(PDAT = as.Date(PDAT), exp_file  = paste(LOC_ID, CULTIVAR, PROJECT, TR_N, sep = "_")) %>%
    dplyr::select(exp_file, PDAT:NPLDS) #%>% set_names(~tolower(.x))
  
  
  
  # Join data to parameter estimation
  data_param_aquacrop <- phen %>% dplyr::select(exp_file, data) %>% 
    dplyr::distinct() %>% 
    mutate(MDAT = map(data, ~.x %>% dplyr::filter(var == "MDAT") %>% pull(value))) %>%
    unnest(MDAT) %>% dplyr::select(-data) %>%
    #  dplyr::filter(exp_file %in% exp_filter) %>%
    mutate(id_name = word(exp_file, 1, sep = "_")) %>% 
    left_join(agro_data, by = join_by(exp_file)) %>% 
    dplyr::select(id_name, exp_file, PDAT, everything()) 
  
  
  
  data_param_aquacrop %>% 
    mutate(exp = pmap(list(path_proj = path_proj,
                           id_name = exp_file,
                           clim_name = id_name,
                           soil_name = id_name,
                           cultivar  = cultivar,
                           sowing_date = PDAT,
                           harvest_date = MDAT,
                           co2_name = "MaunaLoa",
                           irri_name = tolower(CROP_SYS),
                           man_agro = "rice"), write_exp_aquacrop))
  
  
  
  
  # write irrigation file 
  write_irri_aquacrop(path_proj)
  
  
  
  # write agronomic management file
  write_man_aquacrop(path_proj)
  
  
  
  
  
}
