write_files_dssat <-
function(path_proj, test_data, cultivar, crop = "rice"){


  ### Directorio de salidas (OUTPUTS)
  dir.create(path_proj)

  ##Crear Archivos climaticos
  dir_wth <- paste0(path_proj, "/WTH/")
  dir.create(dir_wth)

  test_data$data %>%
    mutate(path = dir_wth , id_name = site) %>%
    dplyr::select(path, id_name, wth_data = wth, lat, lon, elev) %>%
    mutate(wth_data = map(wth_data, ~.x %>% impute_mean_wth)) %>%
    pmap(., write_wth_dssat)



  #crear archivos suelo
  dir_soil <- paste0(path_proj, "/SOIL/")
  dir.create(dir_soil, showWarnings = T)
  #test_data$data$input_data[[1]]$Metadata %>% View %>%filter(VAR_NAME == "SC")


  #SLB  <- 5          #   Depth, base of layer, cm
  #SBDM <- 1.37       #   Bulk density, moist, g cm-3
  #SCEC <- 15.4       #   Cation exchange capacity, cmol kg-1
  #SDUL <- 0.26       #   Upper limit, drained, cm3 cm-3
  #SLBS <- 0.1        #   Base saturation, cmol kg-1
  #SLCF <- 2.2        #   Coarse fraction (>2 mm), %
  #SLCL <- 26.0       #   Clay (<0.002 mm), %
  #SLHB <- 5.3        #   pH in buffer
  #SLHW <- 6.5        #   pH in water
  #SLLL <- 0.125       #   Lower limit, cm3 cm-3
  #SLMH <- "A1"       #   Master horizon
  #SLNI <- 4.444       #   Total nitrogen, %
  #SLOC <- 2.83       #   Organic carbon, %
  #SLSI <- 26.0       #   Silt (0.05 to 0.002 mm), %
  #SRGF <- 0.988       #   Root growth factor, soil only, 0.0 to 1.0
  #SSAT <- 0.412       #   Upper limit, saturated, cm3 cm-3
  #SSKS <- 7.40       #   Sat. hydraulic conductivity, macropore, cm h-1


  ## transform data to  dssat format
  test_data$data %>% unnest(soil) %>%
    dplyr::select(-c(input_data, wth)) %>%
#    mutate(SOC = case_when(LOC_ID == "YOCS" ~ SOC/5,  # Valor anormal en la base de datos
#                           TRUE ~ SOC)) %>%
    mutate(SLB  = NL*DEPTH,
           #SBDM = SBDM,
           SLOC = SOC/10, # %
           SLNI = (SLON + SNH4 + SNO3)/1000,
           OM = (100/58)*SLOC, # Organic matter (%) = Total organic carbon (%) x 1.72 https://www.soilquality.org.au/factsheets/organic-carbon
           SSKS = pmap_dbl(.l = list(SAND, CLAY, OM, SBDM), SSKS_cal)/10,   #multimodel bootstrapping + from mm/h to  cm/h
           SDUL = WCFC/100,
           SSAT = WCST/100,
           SLLL = WCWP/100,
           SLCF = 1.5) %>%
    rename(id_name = site, SLCL = CLAY, SLHW = PH, SLSI = SILT ) %>%
    dplyr::select(c(id_name, SLB,  SCEC,  SLCF,  SLCL,  SLHW,  SLSI,  SBDM,  SLOC,  SLNI,  SDUL,  SSAT,  SLLL,  SSKS, STC)) %>%
    nest(data = -c(id_name)) %>% mutate(map2(id_name, data, ~write_soil_dssat(dir_soil, .x, .y)))




  #crear archivos experimentales

  #crear archivos experimentales
  dir_exp <- paste0(path_proj, "/EXP/")
  dir.create(dir_exp, showWarnings = T)
  #funcion para remover separadores "_" de las variables a analizar
  remove_unders <- function(var){str_replace_all(var, "_", "")}

  # Crop Phenology
  phen <- test_data$phen
  #plot_phen_obs(phen) #%>% ggplotly()



  #Agronomic data - Plant populations #Fertilization data
  agro_data <- test_data$data$input_data %>% map(~.x[["AGRO_man"]]) %>% bind_rows() %>%
    mutate_at(.vars = vars(LOC_ID, CULTIVAR, PROJECT, TR_N), .funs = remove_unders) %>%
    mutate(id_name = paste0(str_sub(LOC_ID,1,2),
                            str_sub(PROJECT, 1,2),
                            str_sub(year(PDAT), 3,4), 0,
                            str_extract(TR_N, "[0-9]"))) %>%
    mutate(PDAT = as.Date(PDAT), exp_file  = paste(LOC_ID, CULTIVAR, PROJECT, TR_N, sep = "_")) %>%
    left_join(test_data$data$input_data %>% map(~.x[["FERT_obs"]]) %>% bind_rows() %>%
                nest(fert_tb  = -c(ID:CULTIVAR)), by = join_by(ID, LOC_ID, CULTIVAR)) %>%
    mutate(fert_in = map(fert_tb, transform_fert_table)) %>%
    dplyr::select(exp_file, id_name, PDAT:NPLDS, fert_in) #%>% set_names(~tolower(.x))



  # Join data to parameter estimation
  data_param_dssat <- phen %>% dplyr::select(exp_file, data) %>%
    dplyr::distinct() %>% unnest(data) %>% pivot_wider(names_from = var) %>%
    #    mutate(MDAT = map(data, ~.x %>% dplyr::filter(var == "MDAT") %>% pull(value)))  %>%
    #  dplyr::filter(exp_file %in% exp_filter) %>%
    mutate(site = word(exp_file, 1, sep = "_")) %>% dplyr::select(-PDAT) %>%
    left_join(agro_data, by = join_by(exp_file)) %>%
    mutate(PLME = ifelse(ESTAB == "DIRECT-SEED", "S", "T"),
           irri = ifelse(CROP_SYS == "IRRIGATED", T, F),
           PLDS = "R", PLRS = 20, PLRD = 90,  PLDP = 4, path = paste0(path_proj, "EXP/"), crop = crop,
           cultivar = list(c("CROP00", cultivar)))  %>%



    #  PPOP   -- Plant population at seeding, m-2
    #  PPOE   -- Plant population at emergence, m-2
    #  PLME   -- Planting method, code: S Dry seed, P Pregerminated seed, T Transplants
    #  PLDS   -- Planting distribution, row R, broadcast B, hill H
    #  PLRS   -- Row spacing, cm
    #  PLRD   -- Row direction, degrees from N
    #  PLDP   -- Planting depth, cm


  mutate(planting_details = pmap(list(PPOP = NPLDS, PPOE = NPLDS,
                                      PLME = PLME, PLDS = PLDS, PLRS = PLRS, PLRD = PLRD, PLDP = PLDP), list),
         soil = paste0(site, "000001"), start_date = PDAT, treatments_number = 1,
         id_name = map2(id_name, exp_file, ~c(.x, .y ))) %>%
    dplyr::select(path, id_name, crop, cultivar, soil, site, planting_details, irri, fert_in,start_date, PDAT, EDAT, treatments_number) %>%
    mutate(exp = pmap(list(path, id_name, crop, cultivar, soil, site, planting_details, irri, fert_in, start_date, PDAT, EDAT, treatments_number),
                      write_exp_dssat))


  return(dplyr::select(agro_data, exp_file, id_name))

}
