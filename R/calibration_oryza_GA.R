calibration_oryza_GA <-
function(calibration_path, cultivar, input_data, exp_files, test_params_oryza, basedata_path, 
                                 cal_stages = c("phen", "dry_matter_lai", "yield"), 
                                 pop_iter = c(20, 5),  res_var = c("yield"), ncores = 4){
  
  
  message("This process can take some time. Go for coffee :V")
  
  
  # plan(multiprocess)
  registerDoFuture()
  cl <- makeCluster(ncores)
  plan(future::cluster, workers = cl)
  
  
  safe_bind <- purrr::possibly(bind_rows, otherwise = NULL) 
  
  
  ##default list of parameters - based on IR72 and IR64 -- max an min == +/- 30%
  default_list <- tidy_to_write_crop(NULL)
  phen_pattern <- "DVRJ|DVRI|DVRP|DVRR"
  growth_pattern <- "FLVTB|FSTTB|FSOTB|SLATB|FSHTB|DRLVT|RGRLMX|RGRLMN|SLAMAX|FSTR"
  yield_pattern <- "SPGF|WGRMX|ZRTMCD|ULLE|LLLE|FSWTD|COLDREP|CTSTER"
  
  
  ### Separa tamaño de la poblacion, maximo de iteraciones y numero de escenarios a simular en tablas de particion
  pop_size <- pop_iter[1]
  max_iter <- pop_iter[2]
  n_escenarios <- pop_size*5 
  if(n_escenarios>10000){n_escenarios <- 10000}
  
  
  ### Posibles configuraciones de calibracion 
  if(all(cal_stages %in% c("global"))) {
    
    ### all parameters of oryza
    test_params_global <- tidy_to_write_crop(test_params_oryza)
    
    global_to_cal <- generate_combinations_paramsTb(test_params_global, default_list, length_escenaries =  n_escenarios)
    
    # plan(multiprocess)
#    registerDoFuture()
#    cl <- makeCluster(ncores)
#    plan(future::cluster, workers = cl)
    
    low_min1 <- global_to_cal$Min %>% unlist()
    upp_max1 <- global_to_cal$Max %>% unlist()
    names_par1 <- global_to_cal$Parameter %>% unlist()
    
    
    tic("Global calibration")
    GA_oryza <<- ga(type = "real-valued", 
                    fitness = function(x) -cal_oryza_global(x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8],x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19], x[20],
                                                            global_to_cal, calibration_path, cultivar, 
                                                            input_data, exp_files, default_list, basedata_path, res_var = res_var),
                    lower = low_min1, 
                    upper = upp_max1, 
                    maxiter = max_iter,
                    popSize = pop_size,
                    pmutation = 0.2,
                    parallel = cl, 
                    names = names_par1)
    
    GA_oryza@solution
    toc()
    
    closeAllConnections()
    gc()
    
    
    oryza_paramsGA <- as.data.frame(GA_oryza@solution) %>% sample_n(1) %>%
      pivot_longer(cols = everything(), values_to = "Set_cal", names_to = "Parameter") #%>%
    #mutate(Set_cal =  map(Set_cal, ~.x))
    
    gparams <- list(
      
      BFT = global_to_cal %>% dplyr::filter(Parameter == "BFTB") %>% pull(tables) %>% pluck(1) %>%
        dplyr::filter(id == as.integer(filter(oryza_paramsGA, Parameter == "BFTB") %>% pull(Set_cal))) %>% 
        pull(data) %>% pluck(1) %>% enframe(name = "Parameter", value = "Set_cal"),
      
      other_t = map(c("SLATB", "FSHTB", "DRLVT"),  ~dplyr::filter(global_to_cal, Parameter == .x) %>% pull(tables) %>% pluck(1) %>%
                      dplyr::filter( id == as.integer(filter(oryza_paramsGA, Parameter == .x) %>% pull(Set_cal))) %>% 
                      pull(data) %>% pluck(1)  %>% mutate(Parameter = .x) %>% nest(Set_cal = -Parameter)) %>% bind_rows(),
      
      other_p = tibble(Parameter = global_to_cal$Parameter,
                       Set_cal = oryza_paramsGA$Set_cal) %>% slice(-c(1:4)) %>% mutate(Set_cal =  map(Set_cal, ~.x)))
    
    
    
    
    
    global_params <<-  safe_bind(gparams)
    
    
    #closeAllConnections()
    message("GA - Global calibration done!")
    return(list(params = global_params, GA_global = GA_oryza))
    
    message("Parameter Optimization Done!")
    

    }
  else if(all(cal_stages %in% c("phen"))) {
    
    ## Filtrar los parametros a Calibrar --- Debe contener las columnas Base, Min y Max
    params_to_cal <- test_params_oryza %>% dplyr::filter(str_detect(Parameter, "DVRJ|DVRI|DVRP|DVRR"))
    
    #1. Phenological development parameters 
    tic(paste0("Phenology Calibration"))
    GA_phen <- ga(type = "real-valued", fitness = function(x) -cal_phen_oryza(x[1], x[2], x[3], x[4], params_to_cal, calibration_path, cultivar, input_data, exp_files, test_params_oryza, basedata_path),
                  lower = params_to_cal$Min %>% unlist(), upper = params_to_cal$Max %>% unlist(), maxiter = max_iter,
                  popSize = pop_size,
                  pmutation = 0.2,
                  parallel = cl, 
                  names = params_to_cal$Parameter %>% unlist())
    
    GA_phen@solution
    toc()
    
    gc()
    Sys.sleep(5)
    
    phen_params <<- as.data.frame(GA_phen@solution) %>% sample_n(1) %>%
      pivot_longer(cols = everything(), values_to = "Set_cal", names_to = "Parameter") %>%
      mutate(Set_cal =  map(Set_cal, ~.x))
    
    
    closeAllConnections()
    
    return(list(params = phen_params, GA_phen = GA_phen))
    
    
    
    
    
  
    
    } 
  else if(all(cal_stages %in% c("phen", "yield"))) {
    
    params_to_cal <- test_params_oryza %>% dplyr::filter(str_detect(Parameter, phen_pattern))
    
    
    #1. Phenological development parameters 
    tic(paste0("Phenology Calibration"))
    GA_phen <- ga(type = "real-valued", fitness = function(x) -cal_phen_oryza(x[1], x[2], x[3], x[4], params_to_cal, calibration_path, cultivar, input_data, exp_files, test_params_oryza, basedata_path),
                  lower = params_to_cal$Min %>% unlist(), upper = params_to_cal$Max %>% unlist(), maxiter = max_iter,
                  popSize = pop_size,
                  pmutation = 0.2,
                  parallel = cl, 
                  names = params_to_cal$Parameter %>% unlist())
    
    GA_phen@solution
    toc()
    
    gc()
    Sys.sleep(5)
    
    phen_params <- as.data.frame(GA_phen@solution) %>% sample_n(1) %>%
      pivot_longer(cols = everything(), values_to = "Set_cal", names_to = "Parameter") %>%
      mutate(Set_cal =  map(Set_cal, ~.x))
    
    
    #x1 = 64900      #"SPGF"  
    #x2 = 0.0000249  #"WGRMX"  
    #x3 = 0.4        #"ZRTMCD"  
    #x4 = 1.45       #"ULLE"   
    #x5 = 1404       #"LLLE"   
    #x6 = 0.4        #"FSWTD"    
    #x7 = 21         #"COLDREP"
    #x8 = 36.5       #"CTSTER"   
    
    test_params_model <- phen_params %>% right_join(test_params_oryza, by = "Parameter") %>%
      mutate(Base = map2(Base, Set_cal, function(x, y) if(is.null(y)){x} else {y})) %>% dplyr::select(-Set_cal)
    growth_params <- test_params_model %>% filter(str_detect(Parameter, growth_pattern)) %>% 
      dplyr::select(Parameter, Set_cal = Base)
    params_to_cal <- test_params_model %>% filter(str_detect(Parameter, yield_pattern))
    
    
    
    ## Yield
    #9. Temperature and drought stress parameters
    
    tic("Yield calibration + stress parameters")
    GA_yield <- ga(type = "real-valued", 
                   fitness = function(x) -cal_yield_oryza(x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8],
                                                          params_to_cal, phen_params, growth_params, calibration_path, cultivar, 
                                                          input_data, exp_files, test_params_model, basedata_path),
                   lower = params_to_cal$Min %>% unlist(), 
                   upper = params_to_cal$Max %>% unlist(), 
                   maxiter = max_iter,
                   popSize = pop_size,
                   pmutation = 0.2,
                   parallel = cl, 
                   names = params_to_cal$Parameter %>% unlist())
    
    GA_yield@solution
    toc()
    
    gc()
    Sys.sleep(5)
    
    yield_params <- as.data.frame(GA_yield@solution) %>% sample_n(1) %>%
      pivot_longer(cols = everything(), values_to = "Set_cal", names_to = "Parameter") %>%
      mutate(Set_cal =  map(Set_cal, ~.x))
    
#    load("calibracion_rendimiento_test.RData")
    
    
    phen_yield_params <- bind_rows(phen_params, yield_params) # %>% deframe()
    
    closeAllConnections()
    
    return(list(params = phen_yield_params, GA_phen = GA_phen, GA_yield = GA_yield))
    
    
    
    
    
    
    
    
  
    
    
    
    
    } 
  else if(all(cal_stages %in% c("phen", "global"))) {
    
    params_to_cal <- test_params_oryza %>% dplyr::filter(str_detect(Parameter, phen_pattern))
    
    
    #1. Phenological development parameters 
    tic(paste0("Phenology Calibration"))
    GA_phen <- ga(type = "real-valued", fitness = function(x) -cal_phen_oryza(x[1], x[2], x[3], x[4], params_to_cal, calibration_path, cultivar, input_data, exp_files, test_params_oryza, basedata_path),
                  lower = params_to_cal$Min %>% unlist(), upper = params_to_cal$Max %>% unlist(), maxiter = max_iter,
                  popSize = pop_size,
                  pmutation = 0.2,
                  parallel = cl, 
                  names = params_to_cal$Parameter %>% unlist())
    
    GA_phen@solution
    toc()
    
    gc()
    Sys.sleep(5)
    
    phen_params <- as.data.frame(GA_phen@solution) %>% sample_n(1) %>%
      pivot_longer(cols = everything(), values_to = "Set_cal", names_to = "Parameter") %>%
      mutate(Set_cal =  map(Set_cal, ~.x))
    
    
    #x1 = 64900      #"SPGF"  
    #x2 = 0.0000249  #"WGRMX"  
    #x3 = 0.4        #"ZRTMCD"  
    #x4 = 1.45       #"ULLE"   
    #x5 = 1404       #"LLLE"   
    #x6 = 0.4        #"FSWTD"    
    #x7 = 21         #"COLDREP"
    #x8 = 36.5       #"CTSTER"   
    
    test_params_model <- phen_params %>% right_join(test_params_oryza, by = "Parameter") %>%
      mutate(Base = map2(Base, Set_cal, function(x, y) if(is.null(y)){x} else {y})) %>% dplyr::select(-Set_cal)
    # growth_params <- test_params_model %>% filter(str_detect(Parameter, growth_pattern)) %>% 
    #   dplyr::select(Parameter, Set_cal = Base)
    params_to_cal <- generate_combinations_paramsTb(test_params_model, default_list, length_escenaries =  n_escenarios)
    
    
    tic("Global calibration")
    GA_oryza <- ga(type = "real-valued", 
                   fitness = function(x) -cal_oryza_global(x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8],x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19], x[20],
                                                           params_to_cal, calibration_path, cultivar, 
                                                           input_data, exp_files, default_list, basedata_path, res_var = res_var),
                   lower = params_to_cal$Min %>% unlist(), 
                   upper = params_to_cal$Max %>% unlist(), 
                   maxiter = max_iter,
                   popSize = pop_size,
                   pmutation = 0.2,
                   parallel = cl, 
                   names = params_to_cal$Parameter %>% unlist())
    
    GA_oryza@solution
    toc()
    
    gc()
    Sys.sleep(5)
    
    
    oryza_paramsGA <- as.data.frame(GA_oryza@solution) %>% sample_n(1) %>%
      pivot_longer(cols = everything(), values_to = "Set_cal", names_to = "Parameter") #%>%
    #mutate(Set_cal =  map(Set_cal, ~.x))
    
    oryza_params <-    bind_rows(
      
      params_to_cal %>% dplyr::filter(Parameter == "BFTB") %>% pull(tables) %>% pluck(1) %>%
        dplyr::filter( id == as.integer(filter(oryza_paramsGA, Parameter == "BFTB") %>% pull(Set_cal))) %>% 
        pull(data) %>% pluck(1) %>% enframe(name = "Parameter", value = "Set_cal"),
      
      map(c("SLATB", "FSHTB", "DRLVT"),  ~dplyr::filter(params_to_cal, Parameter == .x) %>% pull(tables) %>% pluck(1) %>%
            dplyr::filter( id == as.integer(filter(oryza_paramsGA, Parameter == .x) %>% pull(Set_cal))) %>% 
            pull(data) %>% pluck(1)  %>% mutate(Parameter = .x) %>% nest(Set_cal = -Parameter)) %>% bind_rows(), 
      
      tibble(Parameter = params_to_cal$Parameter,
             Set_cal = oryza_paramsGA$Set_cal) %>% slice(-c(1:4)) %>% 
        mutate(Set_cal =  map(Set_cal, ~.x)))
    
    closeAllConnections()
    
    return(list(params = oryza_params, GA_phen = GA_phen, GA_global = GA_oryza))
    
    
    
  
    
    
    
    } 
  else if(all(cal_stages %in% c("phen", "dry_matter_lai", "yield"))) {
    
    
    message("ORYZA v3.0 - Genetic Algorithm
            
            - Parameter Optimization - 3 stage:
            ")
    
    ##default list of parameters - based on IR72 and IR64 -- max an min == +/- 30%
    default_list <- tidy_to_write_crop(NULL)
    phen_pattern <- "DVRJ|DVRI|DVRP|DVRR"
    growth_pattern <- "FLVTB|FSTTB|FSOTB|SLATB|FSHTB|DRLVT|RGRLMX|RGRLMN|SLAMAX|FSTR"
    yield_pattern <- "SPGF|WGRMX|ZRTMCD|ULLE|LLLE|FSWTD|COLDREP|CTSTER"
    
    phen_to_cal <- test_params_oryza %>% dplyr::filter(str_detect(Parameter, phen_pattern))
    
    low_minp <- phen_to_cal$Min %>% unlist()
    upp_maxp <- phen_to_cal$Max %>% unlist()
    names_parp <- phen_to_cal$Parameter %>% unlist()
    
    
    #1. Phenological development parameters
    message(paste0("1st Stage: GA_Phenology - Parameters: ", phen_pattern))
    tic(paste0("Phenology parameters Calibration"))
    GA_phen <- ga(type = "real-valued", 
                  fitness = function(x) -cal_phen_oryza(x[1], x[2], x[3], x[4], phen_to_cal, calibration_path, cultivar, input_data, exp_files, test_params_oryza, basedata_path),
                  lower = low_minp, 
                  upper = upp_maxp, 
                  maxiter = max_iter,
                  popSize = pop_size,
                  pmutation = 0.2,
                  parallel = cl, 
                  names = names_parp)
    
    GA_phen@solution
    toc()
    
    
    closeAllConnections()
    gc()
    
    phen_params <<- as.data.frame(GA_phen@solution) %>% sample_n(1) %>%
      pivot_longer(cols = everything(), values_to = "Set_cal", names_to = "Parameter") %>%
      mutate(Set_cal =  map(Set_cal, ~.x))
    
    message("GA - Phenology done!")
    #x1 = 64900      #"SPGF"  
    #x2 = 0.0000249  #"WGRMX"  
    #x3 = 0.4        #"ZRTMCD"  
    #x4 = 1.45       #"ULLE"   
    #x5 = 1404       #"LLLE"   
    #x6 = 0.4        #"FSWTD"    
    #x7 = 21         #"COLDREP"
    #x8 = 36.5       #"CTSTER"   
    
    test_params_model <- filter(test_params_oryza, str_detect(Parameter, growth_pattern))
    

    #   dplyr::select(Parameter, Set_cal = Base) %>% filter(test_params_model, str_detect(Parameter, growth_pattern))
    growth_to_cal <- generate_combinations_paramsTb(test_params_model, default_list, length_escenaries =  n_escenarios)
    
    
    
    # plan(multiprocess)
    registerDoFuture()
    cl <- makeCluster(ncores)
    plan(future::cluster, workers = cl)
    
    low_ming <- growth_to_cal$Min %>% unlist()
    upp_maxg <- growth_to_cal$Max %>% unlist()
    names_parg <- growth_to_cal$Parameter %>% unlist()
    
    
    
    #6. Growth parameters 
    message(paste0("2nd Stage: GA_Growth - Parameters: ", growth_pattern))
    tic("Growth and Leaf parameters Calibration")
    GA_growth <- ga(type = "real-valued", 
                    fitness = function(x) -cal_growth_oryza(x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8],
                                                            growth_to_cal, phen_params, calibration_path, cultivar, 
                                                          input_data, exp_files, test_params_oryza, basedata_path),
                    lower =  low_ming, 
                    upper = upp_maxg, 
                    maxiter = max_iter,
                    popSize = pop_size,
                    pmutation = 0.2,
                    parallel = cl, 
                    names = names_parg)
    
    GA_growth@solution
    toc()
    
    
    closeAllConnections()
    gc()
    
    ### Organiza parametros de GA para continuar proceso de calibracion 
    
    grow_paramsGA <- as.data.frame(GA_growth@solution) %>% sample_n(1) %>%
      pivot_longer(cols = everything(), values_to = "Set_cal", names_to = "Parameter") #%>%
    #  mutate(Set_cal =  map(Set_cal, ~.x))
    
    message("GA - Growth done!")
    
    pparams <- list(
      
      BFT = growth_to_cal %>% dplyr::filter(Parameter == "BFTB") %>% pull(tables) %>% pluck(1) %>%
           dplyr::filter(id == as.integer(filter(grow_paramsGA, Parameter == "BFTB") %>% pull(Set_cal))) %>% 
           pull(data) %>% pluck(1) %>% enframe(name = "Parameter", value = "Set_cal"),
         
         other_t = map(c("SLATB", "FSHTB", "DRLVT"),  ~dplyr::filter(growth_to_cal, Parameter == .x) %>% pull(tables) %>% pluck(1) %>%
               dplyr::filter( id == as.integer(filter(grow_paramsGA, Parameter == .x) %>% pull(Set_cal))) %>% 
               pull(data) %>% pluck(1)  %>% mutate(Parameter = .x) %>% nest(Set_cal = -Parameter)) %>% bind_rows(),
         
         other_p = tibble(Parameter = growth_to_cal$Parameter,
                           Set_cal = grow_paramsGA$Set_cal) %>% slice(-c(1:4)) %>% mutate(Set_cal =  map(Set_cal, ~.x)))
    
    
         
   # safe_bind <- possibly(bind_rows, otherwise = NULL)   
    
    growth_params <<-  safe_bind(pparams)
      

    #test_params_model <- bind_rows(phen_params, growth_params) %>% right_join(test_params_oryza, by = "Parameter") %>%
      #mutate(Base = map2(Base, Set_cal, function(x, y) if(is.null(y)){x} else {y})) %>% dplyr::select(-Set_cal)
    
    
    yield_to_cal <- test_params_oryza %>% filter(str_detect(Parameter, yield_pattern))
    
    # plan(multiprocess)
    registerDoFuture()
    cl <- makeCluster(ncores)
    plan(future::cluster, workers = cl)
    
   low_min <- yield_to_cal$Min %>% unlist()
   upp_max <- yield_to_cal$Max %>% unlist()
   names_par <- yield_to_cal$Parameter %>% unlist()
    
    ## Yield
    #9. Temperature and drought stress parameters
    message(paste0("3rd Stage: GA_Yield+Stress - Parameters: ", yield_pattern))
    tic("Yield calibration + stress parameters")
    GA_yield <- ga(type = "real-valued", 
                   fitness = function(x) -cal_yield_oryza(x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8],
                                                          yield_to_cal, phen_params, growth_params, calibration_path, cultivar, 
                                                          input_data, exp_files, test_params_oryza, basedata_path),
                   lower = low_min, 
                   upper = upp_max, 
                   maxiter = max_iter,
                   popSize = pop_size,
                   pmutation = 0.2,
                   parallel = cl, 
                   names = names_par)
    
    GA_yield@solution
    toc()
    
    closeAllConnections()
    gc()
    
    yield_params <<- as.data.frame(GA_yield@solution) %>% sample_n(1) %>%
      pivot_longer(cols = everything(), values_to = "Set_cal", names_to = "Parameter") %>%
      mutate(Set_cal =  map(Set_cal, ~.x))
    
    message("GA - Yield done!")
    

    
    message("Parameter Optimization Done!")
    
    return(list(params = bind_rows(phen_params, growth_params, yield_params), 
                GA_phen = GA_phen, GA_growth = GA_growth, GA_yield = GA_yield))
    
    
  
    
    
    
    
    } 
  else if(all(cal_stages %in% c("phen", "dry_matter_lai", "yield", "global"))) {
    
    message("ORYZA v3.0 - Genetic Algorithm
            4 stages:
            ")
    
    ##default list of parameters - based on IR72 and IR64 -- max an min == +/- 30%
    default_list <- tidy_to_write_crop(NULL)
    phen_pattern <- "DVRJ|DVRI|DVRP|DVRR"
    growth_pattern <- "FLVTB|FSTTB|FSOTB|SLATB|FSHTB|DRLVT|RGRLMX|RGRLMN|SLAMAX|FSTR"
    yield_pattern <- "SPGF|WGRMX|ZRTMCD|ULLE|LLLE|FSWTD|COLDREP|CTSTER"
    
    phen_to_cal <- test_params_oryza %>% dplyr::filter(str_detect(Parameter, phen_pattern))
    
    low_minp <- phen_to_cal$Min %>% unlist()
    upp_maxp <- phen_to_cal$Max %>% unlist()
    names_parp <- phen_to_cal$Parameter %>% unlist()
    
    
    #1. Phenological development parameters
    message(paste0("1st Stage: GA_Phenology - Parameters: ", phen_pattern))
    tic(paste0("Phenology parameters Calibration"))
    GA_phen <- ga(type = "real-valued", 
                  fitness = function(x) -cal_phen_oryza(x[1], x[2], x[3], x[4], phen_to_cal, calibration_path, cultivar, input_data, exp_files, test_params_oryza, basedata_path),
                  lower = low_minp, 
                  upper = upp_maxp, 
                  maxiter = max_iter,
                  popSize = pop_size,
                  pmutation = 0.2,
                  parallel = cl, 
                  names = names_parp)
    
    GA_phen@solution
    toc()
    
    
    closeAllConnections()
    gc()
    
    phen_params <<- as.data.frame(GA_phen@solution) %>% sample_n(1) %>%
      pivot_longer(cols = everything(), values_to = "Set_cal", names_to = "Parameter") %>%
      mutate(Set_cal =  map(Set_cal, ~.x))
    
    message("GA - Phenology done!")
    #x1 = 64900      #"SPGF"  
    #x2 = 0.0000249  #"WGRMX"  
    #x3 = 0.4        #"ZRTMCD"  
    #x4 = 1.45       #"ULLE"   
    #x5 = 1404       #"LLLE"   
    #x6 = 0.4        #"FSWTD"    
    #x7 = 21         #"COLDREP"
    #x8 = 36.5       #"CTSTER"   
    
    test_params_model <- filter(test_params_oryza, str_detect(Parameter, growth_pattern))
    
    
    #   dplyr::select(Parameter, Set_cal = Base) %>% filter(test_params_model, str_detect(Parameter, growth_pattern))
    growth_to_cal <- generate_combinations_paramsTb(test_params_model, default_list, length_escenaries =  n_escenarios)
    
    
    
    # plan(multiprocess)
    registerDoFuture()
    cl <- makeCluster(ncores)
    plan(future::cluster, workers = cl)
    
    low_ming <- growth_to_cal$Min %>% unlist()
    upp_maxg <- growth_to_cal$Max %>% unlist()
    names_parg <- growth_to_cal$Parameter %>% unlist()
    
    
    
    #6. Growth parameters 
    message(paste0("2nd Stage: GA_Growth - Parameters: ", growth_pattern))
    tic("Growth and Leaf parameters Calibration")
    GA_growth <- ga(type = "real-valued", 
                    fitness = function(x) -cal_growth_oryza(x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8],
                                                            growth_to_cal, phen_params, calibration_path, cultivar, 
                                                            input_data, exp_files, test_params_oryza, basedata_path),
                    lower =  low_ming, 
                    upper = upp_maxg, 
                    maxiter = max_iter,
                    popSize = pop_size,
                    pmutation = 0.2,
                    parallel = cl, 
                    names = names_parg)
    
    GA_growth@solution
    toc()
    
    
    closeAllConnections()
    gc()
    
    ### Organiza parametros de GA para continuar proceso de calibracion 
    
    grow_paramsGA <- as.data.frame(GA_growth@solution) %>% sample_n(1) %>%
      pivot_longer(cols = everything(), values_to = "Set_cal", names_to = "Parameter") #%>%
    #  mutate(Set_cal =  map(Set_cal, ~.x))
    
    message("GA - Growth done!")
    
    pparams <- list(
      
      BFT = growth_to_cal %>% dplyr::filter(Parameter == "BFTB") %>% pull(tables) %>% pluck(1) %>%
        dplyr::filter(id == as.integer(filter(grow_paramsGA, Parameter == "BFTB") %>% pull(Set_cal))) %>% 
        pull(data) %>% pluck(1) %>% enframe(name = "Parameter", value = "Set_cal"),
      
      other_t = map(c("SLATB", "FSHTB", "DRLVT"),  ~dplyr::filter(growth_to_cal, Parameter == .x) %>% pull(tables) %>% pluck(1) %>%
                      dplyr::filter( id == as.integer(filter(grow_paramsGA, Parameter == .x) %>% pull(Set_cal))) %>% 
                      pull(data) %>% pluck(1)  %>% mutate(Parameter = .x) %>% nest(Set_cal = -Parameter)) %>% bind_rows(),
      
      other_p = tibble(Parameter = growth_to_cal$Parameter,
                       Set_cal = grow_paramsGA$Set_cal) %>% slice(-c(1:4)) %>% mutate(Set_cal =  map(Set_cal, ~.x)))
    
    
    
   # safe_bind <- possibly(bind_rows, otherwise = NULL)   
    
    growth_params <<-  safe_bind(pparams)
    
    
    #test_params_model <- bind_rows(phen_params, growth_params) %>% right_join(test_params_oryza, by = "Parameter") %>%
    #mutate(Base = map2(Base, Set_cal, function(x, y) if(is.null(y)){x} else {y})) %>% dplyr::select(-Set_cal)
    
    
    yield_to_cal <- test_params_oryza %>% filter(str_detect(Parameter, yield_pattern))
    
    # plan(multiprocess)
    registerDoFuture()
    cl <- makeCluster(ncores)
    plan(future::cluster, workers = cl)
    
    low_min <- yield_to_cal$Min %>% unlist()
    upp_max <- yield_to_cal$Max %>% unlist()
    names_par <- yield_to_cal$Parameter %>% unlist()
    
    ## Yield
    #9. Temperature and drought stress parameters
    message(paste0("3rd Stage: GA_Yield+Stress - Parameters: ", yield_pattern))
    tic("Yield calibration + stress parameters")
    GA_yield <- ga(type = "real-valued", 
                   fitness = function(x) -cal_yield_oryza(x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8],
                                                          yield_to_cal, phen_params, growth_params, calibration_path, cultivar, 
                                                          input_data, exp_files, test_params_oryza, basedata_path),
                   lower = low_min, 
                   upper = upp_max, 
                   maxiter = max_iter,
                   popSize = pop_size,
                   pmutation = 0.2,
                   parallel = cl, 
                   names = names_par)
    
    GA_yield@solution
    toc()
    
    closeAllConnections()
    gc()
    
    yield_params <<- as.data.frame(GA_yield@solution) %>% sample_n(1) %>%
      pivot_longer(cols = everything(), values_to = "Set_cal", names_to = "Parameter") %>%
      mutate(Set_cal =  map(Set_cal, ~.x))
    
    message("GA - Yield done!")
    
    
    
    message(paste0("4th Stage: GA Global - Parameters: ", phen_pattern, growth_pattern, yield_pattern))
    
    test_params_global <- bind_rows(phen_params, growth_params, yield_params) %>% tidy_to_write_crop()
    # growth_params <- test_params_model %>% filter(str_detect(Parameter, growth_pattern)) %>% 
    #   dplyr::select(Parameter, Set_cal = Base)
    global_to_cal <- generate_combinations_paramsTb(test_params_global, default_list, length_escenaries =  n_escenarios)
    
    # plan(multiprocess)
    registerDoFuture()
    cl <- makeCluster(ncores)
    plan(future::cluster, workers = cl)
    
    low_min1 <- global_to_cal$Min %>% unlist()
    upp_max1 <- global_to_cal$Max %>% unlist()
    names_par1 <- global_to_cal$Parameter %>% unlist()
    
    
    tic("Global calibration")
    GA_oryza <<- ga(type = "real-valued", 
                   fitness = function(x) -cal_oryza_global(x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8],x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19], x[20],
                                                           global_to_cal, calibration_path, cultivar, 
                                                           input_data, exp_files, default_list, basedata_path, res_var = res_var),
                   lower = low_min1, 
                   upper = upp_max1, 
                   maxiter = max_iter,
                   popSize = pop_size,
                   pmutation = 0.2,
                   parallel = cl, 
                   names = names_par1)
    
    GA_oryza@solution
    toc()
    
    closeAllConnections()
    gc()
    
    
    oryza_paramsGA <- as.data.frame(GA_oryza@solution) %>% sample_n(1) %>%
      pivot_longer(cols = everything(), values_to = "Set_cal", names_to = "Parameter") #%>%
    #mutate(Set_cal =  map(Set_cal, ~.x))
    
    
    
    BFT <-  global_to_cal %>% dplyr::filter(Parameter == "BFTB") %>% pull(tables) %>% pluck(1) %>%
      dplyr::filter(id == as.integer(filter(oryza_paramsGA, Parameter == "BFTB") %>% pull(Set_cal))) %>% 
      pull(data) %>% pluck(1) %>% enframe(name = "Parameter", value = "Set_cal")
    
    
    other_t <- map(c("SLATB", "FSHTB", "DRLVT"),  ~dplyr::filter(global_to_cal, Parameter == .x) %>% pull(tables) %>% pluck(1) %>%
                    dplyr::filter( id == as.integer(filter(oryza_paramsGA, Parameter == .x) %>% pull(Set_cal))) %>% 
                    pull(data) %>% pluck(1)  %>% mutate(Parameter = .x) %>% nest(Set_cal = -Parameter)) %>% bind_rows()
    
    other_p <- tibble(Parameter = global_to_cal$Parameter,
                     Set_cal = oryza_paramsGA$Set_cal) %>% slice(-c(1:4)) %>% mutate(Set_cal =  map(Set_cal, ~.x))
    
    
    gparams <- list(BFT, other_t, other_p)
    
    global_params <<-  safe_bind(gparams)
    
    
    #closeAllConnections()
    message("GA - Global calibration done!")
    return(list(parameters_final = global_params, parameters_3stage = test_params_global, GA_phen = GA_phen, GA_growth = GA_growth, GA_yield = GA_yield, GA_global = GA_oryza))
    
    message("Parameter Optimization Done!")
    
    
  }
  else{message("Unknown Calibration Stages")}
  
  
  message("Parameter Optimization Done!")
  
  
}
