tidy_to_write_crop <-
function(param_data, model = "dssat", values = "Base", export_default = F){

  ###Crea tabla control de parametros y rangos, basados en el CRO standard de aquacrop
  ## Max and min  + Parameter (+/- 30%)

  default_list <- tibble(

    #strsplit(clipboard(), "\n") %>% unlist() %>% paste(collapse = "', '")
    Model = c(rep("DSSAT_CERES", 10)),

    Component = c('Phenology', 'Phenology', 'Phenology', 'Phenology',
                  'Leaf and stem growth', 'Growth AGB-RZ', 'Growth AGB-RZ', 'Growth AGB-RZ', 'Temperature and drought stress','Temperature and drought stress' ),


    Parameter = c('P1', 'P2O', 'P2R', 'P5', 'PHINT', 'G1', 'G2', 'G3', 'THOT', 'TCLDP'),
    Unit = c('GDD', 'h', 'GDD', 'GDD', 'GDD', 'No/g', 'g', 'scaler value', 'oC', 'oC'),
    Base = map(list('500', '12', '100', '450', '83', '55', '0.025', '1', '28', '15'), function(x) if(is.character(x)){as.numeric(x)}else{x}),
    Min = map(list('150', '11', '5', '150', '55', '50', '0.015', '0.7', '25', '12'), function(x) if(is.character(x)){as.numeric(x)}else{x}),
    Max = map(list('800', '13', '300', '850', '90', '75', '0.032', '1.3', '42', '20'), function(x) if(is.character(x)){as.numeric(x)}else{x}),

    Description = c('Basic vegetative phase', 'Critical photoperiod or the longest day length',
                    'Extent to which phasic development leading to panicle initiation is delayed',
                    'Time period from beginning of grain filling', 'Phylochron interval',
                    'Potential spikelet number coefficient', 'Single grain weight', 'Tillering coefficient',
                    'Temperature above which spikelet sterility is affected by high temperature',
                    'Temperature below which spikelet sterility is affected by low temperature')



  )


  if(is.null(param_data)){return(default_list)}



  if(isTRUE(export_default)){default_list <<- default_list}


  if(any(class(param_data) == "data.frame")) {

    stopifnot(any(names(param_data) == "Parameter"))

    message(paste0("Parameters to CRP file: ", paste(param_data$Parameter, collapse = ", ")))

    param_data <- rename_with(param_data, .fn = function(x) str_replace(x, "Base|Set_cal", values))
    if(any(names(param_data) == "Min")){message("Minimum range are available")
    } else {param_data <- mutate(param_data, Min = list(NULL))}
    if(any(names(param_data) == "Max")){message("Maximum range are available")
    } else {param_data <- mutate(param_data, Max = list(NULL))}


    test_params_oryza <- default_list %>%
      left_join(param_data, by = "Parameter") %>%
      mutate(Base = map2(Base.x, Base.y, function(x, y) if(is.null(y)){x} else {y}),
             Min = map2(Min.x, Min.y, function(x, y) if(is.null(y)){x} else {y}),
             Max = map2(Max.x, Max.y, function(x, y) if(is.null(y)){x} else {y})) %>%
      dplyr::select(-contains(".x"), -contains(".y"))


  } else if(any(class(param_data) == "list")) {

    stopifnot(any(default_list$Parameter %in% names(param_data)))

    message(paste0("Parameters to CRP file: ", paste(names(param_data), collapse = ", ")))

    param_data <- enframe(param_data, name = "Parameter", value = "Base") %>%
      mutate(Min = list(NULL), Max = list(NULL))


    test_params_oryza <- default_list %>%
      left_join(param_data, by = "Parameter") %>%
      mutate(Base = map2(Base.x, Base.y, function(x, y) if(is.null(y)){x} else {y}),
             Min = map2(Min.x, Min.y, function(x, y) if(is.null(y)){x} else {y}),
             Max = map2(Max.x, Max.y, function(x, y) if(is.null(y)){x} else {y})) %>%
      dplyr::select(-contains(".x"), -contains(".y"))


  }


  return(test_params_oryza)

}
