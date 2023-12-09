generate_combinations_paramsTb <-
function(params_to_cal, default_list, length_escenaries = 2000){
  
  # Función para generar combinaciones
  generate_combinations <- function(min1, max1, min2, max2, min3, max3) {
    # Crear un data frame con los rangos mínimo y máximo de cada factor
    ranges <- data.frame(min=c(min1, min2, min3), max=c(max1, max2, max3))
    
    # Generar todas las combinaciones posibles de los factores
    combinations <- expand.grid(seq(ranges[1,1], ranges[1,2], by = 0.001),
                                seq(ranges[2,1], ranges[2,2],  by = 0.001),
                                seq(ranges[3,1], ranges[3,2],  by = 0.001)) 
    
    # Filtrar combinaciones que sumen 1
    combinations <- combinations[rowSums(combinations) == 1, ]
    
    return(combinations)
    
  }
  
  
  ### Filtrar parametros en tablas
  
  ## Partition Tables
  part_tables <- default_list %>% dplyr::filter(str_detect(Parameter, "FSOTB|FSTTB|FLVTB")) 
  
  ## Single time series table   
  sing_tables <- default_list %>% dplyr::filter(str_detect(Parameter, pattern = "DRLVT|SLATB|FSHTB", negate = F)) %>% 
    mutate(tables = map2(Min, Max, ~left_join(.x, .y, by = "DVS") %>% set_names(c("DVS", "min", "max")))) %>% #slice(1) %>%
    mutate(tables = map(tables, ~.x %>% mutate(table = map2(min, max, ~seq(from = .x, to =  .y, length.out = length_escenaries))))) %>%
    mutate(tables =  map(tables, ~.x %>% mutate(table = map(table, enframe, name = "id")) %>% 
                           dplyr::select(-c(min, max)) %>% unnest(table) %>% 
                           nest(data = -id))) %>% 
    mutate(Base = rep(list(as.integer(length_escenaries/2)), 3), Min = rep(list(as.integer(0)), 3), Max = rep(list(as.integer(length_escenaries)), 3))
  
  
  #a$tables[[2]] %>% slice(23) %>% pull(data)
  
  
  
  ### Genera combinaciones de tablas de los parametros de particion de biomasa
  c <- part_tables %>% 
    mutate(tables = map2(Min, Max, ~left_join(.x, .y, by = "DVS") %>% set_names(c("DVS", "min", "max"))))
  
  
  d <- c$tables  %>% reduce(left_join, by = "DVS") %>% 
    mutate(combinations = pmap(list(min1 = min.x, max1 = max.x, min2 = min.y, max2 = max.y, min3 = min, max3 = max), generate_combinations))
  
  
  number_combinations <- length_escenaries  
  
  combinations_bpf<- d %>% mutate(samples = map(combinations, ~sample_n(.x, number_combinations, replace = T) %>% mutate(id = 1:number_combinations)))  %>%
    unnest(samples) %>% dplyr::select(-c(combinations, min.x:max)) %>% rename(FLV = Var1, FST = Var2, FSO = Var3) %>% nest(data = -id) %>% 
    mutate(data = map(data, ~list(dplyr::select(.x, DVS, FLV), dplyr::select(.x, DVS, FST), dplyr::select(.x, DVS, FSO)) %>% set_names(part_tables$Parameter))) #%>%
  #dplyr::select(-data)
  
  
  
  
  ## genera fila con variable de tablas de particion de biomas
  part_table <- part_tables %>% slice(1) %>% 
    mutate(Parameter = "BFTB", Base = list(as.integer(length_escenaries/2)), Min = list(as.integer(0)), Max = list(as.integer(length_escenaries)),
           tables = list(combinations_bpf)) %>% dplyr::select(all_of(names(sing_tables)))
  
  
  
  
  #Une los resultados y devuelve la lista con la variable tables para analizar en algoritmo de optimizacion
  comb_final <- bind_rows(sing_tables, part_table, params_to_cal %>% dplyr::filter(str_detect(Parameter, "SLATB|FSHTB|DRLVT|FLVTB|FSTTB|FSOTB", negate = T)) %>% 
              mutate(tables = list(rep(NULL, 3))))
  
  return(comb_final)
  
  
}
