make_grow_curves <-
function(data, dvs = c(0, 0.5, 0.75, 1, 1.5, 2.5), model = "loess", span = 0.5){
  
  
  
  if(model == "loess"){
    
    modeled_data <- data %>% dplyr::select(contains("Parameter"), contains("DVSM"), Value) %>% 
      nest(data = -contains("Parameter")) %>%
      mutate(data =  map(data , ~.x %>% set_names(c("X", "Y"))), 
             model =  map(data, ~loess(Y~X, data = .x, se = T, span = span)),
             predict_tb = map(model, ~predict(.x, newdata = data.frame(X = dvs), interval = "confidence", se =T) %>% 
                                as.data.frame() %>% 
                                mutate(
                                  Min = fit - 1.96*se.fit,
                                  Max = fit + 1.96*se.fit) %>%
                                dplyr::select(Base = fit, Min, Max) %>% 
                                round(5) %>% mutate(DVS = dvs))) %>% 
      dplyr::select(contains("Parameter"), predict_tb) %>% 
      unnest(predict_tb) %>% dplyr::select(contains("Parameter"), DVS, everything())
    
    
    
    
  } else {
    
    # Function to find the best linear model to describe crop grow curves
    find_best_model <- function(data) {
      
      #data$Y
      
      # Crear una lista de modelos que se quieren comparar
      models <- list(lm(Y ~ X, data),
                     lm(Y ~ X + I(X^2), data),
                     lm(Y ~ X + I(X^2) + I(X^3), data),
                     #loess(Y ~X , data), 
                     glm(Y ~ X, family = quasibinomial(link = "logit"), data)
                     #nls(Y ~ A * exp(-B * exp(-C * X)), start = list(A = 1, B = 1, C = 1), data),
                     #nls(Y ~ A * (1 - exp(-B * (X - C))), start = list(A = 1, B = 1, C = 1), data),
                     #nls(Y ~ A / (1 + exp(-B * (X - C))^D), start = list(A = 1, B = 1, C = 1, D = 1), data),
                     #nls(Y ~ K * X / (X + a * (1 - X/K)), start = list(K = 1, a = 1), data)
      )
      
      # Función para calcular el AIC (Akaike Information Criterion)
      calculate_aic <- function(model) {
        AIC(model, k = log(nrow(data)))
      }
      
      # Calcular el AIC para cada modelo
      aics <- map_dbl(models, calculate_aic)
      
      # Encontrar el modelo con el AIC más bajo
      best_model <- models[[which.min(aics)]]
      
      # Devolver el modelo con el AIC más bajo y su AIC
      return(list(best_model, aics[which.min(aics)]))
    }
    
    #result <- find_best_model(data)
    #best_model <- result[[1]]
    #best_aic <- result[[2]]
    
    
    
      modeled_data <- data %>% dplyr::select(contains("Parameter"), contains("DVSM"), Value) %>% 
        nest(data = -contains("Parameter")) %>%
      mutate(data =  map(data , ~.x %>% set_names(c("X", "Y"))), 
             model =  map(data, ~find_best_model(.x)[[1]]),
             predict_tb = map(model, ~predict(.x, newdata = data.frame(X = dvs), interval = "confidence") %>% 
                                as.data.frame() %>% set_names(c("Base", "Min", "Max")) %>% 
                                round(5) %>% mutate(DVS = dvs))) %>% 
      dplyr::select(contains("Parameter"), predict_tb) %>% 
      unnest(predict_tb) %>% dplyr::select(contains("Parameter"), DVS, everything())
  }
  
  
  
  
  # Function to create crp tb
  crp_pf_tb <- function(data, stat = "Base") {
    
    data <- data %>% dplyr::select(Partition_Parameter, DVS, Value = contains(stat)) %>%#bind_rows(.id = "Partition_Parameter") %>%
      mutate(
        Value=case_when(
          Partition_Parameter == "FLV" & DVS > 1.5 ~ 0, 
          Partition_Parameter == "FST" & DVS > 1.5 ~ 0,
          Partition_Parameter != "FSO" & DVS  == 0 ~ 0.5,
          Partition_Parameter == "FSO" & DVS > 1.5 ~ 1,
          Partition_Parameter == "FSO" & DVS < 0.75 ~ 0,
          Value<0 | is.na(Value) ~ 0,
          TRUE ~ Value))
    
    
    if(stat == "Base"){
      
      data <-  data %>%
        pivot_wider(names_from =Partition_Parameter, values_from =  Value) %>%
        mutate(PF_sum=FLV+FSO+FST,
               PF_diff=1-PF_sum,
               FLV = case_when(
                 DVS<1 ~ FLV+(PF_diff/2),
                 TRUE ~ FLV),
               FST = case_when(
                 DVS<1 ~ FST+(PF_diff/2),
                 TRUE ~ FST),
               FSO = case_when(
                 DVS>=1 ~ FSO+PF_diff,
                 TRUE ~ FSO),
               PF_sum2= FLV+FSO+FST,
               Test_log = (FLV+FSO+FST)==1) %>%
        #            rename(DVSM=DVS) %>%
        select(DVS, FLV, FST, FSO) %>%
        pivot_longer(cols = -DVS, names_to = "Partition_Parameter", values_to = "Value") %>%
        mutate(Partition_Parameter = factor(Partition_Parameter,
                                            c("FLV", "FST", "FSO"))) %>%
        dplyr::select(Partition_Parameter, DVS, Value)
      
    }
    
    return(data)
    
  }
  
  crp_sla_tb <- function(data, stat = "Max", SLA_max) {
    
    data %>%  dplyr::select(DVS, Value = contains(stat)) %>%
      mutate(
        Value = case_when(
          DVS == 0 ~ SLA_max,
          Value < 0 | is.na(Value) ~ min(Value, na.rm = T),
          TRUE ~ Value))
    
  }
  
  crp_drlv_tb <- function(data, stat = "Base") {
    
    data %>%  dplyr::select(DVS, Value = contains(stat)) %>%
      mutate(
        Value= case_when(
          DVS > 2   ~ 0.05,
          DVS == 0 ~ 0,
          Value < 0 | is.na(Value) ~ 0,
          TRUE ~ Value))
    
  }
  
  #library(caret)
  #
  ##define k-fold cross validation method
  #ctrl <- trainControl(method = "cv", number = 5)
  #grid <- expand.grid(span = seq(0.3, 0.9, by = 0.05), degree = 1)
  #
  ##perform cross-validation using smoothing spans ranginf from 0.5 to 0.9
  #model <- train(Y ~ X,  data = a$data[[1]], method = "gamLoess", tuneGrid=grid, trControl = ctrl)
  
  
  # join data in list
  
  if(any(str_detect(names(data), "SLA"))){
    
    SLA_max = SLA_max(data)
    
    crp_list <- list(
      Base = crp_sla_tb(modeled_data, stat = "Base", SLA_max),
      Min =  crp_sla_tb(modeled_data, stat = "Min" , SLA_max),
      Max =  crp_sla_tb(modeled_data, stat = "Max" , SLA_max))
    
  } else if (any(str_detect(names(data), "DRLV"))){
    
    crp_list <- list(
      Base = crp_drlv_tb(modeled_data, stat = "Base"),
      Min =  crp_drlv_tb(modeled_data, stat = "Min"),
      Max =  crp_drlv_tb(modeled_data, stat = "Max"))
    
  } else {
    
    crp_list <- list(
      Base = crp_pf_tb(modeled_data, "Base"),
      Min = crp_pf_tb(modeled_data, "Min"),
      Max = crp_pf_tb(modeled_data, "Max"))}
  
  
  return(crp_list)
  
  
}
