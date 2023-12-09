SLA_max <-
function(SLA_df, min = 0.0039, max = 0.0051, fr=0.5) {
  
  sla_max <- SLA_df %>%
    filter(DVSM<fr) %>%
    lm(SLA~DVSM, data = .) %>%
    summary() %>%
    .$coefficients %>% as_tibble() %>%
    mutate(par=Estimate+1.96*`Std. Error`) %>%
    .$par %>%
    .[1]
  
  
  if (sla_max < min) {
    
    message(paste("SLA_max calculated:", sla_max, "- is a low value.", "- Use default"))
    return(min)
    
  } else if (sla_max > max){
    
    message(paste("SLA_max
                  calculated:", sla_max, "- is a high value.", "- Use default"))
    return(max)
    
  } else {
    
    message(paste("SLA_max calculated:", sla_max, "- is a normal value.", "- Use data"))
    return(sla_max)
    
  }
}
