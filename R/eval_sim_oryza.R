eval_sim_oryza <-
function(obs_data, sim_data, exp_set, variable = "phen", by_var = F) {
    
    sim_ <- extract_sim_oryza(sim_data, exp_set, variable = variable)
    
    obs_ <- extract_obs_var(obs_data, variable = variable) %>% 
      dplyr::filter(exp_file %in% str_remove(exp_set, ".exp"))
    
    
    id_join <- if(variable == "phen" | variable == "yield"){ 
        c("exp_file", "var")
    } else {
        c("exp_file", "var", "date")
    } 
    
    if(variable == "phen"){
        obs_ <- obs_ %>% dplyr::select(-contains("data"))
        sim_ <- sim_ %>% dplyr::select(-contains("data"))
        }
    
    
    test_select <- obs_ %>%
        left_join(sim_, by = id_join) %>% 
        rename_at(vars(ends_with(".x")), list(~paste("obs"))) %>%
        rename_at(vars(ends_with(".y")), list(~paste("sim"))) %>% 
        filter(complete.cases(.), obs > 0)
        
    
    if(by_var == TRUE){
        
        test_select %>%
        nest(data = -c(var)) %>% 
        mutate(eval = map(data, ~get_metrics(.x)),
               plot = map(data, ~.x %>% 
                              ggplot(aes(obs, sim)) +
                              geom_point(aes(color = exp_file)) +
                              expand_limits(x = 0, y = 0) + 
                              geom_abline(intercept = 0, slope = 1, linetype = "twodash", linewidth=1)+
                              geom_abline(intercept = 0, slope = 1.15, linetype = "twodash", linewidth=0.5, color = "red") +
                              geom_abline(intercept = 0, slope = 0.85, linetype = "twodash", linewidth=0.5, color = "red") + 
                              #geom_smooth(method = "lm", se=F)+
                              theme_bw())) %>% 
        unnest(eval)
    } else {
        
        test_select %>%
            get_metrics %>% 
            mutate(var  = variable,
                   plot = list(test_select%>% 
                                   ggplot(aes(obs, sim)) +
                                   geom_point(aes(color = exp_file)) +
                                   expand_limits(x = 0, y = 0) + 
                                   geom_abline(intercept = 0, slope = 1, linetype = "twodash", linewidth=1)+
                                   geom_abline(intercept = 0, slope = 1.15, linetype = "twodash", linewidth=0.5, color = "red") +
                                   geom_abline(intercept = 0, slope = 0.85, linetype = "twodash", linewidth=0.5, color = "red") + 
                                   #geom_smooth(method = "lm", se=F)+
                                   theme_bw())) %>% 
            dplyr::select(var, everything())
        
    }
    
    
 
}
