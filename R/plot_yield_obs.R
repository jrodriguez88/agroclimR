plot_yield_obs <-
function(yield_data){
  
  
  yield_ <- yield_data %>% #drop_na() %>%
    mutate(locality = word(exp_file, 1, sep = "_"),
           cultivar = word(exp_file, 2, sep = "_"),
           pat_rem = paste0(locality, "_", cultivar,"_"),
           label_p = map2_chr(exp_file, pat_rem, ~str_remove(.x, .y)))
  
  plot_yield <-  yield_ %>%
    ggplot(aes(exp_file, value, label = exp_file)) + 
    geom_point() +
    geom_errorbar(aes(ymin = value - se, ymax = value + se, color = locality), width = .2, position = position_dodge(0.2)) +
    geom_point(aes(y = value, color = locality)) + 
    #    geom_vline(xintercept = mean(obs)) + 
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45),
      #legend.position = "bottom",
      #        legend.title = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
      strip.text = element_text(face = "bold")) +
    expand_limits(y = 0) +
    labs(title = paste0("Rice Yield - Cultivar ", yield_$cultivar[[1]]) , 
         x = "Experimental file",
         y = "Yield (Kg/ha - 14% grain moisture )",
         color = "Site: ")
  
  return(plot_yield)
  
  
}
