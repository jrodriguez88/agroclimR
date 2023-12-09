plot_drymatter_obs <-
function(dry_matter_data){
  
  dry_matter_ <- dry_matter_data %>% #drop_na() %>%
    mutate(locality = word(exp_file, 1, sep = "_"),
           cultivar = word(exp_file, 2, sep = "_"),
           pat_rem = paste0(locality, "_", cultivar,"_"),
           label_p = map2_chr(exp_file, pat_rem, ~str_remove(.x, .y)))
  
  plot_dry_matter <- dry_matter_ %>% 
    ggplot(aes(date, value, label = exp_file)) + 
    #    geom_smooth(aes(y = sim, color = locality), linetype = "twodash", fill = "lightgray") +
    geom_point(aes(color = var)) +
    geom_errorbar(aes(ymin = value - se, ymax = value + se, color = var)) +
    geom_smooth(aes(y = value, color = var), se = F) + 
    facet_wrap(~exp_file, scales = "free_x") + 
    theme_bw() +
    theme(
      #        axis.text.x = element_blank(),
      legend.position = "bottom",
      #        legend.title = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
      strip.text = element_text(face = "bold")) + ylim(0, max(dry_matter_$value)+1000) +
    labs(title = paste0("Rice shoot dry matter - Cultivar ", dry_matter_$cultivar[[1]]), 
         x = "Date",
         y = "Dry matter (Kg/ha)",
         color = "Crop organs: ")
  
  return(plot_dry_matter)
  
}
