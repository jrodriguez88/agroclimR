plot_phen_obs <-
function(phen_data){


  phen_ <- phen_data %>% #drop_na() %>%
    mutate(locality = word(exp_file, 1, sep = "_"),
           cultivar = word(exp_file, 2, sep = "_"),
           pat_rem = paste0(locality, "_", cultivar,"_"),
           label_p = map2_chr(exp_file, pat_rem, ~str_remove(.x, .y)))

  plot_phen <- phen_ %>% mutate(locality = str_sub(exp_file, 1, 4),
                                var = factor(var, levels = c("IDAT", "FDAT", "MDAT"))) %>%
    ggplot(aes(var, value, label = exp_file)) +
    geom_jitter(aes(color = locality), size = 2) +
    facet_grid(~var, scales = "free_x") +
    stat_summary(fun.data  = mean_cl_normal) +
    theme_bw() +
    theme(
      axis.text.x = element_blank(),
      legend.position = "bottom",
      #        legend.title = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
      strip.text = element_text(face = "bold")) +
    expand_limits(y = 0) +
    labs(title = paste0("Crop Phenology - Cultivar ", phen_$cultivar[[1]]),
         x = "Development Stage",
         y = "Days After Emergence (days)",
         color = "Site: ")

  return(plot_phen)

}
