plot_lai_obs <-
function(lai_data){


  lai_ <- lai_data %>% #drop_na() %>%
    mutate(locality = word(exp_file, 1, sep = "_"),
           cultivar = word(exp_file, 2, sep = "_"),
           pat_rem = paste0(locality, "_", cultivar,"_"),
           label_p = map2_chr(exp_file, pat_rem, ~str_remove(.x, .y)))

  plot_lai <- lai_  %>%
    ggplot(aes(date, value, label = exp_file)) +
    geom_smooth(aes(y = value), color = "darkgreen" , linetype = "twodash", fill = "lightgray") +
    geom_point(aes(color = locality)) +
    geom_errorbar(aes(ymin = value - se, ymax = value + se, color = locality)) +
    #  geom_point(aes(y = value), color = "black") + ylim(0, 10) +
    theme_bw() + facet_wrap(~exp_file, scales = "free_x") +
    theme(
      #        axis.text.x = element_blank(),
      legend.position = "bottom",
      #        legend.title = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
      strip.text = element_text(face = "bold")) +
    labs(title = paste0("Leaf Area Index - Cultivar ", lai_$cultivar[[1]]),
         x = "Date",
         y = "LAI (m2/m2)",
         color = "Site: ") +
    ylim(0, 10)

  return(plot_lai)

}
