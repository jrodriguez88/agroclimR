FSTR_plot <-
function(FSTR_df, save_plot = "N") {
  
  data <- FSTR_df  %>% #drop_na() %>%
    mutate(LOC_ID = word(exp_file, 1, sep = "_"),
           cultivar = word(exp_file, 2, sep = "_"))
  
  plot <- data %>% filter(FSTR>0) %>% mutate(LOC_ID =  str_sub(exp_file, 1,4)) %>%
    ggplot(aes(LOC_ID, FSTR, label=exp_file)) +
    geom_jitter(width = 0.1) +
    stat_summary(fun.data = mean_se, color="red") +
    geom_hline(yintercept = mean(FSTR_df$FSTR), color="blue", linetype="twodash") +
    annotate("text", x = 0.65, y = mean(FSTR_df$FSTR), 
             label =  paste0("mean =\n", round(mean(FSTR_df$FSTR),3))) + 
    labs(title = paste0("Fraction of carbohydrates allocated to stems (stored as reserves) - ", data$cultivar[[1]]),
         x="Locality") +
    theme_bw()
  
  #switch(save_plot,
  #       N = NULL, 
  #       Y = ggsave(plot, filename = paste0("FSTR for ", cultivar, ".png"), width=7, height=3))
  
  plot
  
}
