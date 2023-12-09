DVR_plot1 <-
function(DVR_tb, save_plot = "N") {
  
  data <- DVR_tb  %>% #drop_na() %>%
    mutate(LOC_ID = word(exp_file, 1, sep = "_"),
           cultivar = word(exp_file, 2, sep = "_"))
  
  plot <- data %>%
    ggplot(aes(x=DVR, y=Value)) +
    #           geom_boxplot(fill="gray") +
    geom_jitter(aes(shape=LOC_ID, label = exp_file)) +
    stat_summary(fun.data = mean_cl_boot, color="red")+
    labs(x="", title = paste0("Development Rates for ", data$cultivar[[1]]),
         y = "DVR (oCd-1)", 
         #         y = bquote('DVR; ('*~degree*Cd^-1*')')),
         shape = "Locality:") +
    #           facet_grid(.~ DVR, scales="free") +
    theme_bw() + 
#    scale_shape_discrete(name="Locality") 
  #           theme(legend.position = "bottom", legend.title = element_blank()) +
  
  switch(save_plot,
         N = NULL, 
         Y = ggsave(plot, filename = paste0("DVR for ", data$cultivar[[1]], "_1.png"), width=7, height=3))
  
  plot
  
}
