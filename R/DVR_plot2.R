DVR_plot2 <-
function(DVR_tb, save_plot = "N") {
  
  data <- DVR_tb  %>% #drop_na() %>%
    mutate(LOC_ID = word(exp_file, 1, sep = "_"),
           cultivar = word(exp_file, 2, sep = "_"))
  
  plot <- data %>% filter(DVR!= "DVRI") %>% 
    ggplot(aes(Value)) + #, fill=LOC_ID)) + 
    geom_density(aes(fill=DVR), alpha=.3, adjust = 1) +
    geom_rug(aes(x = Value, y = 1, color=DVR), position = position_jitter(height = 0)) +
    labs(y=NULL, title = paste0("Density of Probability - Development Rates for ", data$cultivar[[1]]))+
    facet_grid(.~ DVR) +
    theme_bw() + 
    theme(legend.position="none", axis.text.y = element_blank())
  
  
  #    scale_x_continuous(labels = scientific) +
  
  
  switch(save_plot,
         N = NULL, 
         Y = ggsave(paste0("DVR for ", data$cultivar[[1]], "_2.png"), width=7, height=3.2))
  
  plot
  
}
