DVR_plot3 <-
function(DVR_tb, save_plot = "N") {
  
  
  data <- DVR_tb  %>% #drop_na() %>%
    mutate(LOC_ID = word(exp_file, 1, sep = "_"),
           cultivar = word(exp_file, 2, sep = "_")) 
  
  plot <- data  %>%
    ggplot(aes(x=LOC_ID, y=Value)) + 
    stat_summary(fun.data = mean_cl_boot, position = position_dodge(width=0.2))+
    labs(x="Locality",
         title = paste0("Development Rates for ", data$cultivar[[1]]),
         y=bquote('DVR; ('*~degree*Cd^-1*')'))+
    facet_grid(.~ DVR, scales="free") +
    theme_bw() + 
    theme(legend.title = element_blank(), 
          strip.background = element_rect(fill="white"), 
          axis.text.x = element_text(angle=90, hjust = 1))
  
  
  
  
  switch(save_plot,
         N = NULL, 
         Y = ggsave(paste0("DVR for ", data$cultivar[[1]], "_3.png"), width=7, height=3))
  
  plot
  
}
