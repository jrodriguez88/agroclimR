DRLV_plot <-
function(DRLV_df, llim = 1.1, save_plot = "N") {
  
  data <- DRLV_df  %>% #drop_na() %>%
    mutate(LOC_ID = word(exp_file, 1, sep = "_"),
           cultivar = word(exp_file, 2, sep = "_"))%>%
    rename(DVS=DVSM)
  
  plot  <- data %>% filter(DVS>llim) %>%
    ggplot(aes(DVS, DRLV, label=exp_file)) +
    geom_point(aes(shape=LOC_ID)) + 
    geom_smooth(se=F, linetype="twodash", color="red") + 
    labs(title = paste0("Leaf Death Coefficient - ", data$cultivar[[1]]),
         x = "Development Stage (DVS)") +
    theme_bw()
  
  
  
 # switch(save_plot,
 #        N = NULL, 
 #        Y = ggsave(plot, filename = paste0("DRLV for ", data$cultivar[[1]], ".png"), width=7, height=3))
 # 
  plot
  
}
