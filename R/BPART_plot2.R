BPART_plot2 <-
function(data, save_plot = "N") {
  
  data <- data  %>% #drop_na() %>%
    mutate(LOC_ID = word(exp_file, 1, sep = "_"),
           cultivar = word(exp_file, 2, sep = "_"))
  
  plot <-  data %>%
    ggplot(aes(x=DVSM, y=Value, label=exp_file)) +
    geom_point(shape=1,aes(label=exp_file), fill="gray") +
    geom_smooth(se=F, linetype="twodash", col="red") + 
    facet_grid(Partition_Parameter ~ LOC_ID) + 
    labs(x="Development Stage (DVS)",
         title = paste0("Shoot dry matter partition by Locality - ", data$cultivar[[1]]),
         y="Fraction") +
    theme_bw()
  
  switch(save_plot,
         N = NULL, 
         Y = ggsave(plot, filename = paste0("Partition Factors ", data$cultivar[[1]], "2.png"), width=7, height=5))
  
  plot
  
}
