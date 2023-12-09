BPART_plot3 <-
function(data, save_plot = "N") {
  
  # Growth phases 
  gstage <- data.frame(
    Growth_Phase=c("Vegetative", "Reproductive", "Ripening"),
    start=c(0, 0.65, 1),
    end=c(0.65,1,2)) %>% mutate(Growth_Phase=factor(Growth_Phase, c("Vegetative", "Reproductive", "Ripening")))
  
  
  data <- data  %>% #drop_na() %>%
    mutate(LOC_ID = word(exp_file, 1, sep = "_"),
           cultivar = word(exp_file, 2, sep = "_"))
  
  plot <-  data %>%
    ggplot(aes(x=DVSM, y=Value)) +
    geom_smooth(se=F, aes(color=LOC_ID), span=0.5) + 
    geom_rect(data = gstage, aes(NULL, NULL, xmin=start, xmax=end, fill=Growth_Phase), ymin= -Inf, ymax=+Inf) + 
    scale_fill_manual(values = alpha(c("chartreuse", "darkgreen", "orange1"), 0.2)) + 
    geom_point(aes(shape=LOC_ID, label=exp_file), size=2) +
    scale_x_continuous(limits = c(-0.055, 2.055), expand = c(0, 0)) +
    scale_y_continuous(limits = c(-0.02, 1.02), expand = c(0, 0)) +
    facet_grid(Partition_Parameter ~.) + 
    labs(x="Development Stage (DVS)",
         title = paste0("Shoot dry matter partition - ", data$cultivar[[1]]),
         y="Fraction") +
    theme_bw()
  
  switch(save_plot,
         N = NULL, 
         Y = ggsave(plot, filename = paste0("Partition Factors ", data$cultivar[[1]], "3.png"), width=7, height=5))
  
  plot
  
}
