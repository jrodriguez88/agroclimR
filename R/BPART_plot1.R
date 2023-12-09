BPART_plot1 <-
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
    geom_smooth(se=F, aes(color=Partition_Parameter), span=0.5) + 
    geom_rect(data = gstage, aes(NULL, NULL, xmin=start, xmax=end, fill=Growth_Phase), ymin=0, ymax=1) + 
    scale_fill_manual(values = alpha(c("chartreuse", "darkgreen", "orange1"), 0.2)) +
    labs(x="Development Stage (DVS)",
         title = paste0("Shoot dry matter partition - ", data$cultivar[[1]]),
         y="Fraction") +
    scale_x_continuous(limits = c(-0.055, 2.055), expand = c(0, 0)) +
    scale_y_continuous(limits = c(-0.02, 1.02), expand = c(0, 0)) +
    geom_point(aes(color=Partition_Parameter, label=exp_file)) +
    theme_bw()
  
  switch(save_plot,
         N = NULL, 
         Y = ggsave(plot, filename = paste0("Partition Factors ", data$cultivar[[1]], "1.png"), width=7, height=4))
  
  plot
  
}
