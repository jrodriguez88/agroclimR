plot_pf_loess <-
function(data_obs, data_sim, span=0.75, nse=4, width=10, height=4) {
  
  # Growth phases 
  gstage <- data.frame(
    Growth_Phase=c("Vegetative", "Reproductive", "Ripening"),
    start=c(0, 0.65, 1),
    end=c(0.65,1,2)) %>% mutate(Growth_Phase=factor(Growth_Phase, c("Vegetative", "Reproductive", "Ripening")))
  
  data <- data_obs  %>% #drop_na() %>%
    mutate(LOC_ID = word(exp_file, 1, sep = "_"),
           cultivar = word(exp_file, 2, sep = "_"))%>%
    rename(DVS=DVSM)
  
  
  plot <- data %>%
    #    mutate(Partition_Parameter=case_when(
    #        Partition_Parameter=="FLV" ~ "Leaves (FLV)",
    #        Partition_Parameter=="FST" ~ "Stems (FST)",
    #        Partition_Parameter=="FSO" ~ "Panicles (FSO)"
    #)) %>% 
    ggplot(aes(x=DVS, y=Value)) +
    geom_rect(data = gstage, aes(NULL, NULL, xmin=start, xmax=end, fill=Growth_Phase), 
              ymin= 0, ymax=1) + 
    geom_point(aes(shape=LOC_ID, label=exp_file), size=2) +
    geom_line(data=data_sim$mean,
              aes(color=paste0("Loess - span:", span)), size=1) + 
    #    geom_smooth(se=T, span=0.3) + 
    geom_line(data=data_sim$max, 
              aes(color=paste0("SE*", nse)), linetype="twodash") +
    geom_line(data=data_sim$min, color="red", linetype="twodash") +
    theme_bw() +
    theme(strip.background = element_rect(fill="white")) +
    labs(x="Development Stage (DVS)",
         title = paste0("Shoot dry matter partition - ", data$cultivar[[1]]),
         y="Fraction") +
    scale_x_continuous(limits = c(-0.055, 2.055), expand = c(0, 0)) +
    scale_y_continuous(limits = c(-0.02, 1.02), expand = c(0, 0)) +
    scale_fill_manual(name="Growth phase",
                      values = alpha(c("chartreuse", "darkgreen", "orange1"), 0.2)) + 
    scale_shape_discrete(name="Locality") + 
    scale_color_manual(name="Smoth curve", values= c("darkgreen", "red")) +
    #    scale_fill_discrete(name="Growth phase") +
    facet_grid(. ~ Partition_Parameter)
  
#  ggsave(plot, filename = paste0(path, "/Partition Factors for ", data$cultivar[[1]], ".png"),
#         width=width, height=height)
#  
  plot
  
  
}
