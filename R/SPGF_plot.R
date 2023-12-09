SPGF_plot <-
function(SPGF_df, save_plot = "N") {
  
  lm_spgf <- lm(formula = SPIK_M2_max ~ diff_pi_fl, data = SPGF_df)
  
  plot <- ggplot(SPGF_df, aes(diff_pi_fl, SPIK_M2_max))+
    geom_point(aes(shape=LOC_ID, label=ID, color = LOC_ID))+
    geom_smooth(method = "lm", se = F, linetype="twodash")+
    theme_bw()+
    xlab("Growth between PI and flowering (g/m²)")+
    ylab("Spikelets/m²") +
    annotate("text", x=-Inf, y=c(max(SPGF_df$SPIK_M2_max), max(SPGF_df$SPIK_M2_max)*0.95, max(SPGF_df$SPIK_M2_max)*0.90, max(SPGF_df$SPIK_M2_max)*0.85) , hjust=-0.1,vjust=0, 
             label=c(paste0(" y = ", round(summary(lm_spgf)$`coefficients`[2,1],2),"x"),
                     paste(" n = ", nrow(SPGF_df)),
                     paste(" r² = ", round(summary(lm_spgf)$`r.squared`,2)), 
                     paste("Pr(>|t|) =", round(summary(lm_spgf)$`coefficients`[2,4],4))))
  
  switch(save_plot,
         N = NULL, 
         Y = ggsave(plot, filename = paste0("SPGF for ", data$cultivar[[1]], ".png"), width=5, height=3))
  
  plot
  
}
