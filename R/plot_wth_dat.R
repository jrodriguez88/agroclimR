plot_wth_dat <-
function(wth_data, station = NULL){
  
  if(!is.null(station)){
    wth_data <- mutate(wth_data, station = station)
  }
  
  wth_data %>%
    pivot_longer(cols = c(tmax, tmin, rain), names_to = "var") %>%
    ggplot(aes(date, value)) +
    geom_line(aes(color = var)) +
    facet_grid(var ~ station, scales = "free") +
    theme_bw() +
    theme(legend.position = "bottom") + 
    scale_colour_manual(values = c("blue", "red", "orange"))
  
}
