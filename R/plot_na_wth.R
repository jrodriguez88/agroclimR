plot_na_wth <-
function(id, wth_data) {
  
  
  vis_miss(dplyr::select(wth_data, -date), warn_large_data = F) +
    #  facet_wrap(id ~.)
    labs(title = id) +
    #       subtitle = paste0(city, " (", dpto, ") "),
    #       caption = "Source: IDEAM") +
    scale_y_continuous(breaks = seq(0, length(wth_data$date), by = 365*5), 
                       labels = cut.Date(wth_data$date, breaks = "5 years") %>% 
                         unique() %>% year()) +
    coord_flip() +
    theme(axis.text.x = element_text(angle = 0))
  
  
}
