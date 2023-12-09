set_theme_jre <-
function() {
  theme_jre <<- theme_bw() + theme(
    legend.position="bottom",
    panel.grid.minor = element_blank(),
    strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
    strip.text = element_text(face = "bold"))
}
