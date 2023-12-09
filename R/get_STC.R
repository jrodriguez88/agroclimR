get_STC <-
function(S, C, sysclass="USDA") {
  stopifnot(require(soiltexture))
  
  Si <- 100-(S+C)
  dat <- data.frame(SAND=S, CLAY=C, SILT=Si)
  
  STC <- TT.points.in.classes(
    tri.data = dat,
    class.sys = paste0(sysclass, ".TT"),
    PiC.type = "t"
  )
  
  return(STC)
  
}
