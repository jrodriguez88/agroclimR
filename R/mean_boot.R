mean_boot <-
function(x){
  
    smean.cl.boot(x, conf.int=.95, B=1000, na.rm=TRUE, reps=T)[1]}
