bootstrap_param <-
function(param_data, reps = 2000, ci = 0.95, stat = "mean"){
  
  require(Hmisc)
  
  stat <- switch (stat,
                  "mean" = 1,
                  "min" = 2,
                  "max" = 3
  )
  
  smean.cl.boot(param_data, conf.int=ci, B=reps, na.rm=TRUE, reps=T)[stat][[1]]
  
  
}
