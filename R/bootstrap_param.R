bootstrap_param <- function(data, reps = 2000, ci = 0.95, stat = "mean"){

  ci_diff <- 1-ci
  ci_inf = 0 + ci_diff/2
  ci_sup <- 1 - ci_diff/2


  mean_b <- mean(sample(data, reps, replace = T), na.rm = TRUE)
  min_b <-  quantile(sample(data, reps, replace = T), ci_inf, na.rm = TRUE)
  max_b <- quantile(sample(data, reps, replace = T), ci_sup, na.rm = TRUE)

  stat <- switch (stat,
                  "mean" = mean_b,
                  "min" = min_b,
                  "max" = max_b)

  #smean.cl.boot(data, conf.int=ci, B=reps, na.rm=TRUE, reps=T)[stat][[1]]


}
