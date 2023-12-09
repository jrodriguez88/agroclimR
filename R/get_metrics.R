get_metrics <-
function(data) {
  
  data %>% filter(complete.cases(.)) %>%
    summarise(n = n(),
              r = cor(obs, sim, method = c("pearson")),
              tau = cor(obs, sim, method = c("kendall")),
              RMSE = sqrt(mean((sim - obs)^2, na.rm = T)),
              NRMSE = RMSE/mean(obs, na.rm = T),
              MAE = sum(abs(sim - obs)/n),
              MBE = sum((sim - obs))/n,
              d = 1 - ((sum((sim - obs)^2, na.rm = T))/
                         sum((abs(sim - mean(obs, na.rm = T)) +
                                abs(obs - mean(obs, na.rm = T)))^2, na.rm = T)),
              NSE = 1 - ((sum((sim - obs)^2, na.rm = T))/
                           sum((obs - mean(obs, na.rm = T))^2, na.rm = T)),
              rsq = summary(lm(sim ~ obs))$r.squared)
  
}
