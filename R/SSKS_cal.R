SSKS_cal <- function(S, C, SOM=1.5, SBDM=1.5, kmin=0.1, kmax=250, output = 'bootmean') {

  #    source("https://raw.githubusercontent.com/jrodriguez88/ORYZA_Model_RTOOLS/master/PT_Functions.R")


  ssks_data <- tibble(S,C,SOM,SBDM) %>%
    mutate(SSKS_Brakensiek = SSKS_Brakensiek(S, C, SBDM),
           SSKS_Campbell = SSKS_Campbell(S, C),
           SSKS_Cosby = SSKS_Cosby(S, C),
           SSKS_Dane_Puckett = SSKS_Dane_Puck(C),
           SSKS_Jabro = SSKS_Jabro(S, C, SBDM),
           SSKS_Puckett = SSKS_Puckett(C),
           SSKS_Rawls = SSKS_Rawls(S, C),
           SSKS_Saxton = SSKS_Saxton(S, C, SOM, SBDM),
           SSKS_Suleiman_Ritchie = SSKS_Suleiman_Ritchie(S, C, SOM, SBDM),
           SSKS_Wosten = SSKS_Wosten99(S,C, SOM, SBDM),
           SSKS_Vereecken = SSKS_Vereecken(S, C, SOM, SBDM),
           SSKS_Ferrer = SSKS_Ferrer(S)) %>%
    select(contains("SSKS")) %>%
    gather(key="KS_PTF", value = "SSKS") %>%
    extract(KS_PTF, "KS_PTF", "_([a-zA-Z0-9_]+)") %>%
    mutate(KS_PTF = as.factor(KS_PTF),
           SSKS = replace(SSKS, SSKS > kmax, NA),
           SSKS = replace(SSKS, SSKS < kmin, NA)) %>%
    drop_na()

  summary <- ssks_data %>%
    dplyr::summarise(ssks_bootmean = mean(sample(SSKS, 1000, replace = T)),
              ssks_mean = mean(SSKS),
              ssks_bootmedian = median(sample(SSKS, 1000, replace = T)),
              ssks_median = median(SSKS),
              ssks_min = quantile(sample(SSKS, 1000, replace = T),0.025),
              ssks_max = quantile(sample(SSKS, 1000, replace = T),0.975),
              ssks_sd = sd(SSKS))

  switch (output,
          data = ssks_data,
          summary = summary,
          bootmean = summary$ssks_bootmean,
          mean = summary$ssks_mean,
          bootmedian = summary$bootmedian,
          median = summary$ssks_median,
          min = summary$ssks_min,
          max = summary$ssks_max,
          sd = summary$ssks_sd)


}



# helpers -----------------------------------------------------------------



