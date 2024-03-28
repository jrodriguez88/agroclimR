#' Soil Pedotransfer Function (PTF) to estimate Soil Saturated Hydraulic Conductivity (SSKS)
#'
#' This function calculates the Saturated Hydraulic Conductivity (SSKS)
#' based on various soil pedotransfer functions (PTFs) using soil properties as input parameters.
#'
#' @inheritParams WCWP_Saxton
#' @param kmin Minimum threshold value for SSKS (mm/h). Default is 0.1.
#' @param kmax Maximum threshold value for SSKS (mm/h). Default is 250.
#' @param output Type of output to be returned. Options are 'data', 'summary', 'bootmean', 'mean', 'bootmedian', 'median', 'min', 'max', and 'sd'. Default is 'bootmean'.
#'
#SPOR <- 48 # Total Soil porosity (%)
#DEPTH <- 20   # (cms)
#GWCFC <- 25.5 #(%)
#'
#' @returns Returns a list or a numeric value of SSKS (mm/h) based on the 'output' parameter. If 'output' is 'data', it returns the complete dataset. If 'output' is 'summary', it returns the summary statistics of SSKS values. If 'output' is any of the other specified types, it returns the corresponding statistic.
#'
#' @examples
#' # Function Arguments
#' S <- 28
#' C = 15.7
#' SOM <- 2
#' SBDM = 1.5
#'
#' # SSKS multi PTF
#' SSKS_cal(S, C)
#' SSKS_cal(S, C, SOM, SBDM)
#' SSKS_cal(S, C, output = 'summary')
#' SSKS_cal(S = 28.3, C = 15.7, output = 'mean')
#'
#'#' #For specific SSKS PTF:
#' SSKS_Saxton(S, C, SOM, SBDM)
#' SSKS_Suleiman_Ritchie(S, C, SOM, SBDM)
#'
#' @references
#' SSKS Pedotransfer Functions:
#'
#' - Brakensiek et al. (1984) for SSKS_Brakensiek
#' - Campbell and Shiozawa (1994) for SSKS_Campbell
#' - Cosby et al. (1984) for SSKS_Cosby
#' - Dane and Puckett (1994) for SSKS_Dane_Puck
#' - Ferrer Julià et al. (2004) for SSKS_Ferrer
#' - Jabro (1992) for SSKS_Jabro
#' - Puckett et al. (1985) for SSKS_Puckett
#' - RAWLS (1986) for SSKS_Rawls
#' - Saxton, K. E., & Rawls, W. J. (2006). for SSKS_Saxton.
#' - Suleiman, A. A., & Ritchie, J. T. (2001). for SSKS_Suleiman_Ritchie
#' - Vereecken et al. (1990) for SSKS_Vereecken
#' - Wösten et al. (1999) for SSKS_Wosten99
#'
#' @export
SSKS_cal <- function(S, C, SOM=1.5, SBDM=1.5, kmin=0.1, kmax=250, output = 'bootmean') {


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
    dplyr::summarise(
      ssks_bootmean = mean(sample(SSKS, 1000, replace = T)),
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
# Saturated hydraulic conductivity Functions (SSKS) ----
# SSKS_Brakensiek(S, C, SBDM)
# SSKS_Brakensiek(S, C, SPOR = 42)

# Brakensiek et al. (1984) SPOR = %

#  Pachepsky, Y., & Rawls, W. J. (Eds.). (2004). Development of pedotransfer functions in soil hydrology
#  (Vol. 30). Elsevier.https://www.elsevier.com/books/development-of-pedotransfer-functions-in-soil-hydrology/pachepsky/978-0-444-51705-0

#  Ghanbarian, B., Taslimitehrani, V., & Pachepsky, Y. A. (2017). Accuracy of sample dimension-dependent pedotransfer functions in
#  estimation of soil saturated hydraulic conductivity. Catena, 149, 374-380. https://doi.org/10.1016/j.catena.2016.10.015

#  Ferrer Julià, M., T.E. Monreal, A.S. del Corral Jiménez, and E. García Meléndez. 2004. Constructing a saturated hydraulic conductivity
#  map of Spain using pedotransfer functions and spatial prediction. Geoderma 123:257–277. doi:10.1016/j.geoderma.2004.02.011

#' @rdname SSKS_cal
#' @param SPOR Total Soil porosity (%)
#' @usage NULL
SSKS_Brakensiek <- function(S,C, SBDM, SPOR=NULL) {
    #SPOR = as fraction
    if(is.null(SPOR)){
      #        message("Porosity was estimated using 2.65g/cm3 as particle density")
      SPOR <- (1-(SBDM/2.65))*100
    }
    SSKS_Br <- 24*exp(19.52348*(SPOR/100) - 8.96847 - 0.028212*C + 0.00018107*(S^2) - 0.0094125*(C^2) - 8.395215*((SPOR/100)^2) + 0.077718*((SPOR/100)*S) - 0.00298*((SPOR/100)^2)*(S^2) -0.019492*((SPOR/100)^2)*(C^2) + 0.0000173*(C*S^2) + 0.02733*((SPOR/100)*C^2) + 0.001434*((SPOR/100)*S^2)- 0.0000035*(S*C^2))
    SSKS_Br*10/24
}


# Campbell and Shiozawa (1994)  #Error Si by S
# SSKS_Campbell(S, C)
#' @rdname SSKS_cal
#' @export
#' @usage NULL
SSKS_Campbell <- function(S,C){
  SSKS_Ca <- 129.6*exp(-0.07*S - 0.167*C)
  return(SSKS_Ca*10/24)
}


# Cosby et al. (1984)
# SSKS_Cosby(S,C)
#' @rdname SSKS_cal
#' @export
#' @usage NULL
SSKS_Cosby <- function(S,C){
  SSKS_Co <- 60.96*10^((-0.6+0.01268*S) - (0.0064*C))
  return(SSKS_Co*10/24)
}


# Dane and Puckett (1994)
# SSKS_Dane_Puck(C)
#' @rdname SSKS_cal
#' @export
#' @usage NULL
SSKS_Dane_Puck <- function(C){
  SSKS_DP <- 729.22*exp(-0.144*C)
  return(SSKS_DP*10/24)
}


#Ferrer-Julia et al. (2004)
# SSKS_Ferrer(S)
#' @rdname SSKS_cal
#' @export
SSKS_Ferrer <- function(S) {
  SSKS_Fer <-  3600000* 2.556*(10^-7)*exp(0.0491*S)

  return(SSKS_Fer)
}


#Jabro (1992)
# SSKS_Jabro(S,C,SBDM)
#' @rdname SSKS_cal
#' @export
#' @usage NULL
SSKS_Jabro <- function(S, C, SBDM){
  Si <- 100-C-S
  SSKS_Ja <- exp(9.56 - 0.81*log(Si) - 1.09*log(C) - 4.64*SBDM)
  return(SSKS_Ja*10)
}


# Puckett et al. (1985)
# SSKS_Puckett(C)
#' @rdname SSKS_cal
#' @export
#' @usage NULL
SSKS_Puckett <- function(C){
  SSKS_Pu <- 376.7*exp(-0.1975*C)
  return(SSKS_Pu*10/24)
}


# RAWLS (1986)
# SSKS_Rawls(S,C)
#' @rdname SSKS_cal
#' @export
SSKS_Rawls <- function(S,C){
  SST_Rawls <- 0.332 - (7.251*(10^-4)*S) + 0.1276*log10(C)
  SSKS_R <- 24*exp(12.012 - (7.55*(10^-2)*S) + (-3.8950 + (0.03671*S) - (0.1103*C) + 8.7546*(10^-4)*(C^2))*(1/(SST_Rawls)))

  return(SSKS_R*10/24)
}


# Vereecken et al. (1990)
# SSKS_Vereecken(S, C, OM, SBDM)  ##verificar
#' @rdname SSKS_cal
#' @export
#' @usage NULL
SSKS_Vereecken <- function(S, C, SOM, SBDM) {
  x <- 20.62- 0.96*log(C)- 0.66*log(S) - 0.46*log(SOM) - 8.43*SBDM
  SSKS_Ve <- exp(x)*10/24

  return(SSKS_Ve)

}


# Wösten et al. (1999)
# Topsoil is a parameter that is set to 1 for topsoils and to 0 for subsoils,
# SSKS_Wosten99(S,C,OM,SBDM, tops=0)
#' @rdname SSKS_cal
#' @export
#' @usage NULL
SSKS_Wosten99 <- function(S,C,SOM,SBDM, tops=0) {
  Si=100-C-S
  x <- 7.755 + 0.0352*Si + 0.93*tops - 0.967*(SBDM^2)- 0.000484*(C^2) - 0.000322*(Si^2) +
    0.001/Si - 0.0748/SOM - 0.643*log(Si) - 0.01398*SBDM*SOM - 0.1673*SBDM*SOM +
    0.02986*tops*C - 0.03305*tops*Si

  SSKS_W <- 1.15741*(10^-7)*exp(x)*3600000  #conversion to mm/h
  return(SSKS_W)
}



# Relative Effective Porosity Model (REPM) ----
# Suleiman, A. A., & Ritchie, J. T. (2001).

#  Suleiman, A. A., & Ritchie, J. T. (2001). Estimating saturated hydraulic conductivity from soil porosity.
#  Transactions of the ASAE, 44(2), 235. https://doi.org/10.13031/2013.4683

# cited by:
#  Gijsman, A. J., Thornton, P. K., & Hoogenboom, G. (2007). Using the WISE database to parameterize soil inputs for crop simulation models.
#  Computers and Electronics in Agriculture, 56(2), 85-100. https://doi.org/10.1016/j.compag.2007.01.001

#' @rdname SSKS_cal
#' @param WCFC Water Content at Field Capacity (%) - (-33kPa)
#' @export
SSKS_Suleiman_Ritchie <- function(S, C, SOM, SBDM, SPOR=NULL, WCFC=NULL){
  #    stopifnot(SPOR)
  if(is.null(WCFC)){
    message("WCFC was estimated using Saxton-PTF")
    WCFC <- WCFC_Saxton(S,C,SOM)}

  if(is.null(SPOR)){
    message("Porosity was estimated using 2.65g/cm3 as particle density")
    SPOR <- (1-(SBDM/2.65))*100
  }

  SSKS <- 75*((SPOR-WCFC)^2/(WCFC)^2)

  return(SSKS*10/24)

}
#SSKS_Suleiman_Ritchie(S,C,SOM, SBDM)
#SSKS_Suleiman_Ritchie(SPOR = 42, WCFC = 24)


# Saturated conductivity (matric soil), mm/h
#' @rdname SSKS_cal
#' @param WCWP Water Content at Wilting Point (%) - (-1500 kPa)
#' @param WCST Water Content at Saturation (%) - (-10kPa)
#' @export
SSKS_Saxton <- function(S, C, SOM, SBDM, WCFC=NULL, WCWP=NULL, WCST=NULL){
  if(is.null(WCFC)){
    #        message("Water Content PTF used")
    WCFC <- WCFC_Saxton(S,C,SOM)}



  if(is.null(WCWP)){
    #        message("Water Content PTF used")
    WCWP <- WCWP_Saxton(S,C,SOM)}
  if(is.null(WCST)){
    #        message("Water Content PTF used")
    WCST <- WCST_Saxton(SBDM)}

  B <- (log(1500) - log(33))/(log(WCFC/100) - log(WCWP/100))  #Eq 15
  alp <- 1/B                #Eq 18

  SSKS <- 1930*abs((WCST/100 - WCFC/100))^(3-alp)   # Eq 16

  return(SSKS)

}
#SSKS_Saxton(S,C,SOM, SBDM)
#SSKS_Saxton(S,C,SOM, SBDM, WCFC, WCWP)
#SSKS_Saxton(S,C,SOM, SBDM, WCFC)
#SSKS_Saxton(WCFC = 22, WCWP = 10, WCST = 55)


















