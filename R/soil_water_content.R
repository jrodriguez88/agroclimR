#' Soil Pedotransfer Function (PTF) to estimate Soil Water Content (WC -v/v (%))
#'
#' This functions calculates the Saturated Hydraulic Conductivity (SSKS)
#' based on various soil pedotransfer functions (PTFs) using soil properties as input parameters.
#'
#' @param S Sand content (%) of the soil.
#' @param C Clay content (%) of the soil.
#' @param SOM Soil Organic Matter content (%) of the soil. Default is 1.5.
#' @param SBDM Soil Bulk Density (g/cm3) of the soil. Default is 1.5.
#' @param SPOR Total Soil porosity (%)
#' @param GWCFC Gravimetric Water Content at Field Capacity (%)
#'
#' @returns Returns a list or a numeric value of SSKS (mm/h) based on the 'output' parameter. If 'output' is 'data', it returns the complete dataset. If 'output' is 'summary', it returns the summary statistics of SSKS values. If 'output' is any of the other specified types, it returns the corresponding statistic.
#'
#' @examples
#' # Function Arguments
#' S <- 28
#' C <- 15.7
#' SOM <- 2
#' SBDM <- 1.5
#' GWCFC <- 25
#'
#' WCFC_Saxton(S, C, SOM)
#'
#' WCST_Tomasella(S, C, SBDM, GWCFC)
#'
#' WCWP_Minasny(C, SOM)
#'
#' @references
#' - Saxton, K. E., & Rawls, W. J. (2006). Soil Water Characteristic Estimates by Texture and
#'  Organic Matter for Hydrologic Solutions. Soil Science Society of America Journal,
#'  70(5), 1569–1578. https://doi.org/10.2136/sssaj2005.0117
#'
#' - Minasny, B., & Hartemink, A. E. (2011). Predicting soil properties in the tropics.
#'  Earth-Science Reviews, 106(1-2), 52-62. https://doi.org/10.1016/j.earscirev.2011.01.005
#'
#' - Tomasella, J., & Hodnett, M. (2004). Pedotransfer functions for tropical soils.
#' Developments in Soil Science, 30, 415-429. https://doi.org/10.1016/S0166-2481(04)30021-8
#'
#' - Pachepsky, Y., & Rawls, W. J. (Eds.). (2004). Development of pedotransfer functions in soil hydrology
#' (Vol. 30). Elsevier. https://www.elsevier.com/books/development-of-pedotransfer-functions-in-soil-hydrology/pachepsky/978-0-444-51705-0
#'
#' - Ghanbarian, B., Taslimitehrani, V., & Pachepsky, Y. A. (2017). Accuracy of sample dimension-dependent pedotransfer functions in
#' estimation of soil saturated hydraulic conductivity. Catena, 149, 374-380. https://doi.org/10.1016/j.catena.2016.10.015
#'
#' - Ferrer Julià, M., T.E. Monreal, A.S. del Corral Jiménez, and E. García Meléndez. 2004. Constructing a saturated hydraulic conductivity
#' map of Spain using pedotransfer functions and spatial prediction. Geoderma 123:257–277. doi:10.1016/j.geoderma.2004.02.011
#'
#' - Suleiman, A. A., & Ritchie, J. T. (2001). Estimating saturated hydraulic conductivity from soil porosity.
#' Transactions of the ASAE, 44(2), 235. https://doi.org/10.13031/2013.4683
#' @rdname soil_water_content
#' @export
#'

# SAXTON & RAWLS: SOIL WATER CHARACTERISTICS ESTIMATES ----
##### http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.452.9733&rep=rep1&type=pdf
# https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=9601aadd2f5ef665a87248aa1563056c66da9f4d

#  Saxton, K. E., & Rawls, W. J. (2006). Soil Water Characteristic Estimates by Texture and
#  Organic Matter for Hydrologic Solutions. Soil Science Society of America Journal,
#  70(5), 1569–1578. https://doi.org/10.2136/sssaj2005.0117


# 1500 kPa moisture, %v
WCWP_Saxton <- function(S, C, SOM) {
  WCWP_i <- -0.024*S/100 + 0.487*C/100 + 0.006*SOM +
    0.005*(S*SOM/100) - 0.013*(C*SOM/100) +
    0.068 *(S/100)*(C/100) + 0.031

  WCWP <- WCWP_i + (0.14*WCWP_i - 0.02)

  return(WCWP*100)
} #Eq 1
#WCWP_Saxton(S, C,SOM)

# 33 kPa moisture, normal density, %v
#' @rdname soil_water_content
#' @export
WCFC_Saxton <- function(S,C,SOM) {
  WCFC_i <- -0.251*S/100 + 0.195*C/100 + 0.011*SOM +
    0.006*(S*SOM/100) - 0.027*(C*SOM/100) +
    0.452*(S/100)*(C/100) + 0.299

  WCFC <- WCFC_i + (1.283*(WCFC_i^2) - (0.374*WCFC_i) - 0.015)

  return(WCFC*100)

}   #Eq 2
#WCFC_Saxton(S, C,SOM)

# SAT-33 kPa moisture, normal density %v
WCSAT_Saxton <- function(S,C,SOM) {

  WCST_i <- 0.278*(S/100) + 0.034*(C/100) + 0.022*SOM -
    0.018*(S/100)*SOM - 0.027*(C/100)*SOM +
    0.584 *(S/100)*(C/100) + 0.078

  WCSAT <- WCST_i + (0.6360*WCST_i) - 0.107

  return(WCSAT*100)

}  #Eq 3
#WCSAT_Saxton(S,C,SOM)

# Tension at air entry (bubbling pressure), kPa
TAE_BP_Saxton <- function(S,C,SOM) {
  WCSAT <- WCSAT_Saxton(S,C,SOM)
  TAE_BPi <- -2.27 -(27.93*C/100) -(81.97*WCSAT) + (71.12*S*WCSAT/100) + (8.29*C*WCSAT/100) + (14.05*(S/100)*(C/100)) + 27.16

  TAE_BP <- TAE_BPi + (0.02*(TAE_BPi^2) -0.113*TAE_BPi -0.70)

  return(TAE_BP)

} #Eq 4
#TAE_BP_Saxton(S,C,SOM)

# Saturated moisture (0 kPa), normal density, %v
WCST_Saxton1 <- function(S,C,SOM) {

  WCFC <- WCFC_Saxton(S,C,SOM)
  WCSAT <- WCSAT_Saxton(S,C,SOM)
  WCST <- WCFC + WCSAT -0.097*(S/100) + 0.043

  return(WCST)

}   #Eq 5
#WCST_Saxton1(S,C,SOM)

# pN Normal density, g/cm³
pN_Saxton <- function(S,C,SOM) {
  WCST <- WCST_Saxton1(S,C,SOM)
  pN <- (1 - WCST)*2.65
  return(pN)
}
#pN_Saxton(S,C,SOM)
#pDF <- pN_cal(S,C,SOM) #DF Compactation soil factor (0.9-1.3)
#pB <- pN

# Saturated moisture (0 kPa), adjusted density, %v
#' @rdname soil_water_content
#' @export
WCST_Saxton <- function(SBDM){

  WCST_S <- 1-(SBDM/2.65)
  return(WCST_S*100)
}    #Eq 8
#WCST_Saxton(SBDM)


# PEDOTRANSFER FUNCTIONS FOR TROPICAL SOILS Cap21
# J. Tomasella, and M. Hodnett (2004) ----

#  Tomasella, J., & Hodnett, M. (2004). Pedotransfer functions for tropical soils.
#  Developments in Soil Science, 30, 415-429. https://doi.org/10.1016/S0166-2481(04)30021-8

# Alpha van Genuchten parameter
Alpha_Tomasella <- function(S, C, SBDM, CS, FS, GWCFC) {
  Si <- 100-C-S
  x1 <- -1.06790 + 0.0536107*CS  #CS Coarse sand
  x2 <- -1.17468 + 0.0808098*FS  #FS Fine sand
  x3 <- -1.05976 + 0.0650437*Si
  x4 <- -2.10641 + 0.0427715*C
  x5 <- -2.21391 + 8.9226800*GWCFC

  x6 <- -6.03516 + 4.81197*SMBD
  z1 <-  4.25417*x1 + 2.72322*x2 + 3.07242*x3 + 5.00093*x4 - 0.195062*x5 - 0.377081*x6
  z2 <- 0.110144 + 0.640373*z1 - 1.16884*(z1)^2 - 0.155394*x4 - 0.358591*z1*x4 - 1.00996*x4*(z1)^2 + 0.126617*(x4)^3

  alpha_T <-  10^(0.0736768 + 0.789068*z2)

  return(alpha_T)
}

# N van Genuchten parameter
N_Tomasella <-  function(S, C, SBDM, CS, FS, GWCFC) {
  Si <- 100-C-S
  x1 <- -1.06790 + 0.0536107*CS  #CS Coarse sand
  x2 <- -1.17468 + 0.0808098*FS  #FS Fine sand
  x3 <- -1.05976 + 0.0650437*Si
  x4 <- -2.10641 + 0.0427715*C
  x5 <- -2.21391 + 8.9226800*GWCFC
  x6 <- -6.03516 + 4.81197*SMBD

  z3 <- 0.37398*x1 - 0.0940338*(x1)^3 + 0.838535*x1*x5 - 0.590525*x5*(x1)^2 + 0.76113*(x5)^2 -
    0.789465*x1*(x5)^2 - 0.273647*(x5)^3 - 0.512764*x6 + 0.455363*x1*x6 - 0.38428*x6*(x1)^2 +
    0.731809*x5*x6 - 1.00484*x1*x5*x6 - 0.172341*x6*(x5)^2 + 0.219746*(x6)^2 - 0.367679*x1*(x6)^2 + 0.131251*(x6)^3

  z4 <- -0.360294 + 0.76878*z3 + 0.0770122*(z3)^3 - 0.193142*x2 - 0.121583*z3*x2 + 0.0889415*x2*(z3)^2 +
    0.284168*(x2)^2 - 0.0674767*(x2)^3 - 0.202897*x3 - 0.341951*z3*x3 - 0.270616*x2*x3 + 0.0880845*x3*(x2)^2 +
    0.24982*(x3)^2 + 0.102658*x2*(x3)^2 - 0.0801841*(x3)^3

  n_vg <- 10^(0.140543 + 0.0797516*z4)

  return(n_vg)
}

# van Genuchten parameter
WC0_Tomasella <- function(S, C, CS, SBDM) {
  Si <- 100-C-S
  x1 <- -1.06790 + 0.0536107*CS  #CS Coarse sand
  x3 <- -1.05976 + 0.0650437*Si
  x6 <- -6.03516 + 4.81197*SBDM

  z5 <- 0.164417 + 0.126139*(x1)^2 + 0.281797*x3 + 0.484823*x1*x3 - 0.293866*(x3)^2 - 0.354924*x1*(x3)^2 -
    0.705803*x6 - 0.189153*x3*x6 - 0.267997*x1*x3*x6 - 0.023954*x6*(x3)^2 - 0.0918816*x1*(x6)^2 + 0.0323997*(x6)^3

  WC_0 <- 0.515224 + 0.100899*z5

  return(WC_0)
}

# Residual water content
WCR_Tomasella <- function(S, C, GWCFC){
  Si <- 100-C-S
  x3 <- -1.05976 + 0.0650437*Si
  x5 <- -2.21391 + 8.9226800*GWCFC/100

  z6 <- 0.12867 - 0.492412*x3 + 0.787425*x5 - 0.235254*x3*x5

  WC_r <- 0.161487 + 0.101111*z6

  return(WC_r*100)
}
#WCR_Tomasella(S,C, GWCFC)

# Saturated water content at (0kPa), may consider WCST
#' @rdname soil_water_content
#' @export
#' @usage NULL
WCST0_Tomasella <- function(C, S, SBDM){
  x7 <- -1.0553 + 0.0533922*C*S
  x8 <- -1.07131 + 0.0649731*S
  x9 <- -6.18145 + 4.95385*SBDM
  z7 <- 0.159379 + 0.137397*(x7^2) + 0.265398*x8 + 0.519965*x7*x8 - 0.276027*(x8^2) - 0.362393*x7*(x8^2) - 0.702969*x9 - 0.222252*x8*x9 - 0.244634*x7*x8*x9 - 0.092267*x7*(x9^2) + 0.0332669*(x9^3)

  WCST_T1 <- 0.517589 + 0.0994301*z7
  return(WCST_T1*100)
}
#WCST_Tomasella(C, S, SBDM)

# Saturated water content at (-10kPa), may consider WCST
#' @rdname soil_water_content
#' @export
WCST_Tomasella <- function(S, C, SBDM, GWCFC) {
  Si <- 100-C-S
  x14 <- -1.05501 + 0.0650857*Si
  x15 <- -2.07588 + 0.0423954*C
  x16 <- -6.03402 + 4.80572*SBDM
  x17 <- -2.18409 + 8.84963*GWCFC/100
  z9 <- 0.175202 + 1.818513*x17 - 0.0996042*(x17^2) + 0.327915*x16 - 0.0758657*(x16^2)
  z10 <- 0.929344*z9 + 0.132519*x14

  WC10_T1 <- 0.339255 + 0.112526*z10

  return(WC10_T1*100)
}
#WC10_Tomasella(S,C,SBDM, GWCFC)

#Saturated water content at (-33kPa)
#' @rdname soil_water_content
#' @export
WCFC_Tomasella <- function(SBDM, GWCFC) {
  x16 <- -6.03402 + 4.80572*SBDM
  x17 <- -2.18409 + 8.84963*GWCFC/100
  z11 <- 0.191452 + 1.25652*x17 - 0.079098*(x17^2) + 0.393814*x16 + 0.152095*x17*x16

  WCFC_T1 <- 0.28951 + 0.103815*z11

  return(WCFC_T1*100)
}
#WCFC_Tomasella(SBDM, GWCFC)

# Saturated water content at (-1500kPa)
#' @rdname soil_water_content
#' @export
WCWP_Tomasella <- function(C, SBDM, GWCFC){

  x15 <- -2.07588 + 0.0423954*C
  x16 <- -6.03402 + 4.80572*SBDM
  x17 <- -2.18409 + 8.84963*GWCFC/100
  z13 <- 0.235084 + 0.33033*x15 - 0.191838*(x15^2) + 0.0543679*(x15^3) + 0.977685*x17 + 0.304174*x15*x17 - 0.218857*(x17^2) - 0.164373*x15*(x17^2) + 0.0415057*(x17^3) + 0.373361*x16 + 0.0811861*x17*x16 - 0.0768087*x15*x17*x16

  WCWP_T1 <- 0.214008 + 0.0862945*z13
  return(WCWP_T1*100)
}
#WCWP_Tomasella(C, SBDM, GWCFC)



# Predicting Soil properties in the tropics
# Budiman Minasny, Alfred E. Hartemink. (2011) ----

#  Minasny, B., & Hartemink, A. E. (2011). Predicting soil properties in the tropics.
#  Earth-Science Reviews, 106(1-2), 52-62. https://doi.org/10.1016/j.earscirev.2011.01.005

#Soil bulk density with SOM effect
SDB_Minasny <- function(DEPTH, S, SOM, BD_OM=0.224){
  SBDmin <- 0.93 + (0.049*log(DEPTH)) + (0.005*S) + 0.000065*((S-38.96)^2)
  SBD_M <- 100/((SOM/BD_OM)+((100-SOM)/SBDmin))
  return(SBD_M)
} #Eq. 1 & 2

# Saturated water content at (-10kPa), may consider WCST
#' @rdname soil_water_content
#' @export
WCST_Minasny <- function(S, SBDM){
  WCST <- 59.9 - (8.78*SBDM) - (0.31*S)
  return(WCST)
}  #Eq. 3
#WCST_Minasny(S, SBDM)

#Saturated water content at (-33kPa)
#' @rdname soil_water_content
#' @export
WCFC_Minasny <- function(S, SBDM){
  WCFC <- 56.5 - (7.49*SBDM) -(0.34*S)
  return(WCFC)
}  #Eq. 4
#WCFC_Minasny(S, SBDM)

# Saturated water content at (-1500kPa)
#' @rdname soil_water_content
#' @export
WCWP_Minasny <- function(C, SOM){
  WCWP <- 7.95 + (0.86*SOM) + (0.4*C) - 0.004*((C-37.7)^2)
  return(WCWP)
}    #Eq. 5
#WCWP_Minasny(C, SOM)






















