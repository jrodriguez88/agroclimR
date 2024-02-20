#' Observed Grain Yield Data
#'
#' A dataset containing observed grain yield and yield components.
#'
#' @format A data frame of 7 rows and 20 columns
#' \describe{
#'   \item{ID}{Trial ID}{character}{XXXX}
#'   \item{LOC_ID}{Locality ID}{character}{name}
#'   \item{CULTIVAR}{Cultivar name}{character}{name}
#'   \item{YIELD_AVG}{Yield average}{numeric}{kg/ha}
#'   \item{YIELD_MIN}{Yield minimum}{numeric}{kg/ha}
#'   \item{YIELD_MAX}{Yield maximum}{numeric}{kg/ha}
#'   \item{HIAM}{Harvest index at maturity}{numeric}{ratio}
#'   \item{HIAM_SE}{Harvest index at maturity standard error}{numeric}{ratio}
#'   \item{PAN_fert}{Panicle fertility}{numeric}{%}
#'   \item{PAN_fert_SE}{Panicle fertility standard error}{numeric}{%}
#'   \item{GW1000}{1000-Grain weight}{numeric}{g}
#'   \item{GW1000_SE}{1000-Grain weight standard error}{numeric}{g}
#'   \item{ST_M2}{Number of stems per square meter}{numeric}{number/m²}
#'   \item{ST_M2_SE}{Number of stems per square meter standard error}{numeric}{number/m²}
#'   \item{PAN_M2}{Number of panicles per square meter}{numeric}{number/m²}
#'   \item{PAN_M2_SE}{Number of panicles per square meter standard error}{numeric}{number/m²}
#'   \item{GT_PAN}{Number of total grains per panicle}{numeric}{number/panicle}
#'   \item{GT_PAN_SE}{Number of total grains per panicle standard error}{numeric}{number/panicle}
#'   \item{GF_PAN}{Number of filled grains per panicle}{numeric}{number/panicle}
#'   \item{GF_PAN_SE}{Number of filled grains per panicle standard error}{numeric}{number/panicle}
#' }
#' @source FEDEARROZ-FNA
"yield"
