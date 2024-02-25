#' General information and Agronomic Data
#'
#' This dataset encompasses comprehensive agronomic and general information about specific experimental trials aimed at crop modeling, serving as the master table for the workbook proposed in the agroclimR package. It provides a foundation for advanced analyses in agricultural research, detailing trial identification, geographic locations, crop varieties, agronomic treatments, and other pertinent variables.
#'
#' @title General information and Agronomic Data
#'
#' @description A dataset for detailed agronomic and general information on experimental trials for crop modeling. It is the master table for the workbook proposed in the agroclimR package.
#'
#' @format A \code{\link[data.frame]{data.frame}} with 7 rows and 14 columns:
#' \describe{
#'   \item{ID}{Trial ID - (\code{character}). A unique identifier for each trial. Example: \code{"LOC1T1PROJ1"}.}
#'   \item{LOC_ID}{Locality ID - (\code{character}). Indicates the location of the trial. Example: \code{"LOC1"}.}
#'   \item{PROJECT}{Project ID - (\code{character}). Associates the trial with a specific project. Example: \code{"PROJ1"}.}
#'   \item{CULTIVAR}{Cultivar name - (\code{character}). Specifies the plant cultivar used in the trial. Example: \code{"CULTIVAR1"}.}
#'   \item{TR_N}{Treatment number - (\code{character}). Example: \code{"T1"}.}
#'   \item{LAT}{Latitude in decimal degrees - (\code{numeric}).}
#'   \item{LONG}{Longitude in decimal degrees - (\code{numeric}).}
#'   \item{ALT}{Elevation in meters above sea level - (\code{numeric}).}
#'   \item{PDAT}{Planting date in MM/DD/YYYY format - (\code{date}).}
#'   \item{CROP_SYS}{Crop system - (\code{character}). Example: \code{"IRRIGATED-RAINFED"}.}
#'   \item{ESTAB}{Establishment method - (\code{character}). Example: \code{"TRANSPLANT-DIRECT-SEED"}.}
#'   \item{NPLDS}{Number of plants per square meter - (\code{numeric}). Example: \code{number/m²}.}
#'   \item{SBDUR}{Seed-bed duration in days - (\code{numeric}).}
#'   \item{TRDAT}{Transplanting date in MM/DD/YYYY format - (\code{date}).}
#' }
#' @source \code{\link[agroclimR]{Rodriguez-Espinoza J, (2024)}} and \code{CIAT-MADR-FEDEARROZ, (2016)}
#' @examples
#' # Assuming `agro` is your dataset name
#' head(agro)
"agro"
