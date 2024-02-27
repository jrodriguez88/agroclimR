#' Observed Weather Data
#'
#' This dataset encompasses daily observed weather data collected from the FEDEARROZ network of Davis Vantage Pro2 meteorological stations. It is designed to provide researchers and practitioners with detailed environmental measurements critical for agricultural research, weather analysis, and climatology studies.
#'
#' @title Observed Weather Data
#'
#' @description A dataset capturing daily weather observations. It includes data on temperature, precipitation, irradiance, relative humidity, and wind speed, collected from the FEDEARROZ network of meteorological stations.
#'
#' @format A \code{\link[data.frame]{data.frame}} with 1461 rows and 7 columns:
#' \describe{
#'   \item{LOC_ID}{Locality ID - (\code{character}). Identifies the location of the trial. Example: \code{"LOC1"}.}
#'   \item{WS_ID}{Weather Station ID - (\code{integer}). Identifies the number of weather station. Example: \code{"1"}.}
#'   \item{DATE}{Date of observation - (\code{Date}). Captures daily dates of weather observations.}
#'   \item{TMAX}{Maximum temperature observed - (\code{numeric}). Values are in degrees Celsius (\code{oC}).}
#'   \item{TMIN}{Minimum temperature observed - (\code{numeric}). Values are in degrees Celsius (\code{oC}).}
#'   \item{RAIN}{Precipitation amount - (\code{numeric}). Values are in millimeters per day (\code{mm d-1}).}
#'   \item{SRAD}{Solar irradiance - (\code{numeric}). Values are in Megajoules per square meter per day (\code{MJ m-2 d-1}).}
#'   \item{RHUM}{Relative humidity - (\code{numeric}). Percentage values (%).}
#'   \item{WSPD}{Wind speed - (\code{numeric}). Values are in meters per second (\code{m/s}).}
#' }
#' @source \code{\link[agroclimR]{Rodriguez-Espinoza J, (2024)}} and \code{CIAT-MADR-FEDEARROZ, (2016)}
#' @examples
#' # Assuming `weather` is your dataset name
#' head(weather)
"weather"
