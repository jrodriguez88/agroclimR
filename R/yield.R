#' Observed Grain Yield Data
#'
#' This dataset presents observed grain yield and yield components data from crop modeling experimental trials. It includes detailed measurements of yield averages, minimum and maximum yields, harvest index, panicle fertility, grain weight, and other vital yield components. Such comprehensive data are essential for analyzing the performance of different cultivars under various agronomic practices and environmental conditions, facilitating advancements in crop yield optimization.
#'
#' @title Observed Grain Yield Data
#'
#' @description A dataset capturing detailed grain yield and yield components from experimental trials. These observations are critical for evaluating the effectiveness of cultivation practices and genetic performance across different rice cultivars.
#'
#' @format A \code{\link[data.frame]{data.frame}} with 7 rows and 20 columns:
#' \describe{
#'   \item{ID}{Trial ID - (\code{character}). Uniquely identifies each trial. Example: \code{"LOC1T1PROJ1"}.}
#'   \item{LOC_ID}{Locality ID - (\code{character}). Denotes the trial's location. Example: \code{"LOC1"}.}
#'   \item{CULTIVAR}{Cultivar name - (\code{character}). Identifies the cultivar used in the trial. Example: \code{"CULTIVAR1"}.}
#'   \item{YIELD_AVG}{Yield average - (\code{numeric}). The average yield, measured in kilograms per hectare (\code{kg/ha}).}
#'   \item{YIELD_MIN}{Yield minimum - (\code{numeric}). The minimum observed yield, in \code{kg/ha}.}
#'   \item{YIELD_MAX}{Yield maximum - (\code{numeric}). The maximum observed yield, in \code{kg/ha}.}
#'   \item{HIAM}{Harvest index at maturity - (\code{numeric}). The ratio of grain yield to total biomass at maturity.}
#'   \item{HIAM_SE}{Harvest index at maturity standard error - (\code{numeric}). Standard error of the harvest index at maturity.}
#'   \item{PAN_fert}{Panicle fertility - (\code{numeric}). Percentage of fertile panicles (%).}
#'   \item{PAN_fert_SE}{Panicle fertility standard error - (\code{numeric}). Standard error of the panicle fertility percentage (%).}
#'   \item{GW1000}{1000-Grain weight - (\code{numeric}). Weight of 1000 grains, measured in grams (\code{g}).}
#'   \item{GW1000_SE}{1000-Grain weight standard error - (\code{numeric}). Standard error of the 1000-grain weight (\code{g}).}
#'   \item{ST_M2}{Number of stems per square meter - (\code{numeric}). The density of stems, in \code{number/m²}.}
#'   \item{ST_M2_SE}{Number of stems per square meter standard error - (\code{numeric}). Standard error of stem density (\code{number/m²}).}
#'   \item{PAN_M2}{Number of panicles per square meter - (\code{numeric}). The density of panicles, in \code{number/m²}.}
#'   \item{PAN_M2_SE}{Number of panicles per square meter standard error - (\code{numeric}). Standard error of panicle density (\code{number/m²}).}
#'   \item{GT_PAN}{Number of total grains per panicle - (\code{numeric}). Total number of grains per panicle.}
#'   \item{GT_PAN_SE}{Number of total grains per panicle standard error - (\code{numeric}). Standard error of the total grains per panicle.}
#'   \item{GF_PAN}{Number of filled grains per panicle - (\code{numeric}). Number of filled grains per panicle.}
#'   \item{GF_PAN_SE}{Number of filled grains per panicle standard error - (\code{numeric}). Standard error of the filled grains per panicle.}
#' }
#' @source \code{\link[agroclimR]{Rodriguez-Espinoza J, (2024)}} and \code{CIAT-MADR-FEDEARROZ, (2016)}
#' @examples
#' # Assuming `yield` is your dataset name
#' summary(yield)
"yield"
