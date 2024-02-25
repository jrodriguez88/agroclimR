#' Observed Plant Phenological Data
#'
#' This dataset encompasses observed plant phenological dates for crop modeling trials. It provides crucial milestones in the growth cycle of plants (based on rice cultivars), including dates of emergence, panicle initiation, flowering, and maturity. These phenological markers are essential for understanding crop development stages, assessing the impact of agronomic practices, and guiding management decisions in rice cultivation.
#'
#' @title Observed Plant Phenological Data
#'
#' @description A dataset containing key phenological dates for plant cultivars used in crop modeling trials. Each record details the phenological stages of rice growth (similar for others crops).
#'
#' @format A \code{\link[data.frame]{data.frame}} with 7 rows and 8 columns:
#' \describe{
#'   \item{ID}{Trial ID - (\code{character}). Serves as a unique identifier for each trial. Example: \code{"LOC1T1PROJ1"}.}
#'   \item{LOC_ID}{Locality ID - (\code{character}). Denotes the trial's location. Example: \code{"LOC1"}.}
#'   \item{CULTIVAR}{Cultivar name - (\code{character}). Identifies the cultivar used in the trial. Example: \code{"CULTIVAR1"}.}
#'   \item{EDAT}{Emergence date - (\code{date}). The date when the plants emerged. Format: MM/DD/YYYY.}
#'   \item{IDAT}{Panicle initiation date - (\code{date}). The date when panicle initiation was observed. Format: MM/DD/YYYY.}
#'   \item{FDAT}{Flowering date - (\code{date}). The date when flowering occurred. Format: MM/DD/YYYY.}
#'   \item{MDAT}{Maturity date - (\code{date}). The date when the plants reached maturity. Format: MM/DD/YYYY.}
#' }
#' @source \code{\link[agroclimR]{Rodriguez-Espinoza J, (2024)}} and \code{CIAT-MADR-FEDEARROZ, (2016)}
#' @examples
#' # Assuming `phenol` is your dataset name
#' head(phenol)
"phenol"
