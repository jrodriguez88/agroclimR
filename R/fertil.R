#' Observed Fertilization Plan Data
#'
#' This dataset contains detailed information on fertilization management or application for each experimental trial identified with an ID in the workbook of the agroclimR package. It is designed to provide insights into the nutrient management practices applied across different trials, facilitating research in agricultural productivity and sustainability.
#'
#' @title Observed Fertilization Plan Data
#'
#' @description A dataset detailing fertilization management or application data across various experimental trials for research in agricultural productivity and sustainability. It includes information on trial identification, locality, project IDs, and specific details on fertilizer applications.
#'
#' @format A \code{\link[data.frame]{data.frame}} with 37 rows and 7 columns:
#' \describe{
#'   \item{ID}{Trial ID - (\code{character}). Serves as a unique identifier for each trial. Example: \code{"LOC1T1PROJ1"}.}
#'   \item{LOC_ID}{Locality ID - (\code{character}). Identifies the location of the trial. Example: \code{"LOC1"}.}
#'   \item{PROJECT}{Project ID - (\code{character}). Associates the trial with a specific project. Example: \code{"PROJ1"}.}
#'   \item{FERT_No}{Fertilizer application number - (\code{numeric}). Indicates the sequence number of the fertilizer application.}
#'   \item{DDE}{Days after Emergence - (\code{numeric}). Specifies the number of days after crop emergence when the fertilizer was applied.}
#'   \item{N}{Nitrogen - (\code{numeric}). Amount of nitrogen applied, in kilograms per hectare (\code{kg/ha}).}
#'   \item{P}{Phosphate - (\code{numeric}). Amount of phosphate applied, in kilograms per hectare (\code{kg/ha}).}
#'   \item{K}{Potassium - (\code{numeric}). Amount of potassium applied, in kilograms per hectare (\code{kg/ha}).}
#' }
#' @source \code{\link[agroclimR]{Rodriguez-Espinoza J, (2024)}} and \code{CIAT-MADR-FEDEARROZ, (2016)}
#' @examples
#' # Assuming `fertil` is your dataset name
#' head(fertil)
"fertil"
