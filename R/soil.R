#' Observed Soil Data
#'
#' This dataset contains comprehensive chemical and physical soil data from the same experimental trials used in crop modeling within the agroclimR package framework. It includes details on soil layers, texture, content, and other vital soil properties, essential for understanding soil characteristics that influence crop growth and development.
#'
#' @title Observed Soil Data
#'
#' @description A dataset of observed soil data providing detailed chemical and physical properties of soil from experimental trials. These observations are critical for crop modeling, allowing for an in-depth analysis of soil conditions that affect agricultural productivity.
#'
#' @format A \code{\link[data.frame]{data.frame}} with 18 rows and 22 columns:
#' \describe{
#'   \item{ID}{Trial ID - (\code{character}). A unique identifier for each trial. Example: \code{"LOC1T1PROJ1"}.}
#'   \item{LOC_ID}{Locality ID - (\code{character}). Indicates the location of the trial. Example: \code{"LOC1"}.}
#'   \item{SAMPLING_DATE}{Sampling date - (\code{date}). The date when the plant sampling was conducted, in MM/DD/YYYY format.}
#'   \item{NL}{Number of soil layers - (\code{numeric}). Indicates the maximum layers considered, up to 10.}
#'   \item{DEPTH}{Thickness of each soil layer - (\code{numeric}). Measured in centimeters (\code{cm}).}
#'   \item{STC}{Soil Texture Class (12-USDA) - (\code{character}). Represents the USDA texture class.}
#'   \item{SAND}{Soil sand content - (\code{numeric}). Percentage of sand content (%).}
#'   \item{SILT}{Soil silt content - (\code{numeric}). Percentage of silt content (%).}
#'   \item{CLAY}{Soil clay content - (\code{numeric}). Percentage of clay content (%).}
#'   \item{SBDM}{Soil Bulk Density - (\code{numeric}). Measured in grams per cubic centimeter (\code{g/cm³}).}
#'   \item{SOC}{Soil organic carbon - (\code{numeric}). Measured in grams per kilogram (\code{g/kg}).}
#'   \item{SLON}{Soil Organic Nitrogen - (\code{numeric}). Measured in milligrams per kilogram (\code{mg/kg}).}
#'   \item{SNH4}{Ammonium (KCl, elemental N) - (\code{numeric}). Measured in milligrams per kilogram (\code{mg/kg}).}
#'   \item{SNO3}{Nitrate (KCl, elemental N) - (\code{numeric}). Measured in milligrams per kilogram (\code{mg/kg}).}
#'   \item{PH}{pH - (\code{numeric}). A measure of soil acidity or alkalinity.}
#'   \item{SCEC}{Cation exchange capacity - (\code{numeric}). Measured in centimoles per kilogram (\code{cmol/kg}).}
#'   \item{WCST}{Saturated volumetric water content - (\code{numeric}). Percentage of water content when soil is saturated (%).}
#'   \item{WCFC}{Volumetric water content at field capacity - (\code{numeric}). Percentage of water content at field capacity (%).}
#'   \item{WCWP}{Volumetric water content at wilting point - (\code{numeric}). Percentage of water content at wilting point (%).}
#'   \item{WCAD}{Volumetric water content at air dryness - (\code{numeric}). Percentage of water content when soil is air dry (%).}
#'   \item{SSKS}{Saturated hydraulic conductivity - (\code{numeric}). Measured in milimeters per hour (\code{mm/h}).}
#' }
#' @source \code{\link[agroclimR]{Rodriguez-Espinoza J, (2024)}} and \code{CIAT-MADR-FEDEARROZ, (2016)}
#' @examples
#' # Assuming `soil` is your dataset name
#' summary(soil)
"soil"
