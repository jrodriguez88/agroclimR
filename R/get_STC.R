#' Calculate Soil Texture Class
#'
#' This function calculates the soil texture class based on the percentages of sand and clay
#' using the specified soil classification system (default is USDA).
#'
#' @param S Numeric vector indicating the percentage of sand in the soil.
#' @param C Numeric vector indicating the percentage of clay in the soil.
#' @param sysclass Character string specifying the soil classification system to use.
#'        Default is "USDA", but can be changed if other systems are supported by the `soiltexture` package.
#' @returns String with the calculated soil texture class name, based on the input percentages of sand, clay, and silt.
#' @export
#' @examples
#' # Calculate the soil texture class for a soil with 30% sand and 20% clay using USDA system
#' get_STC(30, 20)
#' @importFrom soiltexture TT.points.in.classes
#' @note This function requires the `soiltexture` package to calculate the soil texture class.
#'       Ensure that the `soiltexture` package is installed and loaded into your R session.
#' @references
#' Soil Texture, The Soil Texture Wizard: <URL to soiltexture or related documentation>

get_STC <- function(S, C, sysclass="USDA") {
  #stopifnot(require(soiltexture))

  # Calculate the percentage of silt as the remainder to 100% from sand and clay percentages
  Si <- 100 - (S + C)

  # Prepare the data for texture triangle plotting
  dat <- data.frame(SAND = S, CLAY = C, SILT = Si)

  # Determine the soil texture class using the specified classification system
  STC <- TT.points.in.classes(
    tri.data = dat,
    class.sys = paste0(sysclass, ".TT"),
    PiC.type = "t"
  )

  return(STC)
}
