#' Calculate Soil Texture Class
#'
#' This function calculates the soil texture class based on the percentages of sand and clay
#' using the specified soil classification system (default is USDA).
#'
#' @param S Numeric vector indicating the percentage of sand in the soil.
#' @param C Numeric vector indicating the percentage of clay in the soil.
#' @param sysclass Character string specifying the soil classification system to use.
#'        Default is "USDA". Other systems require the optional `soiltexture`
#'        package.
#' @returns String with the calculated soil texture class name, based on the input percentages of sand, clay, and silt.
#' @export
#' @examples
#' # Calculate the soil texture class for a soil with 30% sand and 20% clay using USDA system
#' get_STC(30, 20)
#' @note The USDA classifier is implemented internally to avoid loading GUI
#'       dependencies during package checks. The optional `soiltexture` package
#'       is used only when `sysclass` is not `"USDA"`.
#' @references
#' Soil Texture, The Soil Texture Wizard: <URL to soiltexture or related documentation>

get_STC <- function(S, C, sysclass="USDA") {
  stopifnot(is.numeric(S), is.numeric(C), length(S) == length(C))

  # Calculate the percentage of silt as the remainder to 100% from sand and clay percentages
  Si <- 100 - (S + C)

  if (identical(toupper(sysclass), "USDA")) {
    return(usda_texture_class(S, C, Si))
  }

  if (!requireNamespace("soiltexture", quietly = TRUE)) {
    stop(
      "The 'soiltexture' package is required when sysclass is not 'USDA'.",
      call. = FALSE
    )
  }

  # Prepare the data for texture triangle plotting
  dat <- data.frame(SAND = S, CLAY = C, SILT = Si)

  # Determine the soil texture class using the specified classification system
  STC <- soiltexture::TT.points.in.classes(
    tri.data = dat,
    class.sys = paste0(sysclass, ".TT"),
    PiC.type = "t"
  )

  return(STC)
}

usda_texture_class <- function(S, C, Si) {
  out <- rep(NA_character_, length(S))
  valid <- !is.na(S) & !is.na(C) & !is.na(Si) & S >= 0 & C >= 0 & Si >= 0

  out[valid & S >= 85 & (Si + 1.5 * C) < 15] <- "Sa"
  out[valid & is.na(out) &
        ((S >= 70 & S < 90 & (Si + 2 * C) < 30) |
           (S >= 85 & (Si + 1.5 * C) >= 15))] <- "LoSa"
  out[valid & is.na(out) &
        ((C >= 7 & C < 20 & S > 52 & (Si + 2 * C) >= 30) |
           (C < 7 & Si < 50 & (Si + 2 * C) >= 30) |
           (C >= 20 & C < 35 & S > 45 & Si < 28))] <- "SaLo"
  out[valid & is.na(out) & C >= 7 & C < 27 & Si >= 28 & Si < 50 & S <= 52] <- "Lo"
  out[valid & is.na(out) &
        ((Si >= 50 & C >= 12 & C < 27) |
           (Si >= 50 & Si < 80 & C < 12))] <- "SiLo"
  out[valid & is.na(out) & Si >= 80 & C < 12] <- "Si"
  out[valid & is.na(out) & C >= 20 & C < 35 & Si >= 28 & S <= 45] <- "ClLo"
  out[valid & is.na(out) & C >= 27 & C < 40 & S > 20 & S <= 45] <- "ClLo"
  out[valid & is.na(out) & C >= 27 & C < 40 & S <= 20] <- "SiClLo"
  out[valid & is.na(out) & C >= 35 & S > 45] <- "SaCl"
  out[valid & is.na(out) & C >= 40 & Si >= 40] <- "SiCl"
  out[valid & is.na(out) & C >= 40 & S <= 45 & Si < 40] <- "Cl"

  out
}
