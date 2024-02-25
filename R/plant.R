#' Observed Plant Growth Data
#'
#' This dataset contains observed plant growth data, including organ biomass or dry matter samplings, crucial for modeling and crop physiology studies. It provides a detailed view of plant growth dynamics across various trials, facilitating the analysis of crop development, yield potential, and the effects of agronomic treatments on plant growth.
#'
#' @title Observed Plant Growth Data
#'
#' @description A comprehensive dataset of observed plant growth metrics, including organ biomass and dry matter samplings. These measurements are integral for crop modeling and physiology studies, offering insights into the development and health of different cultivars under varying conditions.
#'
#' @format A \code{\link[data.frame]{data.frame}} with 77 rows and 22 columns:
#' \describe{
#'   \item{ID}{Trial ID - (\code{character}). A unique identifier for each trial. Example: \code{"LOC1T1PROJ1"}.}
#'   \item{LOC_ID}{Locality ID - (\code{character}). Indicates the location of the trial. Example: \code{"LOC1"}.}
#'   \item{PROJECT}{Project ID - (\code{character}). Associates the trial with a specific project. Example: \code{"PROJ1"}.}
#'   \item{CULTIVAR}{Cultivar name - (\code{character}). Specifies the plant cultivar used in the trial. Example: \code{"CULTIVAR1"}.}
#'   \item{SAMPLING_DATE}{Sampling date - (\code{date}). The date when the plant sampling was conducted, in MM/DD/YYYY format.}
#'   \item{LAI_OBS}{Leaf Area Index observed - (\code{numeric}). Measures the area of leaves per unit area of ground, in m² leaf/m² ground.}
#'   \item{LAI_SE}{Leaf Area Index standard deviation - (\code{numeric}). The standard deviation of the Leaf Area Index, in m² leaf/m² ground.}
#'   \item{WLVG_OBS}{Green leaf dry weight - (\code{numeric}). The dry weight of green leaves, in kg/ha.}
#'   \item{WLVG_SE}{Green leaf dry weight standard error - (\code{numeric}). The standard error of green leaf dry weight, in kg/ha.}
#'   \item{WLVD_OBS}{Dead leaf dry weight - (\code{numeric}). The dry weight of dead leaves, in kg/ha.}
#'   \item{WLVD_SE}{Dead leaf dry weight standard error - (\code{numeric}). The standard error of dead leaf dry weight, in kg/ha.}
#'   \item{WST_OBS}{Stem dry weight - (\code{numeric}). The dry weight of stems, in kg/ha.}
#'   \item{WST_SE}{Stem dry weight standard error - (\code{numeric}). The standard error of stem dry weight, in kg/ha.}
#'   \item{WSO_OBS}{Panicle dry weight - (\code{numeric}). The dry weight of panicles, in kg/ha.}
#'   \item{WSO_SE}{Panicle dry weight standard error - (\code{numeric}). The standard error of panicle dry weight, in kg/ha.}
#'   \item{WAGT_OBS}{Total dry weight - (\code{numeric}). The total plant dry weight, including all sampled organs, in kg/ha.}
#'   \item{WAGT_SE}{Total dry weight standard error - (\code{numeric}). The standard error of total dry weight, in kg/ha.}
#'   \item{NLV_OBS}{Number of green leaves - (\code{numeric}). The number of green leaves per square meter, in number/m².}
#'   \item{NLV_SE}{Number of green leaves standard error - (\code{numeric}). The standard error of the number of green leaves, in number/m².}
#'   \item{NST_OBS}{Number of stems - (\code{numeric}). The number of stems per square meter, in number/m².}
#'   \item{NST_SE}{Number of stems standard error - (\code{numeric}). The standard error of the number of stems, in number/m².}
#'   \item{NP_OBS}{Number of panicles - (\code{numeric}). The number of panicles per square meter, in number/m².}
#'   \item{NP_SE}{Number of panicles standard error - (\code{numeric}). The standard error of the number of panicles, in number/m².}
#' }
#' @source \code{\link[agroclimR]{Rodriguez-Espinoza J, (2024)}} and \code{CIAT-MADR-FEDEARROZ, (2016)}
#' @examples
#' # Assuming `plant` is your dataset name
#' summary(plant)
"plant"
