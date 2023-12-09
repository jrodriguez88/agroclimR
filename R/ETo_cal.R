#' Write AquaCrop Weather File
#'
#' Function compute weather information AquaCrop v6.0  weather file.
#'
#' @param wth_data A Data frame Weather data. minimum = date, tmax, tmin, rain
#' @param lon Numeric. Longitude (decimal degrees)
#' @param elev Numeric. Elevation (meters above sea level)
#' @param kRs Numeric. Hargreaves empirical coefficient, Coastal coefficient
#' @param ws_mean Numeric. Wind Speed Mean, default = 2 m/s
#' @import dplyr
#' @import purrr
#' @import lubridate
#' @import stringr
#' @importFrom sirad es
#' @importFrom sirad et0
#' @importFrom sirad extrat
#' @importFrom sirad radians
#' @export
#' @examples
#' # Write file
#' ETo_cal(wth_data = weather, lat = 3.8, elev = 650)
#'
## Update the details for the return value
#' @return This function returns a \code{Numeric} ET0 values (mm).
#'
ETo_cal <- function(wth_data, lat, elev, ref_ht = 2, kRs = 0.175, ws_mean = 2){

    stopifnot(require(sirad))

    varnames <- colnames(wth_data)

    if (all(c("rhum", "tmax", "tmin", "srad", "wspd") %in% varnames)) {
        ### Cal ETo

        message("Reference evapotranspiration (ETo) Method: FAO Penman-Monteith equation")

            ## Estimate clear sky transmissivity
            extraT <- extrat(lubridate::yday(wth_data$date), radians(lat))$ExtraTerrestrialSolarRadiationDaily

            ## cal trasmisivity 3% days
            #    tal <- cst(RefRad = wth_data$srad, days = wth_data$date, extraT = extraT, lat = radians(lat), perce = 5)

            ETo <- wth_data %>%
                mutate(
                    es = sirad::es(tmax, tmin),
                    ea = es*rhum/100,
                    #          ea = if_else(is.na(ea), 0.611*exp(17.27*tmin/(tmin+237.3)), ea),
                    #            vp = es-ea,
                    extraT = extraT,
                    tmean = (tmax+tmin)/2,
                    ETo = sirad::et0(tmax, tmin, ea, srad, 0.85, elev, wspd, ref_ht, extraT),
                    ETo = case_when(is.na(ETo) ~ 0.0023*(tmean + 17.8)*((tmax - tmin)^0.5)*extraT/3,
                                    TRUE ~ ETo)
                ) %>%
                pull(ETo)

        } else if (all(c("rhum", "tmax", "tmin", "srad") %in% varnames)) {
        ### Cal ETo

        if (!"wspd" %in% varnames) {
            wth_data <- mutate(wth_data, wspd = ws_mean)
            message(paste0("Wind Speed = ", ws_mean, "m/s was used"))
        }

        message(paste0("Reference evapotranspiration (ETo) Method: FAO Penman-Monteith equation  +  Assumption: Wind Speed mean = ", ws_mean, "m/s"))

            ## Estimate clear sky transmissivity
            extraT <- extrat(lubridate::yday(wth_data$date), radians(lat))$ExtraTerrestrialSolarRadiationDaily

            ## cal trasmisivity 3% days
            #    tal <- cst(RefRad = wth_data$srad, days = wth_data$date, extraT = extraT, lat = radians(lat), perce = 5)

            ETo <- wth_data %>%
                mutate(
                    es = sirad::es(tmax, tmin),
                    ea = es*rhum/100,
                    #          ea = if_else(is.na(ea), 0.611*exp(17.27*tmin/(tmin+237.3)), ea),
                    #            vp = es-ea,
                    extraT = extraT,
                    tmean = (tmax+tmin)/2,
                    ETo = sirad::et0(tmax, tmin, ea, srad, 0.85, elev, wspd, ref_ht, extraT),
                    ETo = case_when(is.na(ETo) ~ 0.0023*(tmean + 17.8)*((tmax - tmin)^0.5)*extraT/3,
                                    TRUE ~ ETo)
                ) %>%
                pull(ETo)

        } else if (all(c("tmax", "tmin") %in% varnames)) {

        message("Reference evapotranspiration (ETo) Method: Hargreaves equation")

            if (!"wspd" %in% varnames) {
                wth_data <- mutate(wth_data, wspd = ws_mean)
                #                message("Wind Speed = 2 m/s was used")
            }

            ## Estimate clear sky transmissivity
            extraT <- extrat(lubridate::yday(wth_data$date), radians(lat))$ExtraTerrestrialSolarRadiationDaily

            ETo <- wth_data %>%
                mutate(extraT = extraT,
                       ea = 0.611*exp(17.27*tmin/(tmin+237.3)),
                       tmean = (tmax+tmin)/2,
                       srad = kRs*sqrt(tmax - tmin)*extraT, # Coeficient, coastal
                       ETo = sirad::et0(tmax, tmin, ea, srad, 0.85, elev, wspd, ref_ht, extraT),
                       ETo = case_when(is.na(ETo) ~ 0.0023*(tmean + 17.8)*((tmax - tmin)^0.5)*extraT/2.5,
                                       TRUE ~ ETo)) %>%
                pull(ETo)

        } else {

        message("No data to calculate ETo!.")

    }


return(ETo)



}
