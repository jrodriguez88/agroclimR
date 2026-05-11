#' Write a DSSAT v4.8 weather file
#'
#' Formats daily weather data as a DSSAT weather (`.WTH`) file. The input data
#' are standardized internally and optional wind speed and relative humidity
#' columns are written when available.
#'
#' @param path Character. Directory where the `.WTH` file will be written.
#' @param id_name Character. Station or site identifier used as the output file
#'   name, without extension.
#' @param wth_data Data frame with daily weather data. Required columns are
#'   `date` (`Date`), `tmax`, `tmin`, `rain`, and `srad`. Optional columns are
#'   `wspd` (m s-1) and `rhum` (%).
#' @param lat,lon Numeric. Latitude and longitude in decimal degrees.
#' @param elev Numeric. Elevation in meters above sea level.
#' @param ref_ht Numeric. Measurement height, in meters, used for DSSAT `REFHT`
#'   and `WNDHT`.
#' @import dplyr
#' @import purrr
#' @import lubridate
#' @import stringr
#' @export
#' @examples
#' # Write DSSAT weather file
#' wth_files_created <- write_wth_dssat(
#'   path = tempdir(), id_name = "TEST", wth_data = weather,
#'   lat = 3.91, lon = -75.0, elev = 450)
#'
#' readLines(wth_files_created[1], n = 15) |> writeLines()
#' file.remove(wth_files_created)
#'
#' @returns Character vector with the path of the DSSAT weather file created.
#'
#' @references
#' - DSSAT Weather module: <https://dssat.net/weather-module/>
#'
#' - Brakensiek et al. (1984) for SSKS_Brakensiek
# @seealso \link[https://dssat.net/weather-module/]{DSSAT Weather module}
write_wth_dssat <- function(path = ".", id_name, wth_data, lat, lon, elev, ref_ht = 2){

    data <- tidy_wth_dssat(wth_data)
    var_names <- colnames(wth_data)
#    data$datadata$wth_data

    # Function to write date into DSSAT format
    # 'var' must be object date class
    date_for_dssat <- function(var) {
      stopifnot(class(var)=="Date")
      #stopifnot(require(lubridate))

      yr <- str_sub(year(var), -2)
      doy <- yday(var)

      paste0(yr, sprintf("%.3d", doy))

    }

    date <- date_for_dssat(data$wth_data$date)
    srad <- data$wth_data$srad
    tmax <- data$wth_data$tmax
    tmin <- data$wth_data$tmin
    rain <- data$wth_data$rain
    wspd <- if(is.numeric(data$wth_data$wspd)) as.character(sprintf("%3.1f", data$wth_data$wspd)) else data$wth_data$wspd
    rhum <- if(is.numeric(data$wth_data$rhum)) as.character(sprintf("%2.1f", data$wth_data$rhum)) else data$wth_data$rhum


    #File name
    file_name <- file.path(path, paste0(id_name, ".WTH"))

    sink(file_name, append = F)



    cat(paste("*WEATHER DATA :"), paste(id_name), "DSSAT Weather file - by agroclimR")
    cat("\n")
    cat("\n")
    cat(c("@ INSI      LAT     LONG  ELEV   TAV   AMP REFHT WNDHT"))
    cat("\n")
    cat(sprintf("%6s %8.3f %8.3f %5.0f %5.1f %5.1f %5.1f %5.1f", "ACRP", lat, lon, elev, data$tav, data$amp, ref_ht, ref_ht))
    cat("\n")
    cat(c('@DATE  SRAD  TMAX  TMIN  RAIN  DEWP  WIND   PAR  EVAP  RHUM'))
    cat("\n")
    cat(cbind(sprintf("%5s %5.1f %5.1f %5.1f %5.1f %5s %5s %5s %5s %5s",
                      date, srad, tmax, tmin, rain, " ", wspd, " ", " ", rhum)), sep = "\n")
    sink()

    message(paste("DSSAT Weather Files created in ", path, " : \n",
                  paste(file_name, collapse = " ,")))
    file_name



}


# helpers -----------------------------------------------------------------

tidy_wth_dssat <- function(wth_data){

  #var_names <- colnames(wth_data)
  var_names <- tolower(colnames(wth_data))
  wth_data <- setNames(wth_data, var_names)

 # stopifnot(require(sirad))
  stopifnot(is.Date(wth_data$date) & all(c("tmax", "tmin", "rain", "srad") %in%  var_names))

  if (!"rhum" %in% var_names) {
    wth_data <- mutate(wth_data, rhum = " ")
    #    message("Wind Speed is not Available - Set as NA: -99")
  }

  if (!"wspd" %in% var_names) {
    wth_data <- mutate(wth_data, wspd = " ")
    #    message("Wind Speed is not Available - Set as NA: -99")
  } else {wth_data <- mutate(wth_data, wspd = wspd*3.6*24)}  #from wind speed mean (m/s) to (km/day)

  tav <- wth_data %>%
    mutate(tmean = (tmax + tmin)/2 ) %>%
    summarise(tav = mean(tmean)) %>% pull(tav)


  amp <- wth_data %>%
    group_by(month(date)) %>%
    summarise(tmax = mean(tmax), tmin = mean(tmin)) %>%
    mutate(amp = tmax - tmin) %>% pull(amp) %>% mean

  return(list(wth_data = wth_data, tav = tav, amp = amp))

}
