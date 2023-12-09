#' Write AquaCrop Weather File
#'
#' Function compute weather information AquaCrop v6.0  weather file.
#'
#' @param path A string indicating path folder or working directory
#' @param id_name A String. 4 letters string of locality name. "AIHU" = Aipe, Huila
#' @param wth_data A Data frame Weather data. minimum = date, tmax, tmin, rain
#' @param lat Numeric. Latitude (decimal degrees)
#' @param lon Numeric. Longitude (decimal degrees)
#' @param elev Numeric. Elevation (meters above sea level)
#' @param co2_file A String. Default is "MaunaLoa.CO2".  *.CO2 files are available in Aquacrop default DB
#' @import dplyr
#' @import purrr
#' @import lubridate
#' @import stringr
#' @importFrom sirad es
#' @importFrom sirad etO
#' @importFrom sirad extraT
#' @importFrom sirad extrat
#' @export
#' @examples
#' # Write file
#' write_wth_aquacrop(
#'   path = ".", id_name = "TEST", wth_data = weather,
#'   lat = 3.8, lon = -76.5, elev = 650)
#'
## Update the details for the return value
#' @return This function returns a \code{logical} if files created in path folder.
#'
# @seealso \link[https://dssat.net/weather-module/]{se}
write_wth_aquacrop <- function(path, id_name, wth_data, lat, lon, elev, co2_file = "MaunaLoa.CO2", ...) {


    data <- tidy_wth_aquacrop(wth_data) %>% mutate(ETo = ETo_cal(., lat, elev, ...))

    ## Split data and write .ETo / .PLU / Tnx / .CLI files.

    # Climate file .CLI
    write_CLI <- function(id_name){
        sink(file = paste0(path, id_name, ".CLI"), append = F)
        cat(paste(id_name, "Station, lat:", lat, "long:", lon, "- by https://github.com/jrodriguez88"), sep = "\n")
        cat("6.0   : AquaCrop Version (March 2017)", sep = "\n")
        cat(paste0(id_name, ".Tnx"), sep = "\n")
        cat(paste0(id_name, ".ETo"), sep = "\n")
        cat(paste0(id_name, ".PLU"), sep = "\n")
        cat(paste(co2_file), sep = "\n")

        sink()

    }
    write_CLI(id_name)

    # Temperature file .Tnx
    write_Tnx <- function(id_name){
        sink(file = paste0(path , id_name, ".Tnx"), append = F)
        cat(paste0(id_name, " : daily temperature data (", format(min(data$date), "%d %B %Y"), " - ", format(max(data$date), "%d %B %Y"), ")"))
        cat("\n")
        cat(paste0("     1  : Daily records (1=daily, 2=10-daily and 3=monthly data)"), sep = "\n")
        cat(paste0("     ", day(min(data$date)), "  : First day of record (1, 11 or 21 for 10-day or 1 for months)") , sep = "\n")
        cat(paste0("     ", month(min(data$date)), "  : First month of record"), sep = "\n")
        cat(paste0("  ", year(min(data$date)), "  : First year of record (1901 if not linked to a specific year)") , sep = "\n")
        cat("\n")
        cat(paste0("  Tmin (C)   TMax (C)", sep = "\n"))
        cat("=======================", sep = "\n")
        write.table(data.frame(tmin = sprintf("%10.1f", data$tmin),
                               tmax = sprintf("%10.1f", data$tmax)),
                    row.names = F, quote = F, col.names = F)

        sink()

    }
    write_Tnx(id_name)

    write_PLU <- function(id_name){
        sink(file = paste0(path, id_name, ".PLU"), append = F)
        cat(paste0(id_name, " : daily rainfall data (", format(min(data$date), "%d %B %Y"), " - ", format(max(data$date), "%d %B %Y"), ")"))
        cat("\n")
        cat(paste0("     1  : Daily records (1=daily, 2=10-daily and 3=monthly data)"), sep = "\n")
        cat(paste0("     ", day(min(data$date)), "  : First day of record (1, 11 or 21 for 10-day or 1 for months)") , sep = "\n")
        cat(paste0("     ", month(min(data$date)), "  : First month of record"), sep = "\n")
        cat(paste0("  ", year(min(data$date)), "  : First year of record (1901 if not linked to a specific year)") , sep = "\n")
        cat("\n")
        cat(paste0("  Total Rain (mm)", sep = "\n"))
        cat("=======================", sep = "\n")
        writeLines(sprintf("%10.1f", data$rain))
        sink()

    }
    write_PLU(id_name)

    write_ETo <- function(id_name){
        sink(file = paste0(path, id_name, ".ETo"), append = F)
        cat(paste0(id_name, " : daily ETo data (", format(min(data$date), "%d %B %Y"), " - ", format(max(data$date), "%d %B %Y"), ")"))
        cat("\n")
        cat(paste0("     1  : Daily records (1=daily, 2=10-daily and 3=monthly data)"), sep = "\n")
        cat(paste0("     ", day(min(data$date)), "  : First day of record (1, 11 or 21 for 10-day or 1 for months)") , sep = "\n")
        cat(paste0("     ", month(min(data$date)), "  : First month of record"), sep = "\n")
        cat(paste0("  ", year(min(data$date)), "  : First year of record (1901 if not linked to a specific year)") , sep = "\n")
        cat("\n")
        cat(paste0("  Average ETo (mm/day)", sep = "\n"))
        cat("=======================", sep = "\n")
        writeLines(sprintf("%10.1f", data$ETo))
        sink()

    }
    write_ETo(id_name)

    return(any(str_detect(list.files(path, id_name), id_name) == T))

}


# helpers -----------------------------------------------------------------

tidy_wth_aquacrop <- function(wth_data, cal_ETo = T){

  var_names <- colnames(wth_data)
  wth_data <- setNames(wth_data, var_names)

  stopifnot(class(wth_data$date)=="Date" & all(c("tmax", "tmin", "rain") %in%  var_names))

  if("eto" %in% var_names){
    message("Reference evapotranspiration (ETo, mm) in data")
  } else if(isTRUE(cal_ETo))
  {
    wth_data <- wth_data %>% mutate(ETo = ETo_cal(., lat, elev, ...)) #   message("Early morning vapor pressure (VP; kPa) derived from relative humidity data")

  } else {
    wth_data <- mutate(wth_data, VP = NA_real_)
    message("ETo is not Available - ETo Set as NA: -99")

  }

  return(wth_data)

}



