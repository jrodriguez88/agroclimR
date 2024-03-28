#' Write ORYZA v3 Weather File (.WTH - .CLI)
#'
#' Function compute weather information to ORYZA weather file.
#'
#' @param path A string indicating path folder or working directory
#' @param id_name A String 4 letters string of locality name. "AIHU" = Aipe, Huila
#' @param wth_data A Data frame Weather data. minimum = date, tmax, tmin, rain
#' @param lat Numeric. Latitude (decimal degrees)
#' @param lon Numeric. Longitude (decimal degrees)
#' @param elev Numeric. Elevation (meters above sea level)
#' @param stn Integer. Station number
#' @param multiyear A Logical. TRUE = ".cli" multiyear format or FALSE = yearly format (ie. 1998 = *.998)
#' @param tag A Logical. TRUE = write information for each file
#' @import dplyr
#' @import purrr
#' @import stringr
#' @import lubridate
#' @importFrom sirad es
#' @importFrom utils write.table
#' @export
#' @examples
#' # Write wth file
#' wth_files_created <- write_wth_oryza(
#'   path = ".", id_name = "TEST", wth_data = weather,
#'   lat = 3.8, lon = -76.5, elev = 650)
#'
#' readLines(wth_files_created[1], n = 15) |> writeLines()
#' file.remove(wth_files_created)
#'
#' wth_files_created2 <- write_wth_oryza(
#'   path = ".", id_name = "TEST2", wth_data = weather,
#'   lat = 3.8, lon = -76.5, elev = 650, multiyear = TRUE, tag = TRUE)
#'
#' readLines(wth_files_created2[1], n = 25) |> writeLines()
#' file.remove(wth_files_created2)
#'
## Update the details for the return value
#' @returns This function returns a vector of model files created in path folder.
#'
# @seealso \link[sirad]{se}
write_wth_oryza <- function(path = ".", id_name, wth_data, lat, lon, elev, stn=1, multiyear = F, tag = F) {

  # Tidy weather data

  wth_data <- tidy_wth_oryza(wth_data)
  var_names <- colnames(wth_data)

print_tag <- function(){

cat("*-----------------------------------------------------------", sep = '\n')
cat(paste0("*  Station Name: ", id_name), sep = '\n')
cat(paste0("*  ORYZA Weather file - by agroclimR"), sep = '\n')
cat(paste0("*  Longitude: ", lon, " -- Latitude: ", lat, " -- Elevation: ", elev, "m"), sep = '\n')
cat("*-----------------------------------------------------------", sep = '\n')
cat(paste0("*  Date: ", min(wth_data$date), " : ", max(wth_data$date)), sep = '\n')
cat("*", sep = '\n')
cat("*  Column    Daily Value
*     1      Station number
*     2      Year
*     3      Day
*     4      irradiance         KJ m-2 d-1
*     5      min temperature            oC
*     6      max temperature            oC
*     7      vapor pressure            kPa
*     8      mean wind speed         m s-1
*     9      precipitation          mm d-1
*-----------------------------------------------------------", sep = '\n')

}


# Data base
    data_to <- wth_data %>%
            mutate(stn = stn,
                   year = year(date),
                   day = yday(date),
                   srad = if_else(is.na(srad), median(.$srad, na.rm = T), round(srad*1000, 2)),
                   tmax = if_else(is.na(tmax), -99, round(tmax, 2)),
                   tmin = if_else(is.na(tmin), -99, round(tmin, 2)),
                   rain = if_else(is.na(rain), -99, round(rain, 2)),
                   vp  = if_else(is.na(vp), -99, round(vp, 2)),
                   wspd = if_else(is.na(wspd), -99, round(wspd, 2))) %>%
            dplyr::select(stn, year, day, srad, tmin, tmax, vp, wspd, rain)


    #    dir.create(paste0(path,"/WTH"), showWarnings = FALSE)
    set_head <- paste(lon, lat, elev, 0, 0, sep = ",")

    if(isTRUE(multiyear)){
    #DATA=read.table(file, head=T)
        file_name <- paste0(path, "/" , id_name, stn, ".cli")
        sink(file = file_name, append = F)
        if(isTRUE(tag)) print_tag()
        cat(set_head)
        cat("\n")
        write.table(data_to , sep= ",", row.names = F, col.names = F)
        sink()
    } else {
        data_list <- split(data_to, data_to$year)
        file_name <- paste(path,"/", id_name, stn,".", str_sub(names(data_list), 2), sep = "")
        walk2(data_list, file_name, function(x,y) {

            sink(file=y)
            if(isTRUE(tag)) print_tag()
            cat(set_head)
            cat("\n")
            write.table(x ,sep=",",row.names=F,col.names=F)
            sink()})


    }

    message(paste("Oryza Weather Files created in ", path, " : \n",
                  paste(file_name, collapse = " ,")))
    file_name



}

# helpers -----------------------------------------------------------------

tidy_wth_oryza <- function(wth_data, cal_VP = TRUE){

  #stopifnot(require(sirad))
  # Tidy weather data

  var_names <- tolower(colnames(wth_data))
  wth_data <- setNames(wth_data, var_names)

  stopifnot(class(wth_data$date)=="Date" & all(c("tmax", "tmin", "rain", "srad") %in%  var_names))

  if("vp" %in% var_names){
    message("Early morning vapor pressure (VP; kPa) in data")

  } else if(isTRUE(cal_VP) & "rhum" %in% var_names)
  {
    wth_data <- mutate(wth_data,
                       es = sirad::es(tmax, tmin),         #Determination of mean saturation vapour pressure http://www.fao.org/3/x0490e/x0490e07.htm  - eq.12
                       vp = es*rhum/100) %>% select(-es)   #Determination of actual vapour pressure vpd http://www.fao.org/3/x0490e/x0490e07.htm  - eq.19
    message("Early morning vapor pressure (VP; kPa) derived from relative humidity data")

  } else {
    wth_data <- mutate(wth_data, vp = NA_real_)
    message("Vapor Pressure is not Available - VP Set as NA: -99")

  }


  if (!"wspd" %in% var_names) {
    wth_data <- mutate(wth_data, wspd = NA_real_)
    message("Wind Speed is not Available - Set as NA: -99")
  }

  return(wth_data)

}
