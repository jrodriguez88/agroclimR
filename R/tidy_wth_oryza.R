tidy_wth_oryza <-
function(wth_data, cal_VP = T){

    var_names <- colnames(wth_data)

    stopifnot(require(sirad))
    stopifnot(is.Date(wth_data$date) & all(c("tmax", "tmin", "rain", "srad") %in%  var_names))

    if("VP" %in% var_names){
        message("Early morning vapor pressure (VP; kPa) in data")
    } else if(isTRUE(cal_VP) & "rhum" %in% var_names)
    {
        wth_data <- mutate(wth_data,
                           es = sirad::es(tmax, tmin),         #Determination of mean saturation vapour pressure http://www.fao.org/3/x0490e/x0490e07.htm  - eq.12
                           VP = es*rhum/100) %>% select(-es)   #Determination of actual vapour pressure vpd http://www.fao.org/3/x0490e/x0490e07.htm  - eq.19
        message("Early morning vapor pressure (VP; kPa) derived from relative humidity data")

    } else {
        wth_data <- mutate(wth_data, VP = NA_real_)
        message("Vapor Pressure is not Available - VP Set as NA: -99")

    }


    if (!"wspd" %in% var_names) {
        wth_data <- mutate(wth_data, wspd = NA_real_)
        message("Wind Speed is not Available - Set as NA: -99")
    }

    return(wth_data)

}
