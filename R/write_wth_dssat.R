write_wth_dssat <-
function(path, id_name, wth_data, lat, lon, elev, ref_ht = 2){
  
    data <- tidy_wth_dssat(wth_data)
    data$datadata$wth_data
    
    # Function to write date into DSSAT format
    # 'var' must be object date class
    date_for_dssat <- function(var) {
      stopifnot(class(var)=="Date")
      stopifnot(require(lubridate))
      
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
     
    
    
    
    sink(paste0(path, id_name, '.WTH'), append = F)

    

    cat(paste("*WEATHER DATA :"), paste(id_name), "DSSAT Weather file - by https://github.com/jrodriguez88")
    cat("\n")
    cat("\n")
    cat(c("@ INSI      LAT     LONG  ELEV   TAV   AMP REFHT WNDHT"))
    cat("\n")
    cat(sprintf("%6s %8.3f %8.3f %5.0f %5.1f %5.1f %5.1f %5.1f", "CIAT", lat, lon, elev, data$tav, data$amp, ref_ht, ref_ht))
    cat("\n")
    cat(c('@DATE  SRAD  TMAX  TMIN  RAIN  DEWP  WIND   PAR  EVAP  RHUM'))
    cat("\n")
    cat(cbind(sprintf("%5s %5.1f %5.1f %5.1f %5.1f %5s %5s %5s %5s %5s",
                      date, srad, tmax, tmin, rain, " ", wspd, " ", " ", rhum)), sep = "\n")
    sink()
    
    
  }
