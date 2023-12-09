write_wth_aquacrop <-
function(path, id_name, wth_data, lat, lon, elev, co2_file = "MaunaLoa.CO2") {
    

    data <- tidy_wth_aquacrop(wth_data) %>% mutate(ETo = ETo_cal(., lat, elev))
    
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
        cat(paste0(id_name, " : daily temperature data (", format(min(wth_data$date), "%d %B %Y"), " - ", format(max(wth_data$date), "%d %B %Y"), ")"))
        cat("\n")
        cat(paste0("     1  : Daily records (1=daily, 2=10-daily and 3=monthly data)"), sep = "\n")
        cat(paste0("     ", day(min(wth_data$date)), "  : First day of record (1, 11 or 21 for 10-day or 1 for months)") , sep = "\n")
        cat(paste0("     ", month(min(wth_data$date)), "  : First month of record"), sep = "\n")
        cat(paste0("  ", year(min(wth_data$date)), "  : First year of record (1901 if not linked to a specific year)") , sep = "\n")
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
        cat(paste0(id_name, " : daily rainfall data (", format(min(wth_data$date), "%d %B %Y"), " - ", format(max(wth_data$date), "%d %B %Y"), ")"))
        cat("\n")
        cat(paste0("     1  : Daily records (1=daily, 2=10-daily and 3=monthly data)"), sep = "\n")
        cat(paste0("     ", day(min(wth_data$date)), "  : First day of record (1, 11 or 21 for 10-day or 1 for months)") , sep = "\n")
        cat(paste0("     ", month(min(wth_data$date)), "  : First month of record"), sep = "\n")
        cat(paste0("  ", year(min(wth_data$date)), "  : First year of record (1901 if not linked to a specific year)") , sep = "\n")
        cat("\n")
        cat(paste0("  Total Rain (mm)", sep = "\n"))
        cat("=======================", sep = "\n")
        writeLines(sprintf("%10.1f", data$rain))
        sink()
        
    }
    write_PLU(id_name)
    
    write_ETo <- function(id_name){
        sink(file = paste0(path, id_name, ".ETo"), append = F)
        cat(paste0(id_name, " : daily ETo data (", format(min(wth_data$date), "%d %B %Y"), " - ", format(max(wth_data$date), "%d %B %Y"), ")"))
        cat("\n")
        cat(paste0("     1  : Daily records (1=daily, 2=10-daily and 3=monthly data)"), sep = "\n")
        cat(paste0("     ", day(min(wth_data$date)), "  : First day of record (1, 11 or 21 for 10-day or 1 for months)") , sep = "\n")
        cat(paste0("     ", month(min(wth_data$date)), "  : First month of record"), sep = "\n")
        cat(paste0("  ", year(min(wth_data$date)), "  : First year of record (1901 if not linked to a specific year)") , sep = "\n")
        cat("\n")
        cat(paste0("  Average ETo (mm/day)", sep = "\n"))
        cat("=======================", sep = "\n")
        writeLines(sprintf("%10.1f", data$ETo))
        sink()
        
    }
    write_ETo(id_name)
    
    
}
