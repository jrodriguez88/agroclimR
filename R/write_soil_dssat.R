#' Write ORYZA v3 Soil File
#'
#' Function compute Soil information to ORYZA soil file.
#'
#' @param path A string indicating path folder or working directory
#' @param id_name A String. 4 letters string of locality name. (ex. "JR")
#' @param soil_data A Data frame. Soil data. see `soil`
#' @param salb Numeric. Albedo, fraction
#' @param evapL Numeric. Evaporation limit, (mm)
#' @param slnf Numeric. Mineralization factor, 0 to 1 scale.
#' @param slpf Numeric. Photosynthesis factor, 0 to 1 scale
#' @param multi Logical. Soil annual average temperature of the first layers
#' @param max_depth description
#' @import dplyr
#' @import stringr
#' @export
#' @examples
#' # Write ORYZA Soil file
#' soil = group_by(soil, NL) %>% sample_n(1)
#' write_soil_dssat(id_name = "test_soil", soil_data = soil)
#'
## Update the details for the return value
#' @return This function returns a \code{logical} if files created in path folder.
#'
# @seealso \link[sirad]{se}
write_soil_dssat <- function(path, id_name, soil_data, salb = 0.13, evapL=6, sldr = 0.6, slnf = 1, slpf = 1, multi = F) {

stc <- soil_data$STC
data <- tidy_soil_dssat(soil_data)


SCOM <- "-99"    # SCOM     Color, moist, Munsell hue
SALB <- salb     # SALB     Albedo, fraction
SLU1 <- evapL       # SLU1     Evaporation limit, mm
SLDR <- sldr      # SLDR     Drainage rate, fraction day-1
SLRO <- data[[2]]       # SLRO     Runoff curve no. (Soil Conservation Service)
SLNF <- slnf        # SLNF     Mineralization factor, 0 to 1 scale
SLPF <- slpf     # SLPF     Photosynthesis factor, 0 to 1 scale
SMHB <- "IB001"    # SMHB     pH in buffer determination method, code
SMPX <- "IB001"    # SMPX     Phosphorus determination code
SMKE <- "IB001"    # SMKE     Potassium determination method, code

format_var <- function(soil_data, par, pat = "%3.1f"){

  par <- soil_data[[par]]

  if(is.numeric(par)){
    as.character(sprintf(pat, par))
  } else if (is.null(par)){
    "-99"
  } else {
    par
  }

}

#SLB  <- 5          #   Depth, base of layer, cm
#SBDM <- 1.37       #   Bulk density, moist, g cm-3
#SCEC <- 15.4       #   Cation exchange capacity, cmol kg-1
#SDUL <- 0.26       #   Upper limit, drained, cm3 cm-3
#SLBS <- 0.1        #   Base saturation, cmol kg-1
#SLCF <- 2.2        #   Coarse fraction (>2 mm), %
#SLCL <- 26.0       #   Clay (<0.002 mm), %
#SLHB <- 5.3        #   pH in buffer
#SLHW <- 6.5        #   pH in water
#SLLL <- 0.125       #   Lower limit, cm3 cm-3
#SLMH <- "A1"       #   Master horizon
#SLNI <- 4.444       #   Total nitrogen, %
#SLOC <- 2.83       #   Organic carbon, %
#SLSI <- 26.0       #   Silt (0.05 to 0.002 mm), %
#SRGF <- 0.988       #   Root growth factor, soil only, 0.0 to 1.0
#SSAT <- 0.412       #   Upper limit, saturated, cm3 cm-3
#SSKS <- 7.40       #   Sat. hydraulic conductivity, macropore, cm h-1


soil_data_col <- c('SLB', 'SLMH', 'SLLL', 'SDUL', 'SSAT', 'SRGF', 'SSKS', 'SBDM', 'SLOC', 'SLCL', 'SLSI', 'SLCF', 'SLNI', 'SLHW', 'SLHB', 'SCEC', 'SADC')
format_data_col <- c("%6.0f", "%5s", "%5.3f", "%5.3f", "%5.3f", "%5.3f", "%5.2f", "%5.2f", "%5.2f", "%5.1f", "%5.1f", "%5.1f", "%5.1f", "%5.1f", "%5.1f", "%5.1f", "%5.1f")


soil_tb <- map2(soil_data_col, format_data_col,
                  ~format_var(soil_data = data[[1]], par = .x, pat = .y)) %>%
  set_names(soil_data_col) %>% bind_cols()

#create id for multisoil profile
if (isFALSE(multi)){
  idsoilAR <- 1
  idsoilAR <<- idsoilAR
  suppressWarnings(file.remove(paste0(path, id_name, '.SOL')))
} else if (all(!exists("idsoilAR"), isTRUE(multi))){
  file.remove(paste0(path, id_name, '.SOL'))
  idsoilAR <- 1
  idsoilAR <<- idsoilAR
  } else if (all(exists("idsoilAR"), isTRUE(multi), idsoilAR==1)){
#    file.remove(paste0(path, id_name, '.SOL'))
#    idsoilAR <- idsoilAR + 1
    idsoilAR <<- idsoilAR + 1
  } else if (all(exists("idsoilAR"), isTRUE(multi), idsoilAR>1)){
    idsoilAR <- idsoilAR + 1
    idsoilAR <<- idsoilAR
    }

id_acjr <- paste0("*", id_name, str_pad(idsoilAR, width = 6, "left", "0"))

max_depth <- soil_tb$SLB[[nrow(soil_tb)]]
texture <- toupper(str_sub(stc[[1]],1,2))


sink(paste0(path, id_name, '.SOL'), append = multi)
if (isFALSE(multi)){
  cat("*SOILS: AgroclimR DSSAT Soil Input File - by https://github.com/jrodriguez88/agroclimR", sep = "\n")
  cat("\n")
}
else if(all(exists("idsoilAR"), idsoilAR==1)){
  cat("*SOILS: AgroclimR DSSAT Soil Input File - by https://github.com/jrodriguez88/agroclimR", sep = "\n")
  cat("\n")
} else if(all(exists("idsoilAR"), isTRUE(multi), idsoilAR>1)){
  cat("\n")
}

#cat(paste0("!AgroclimR DSSAT Soil: ",  id_name, " - by https://github.com/jrodriguez88/agroclimR"), sep = "\n")
#cat("\n")
cat(sprintf("%11s %10s %4s %7s %1s",id_acjr, " AgroclimRV1", texture , max_depth,  paste0(" AgroClimR ", id_name)))
cat("\n")
cat(c("@SITE        COUNTRY          LAT     LONG SCS FAMILY"), sep = "\n")
#cat(sprintf(" %-12s%-15s %-6.2f %-6.2f %-16s", id_name, "AgroclimR", lat, lon , paste0("USDA Texture: ", stc[[1]])))
cat(sprintf(" %-12s%-15s   %-6d %-4d%-16s", id_name, "AgroclimR", as.integer(-99), as.integer(-99) , paste0("USDA Texture: ", stc[[1]])))
cat("\n")
cat(c('@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE'))
cat("\n")
cat(sprintf("%6s %5.2f %5.0f %5.1f %5.0f %5.2f %5.2f %5s %5s %5s",
            SCOM, SALB, SLU1, SLDR, SLRO, SLNF, SLPF, SMHB, SMPX, SMKE))
cat("\n")
cat(c('@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC'))
cat("\n")
cat(cbind(sprintf("%6s %5s %5s %5s %5s %5s %5s %5s %5s %5s %5s %5s %5s %5s %5s %5s %5s",
                  soil_tb$SLB, soil_tb$SLMH, soil_tb$SLLL, soil_tb$SDUL, soil_tb$SSAT, soil_tb$SRGF, soil_tb$SSKS,
                  soil_tb$SBDM, soil_tb$SLOC, soil_tb$SLCL, soil_tb$SLSI, soil_tb$SLCF, soil_tb$SLNI, soil_tb$SLHW,
          soil_tb$SLHB, soil_tb$SCEC, soil_tb$SADC)), sep = "\n")
#if(isTRUE(multi)){cat("\n")}
sink()

}


# helpers -----------------------------------------------------------------

tidy_soil_dssat <- function(soil_data, max_depth = 200){

  var_names <- colnames(soil_data)
  if(all(any(c("depth", "DEPTH", "SLB") %in% var_names) &
         any(c("clay", "CLAY", "C", "SLCL") %in%  var_names) &
         any(c("sand", "SAND", "S", "silt", "SILT", "SLSI", "Si") %in%  var_names) &
         any(c("sbdm", "SBDM", "BD") %in% var_names) &
         any(c("soc", "SOC", "OM", "SLOC", "SC") %in% var_names))){

    message("Minimun data are available")

  } else {stop(message("NO data")) }

  # require SRGF_cal function --> utils_crop_model
  SRGF <- soil_data[c("depth", "DEPTH", "SLB")[which(c("depth", "DEPTH", "SLB") %in% var_names)]] %>%
    mutate(SRGF = SRGF_cal(pull(.), max_depth, 2)) %>% pull(SRGF)

  soil_data <- soil_data %>% mutate(SRGF = SRGF)


  #CN: Curve number (dimensionless)
  CN <- soil_data[1,] %>%
    mutate(SSKS = SSKS*10,
           CN = case_when(SSKS <= 10 ~ 85,
                          SSKS > 10 & SSKS <=50 ~ 80,
                          SSKS > 50 & SSKS <=250 ~ 75,
                          SSKS > 250 ~ 65)) %>% pull(CN)


  list(soil_data, CN, lat = -99, lon = -99)


}



SRGF_cal <- function(z, zmax, wcg){
  #Root growth factor, soil only, 0.0 to 1.0  -- Function to calculate
  #https://www.researchgate.net/publication/43282937_Effects_of_Estimating_Soil_Hydraulic_Properties_and_Root_Growth_Factor_on_Soil_Water_Balance_and_Crop_Production
  #Higher WCG values produced less root distribution deeper in the soil profile


  # Eq 1. SRGF or values calculated from Jones et al. (1991)

  # where SRGF(z) is the soil root growth factor at soil depth z (dimensionless)

  # z <- 10       # Depth, base of layer, cm
  # zmax <- 45    # The maximum rooting depth (cm)
  # wcg <- 3      # Exponential geotropism constant.


  SRGF_ <- ifelse(z <=15 , 1, (1 - (z/zmax))^wcg)

  return(ifelse(SRGF_ < 0, 0, SRGF_))


}
#SRGF_cal(100, 50, 3)
