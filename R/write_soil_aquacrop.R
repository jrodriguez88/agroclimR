#' Write Aquacrop v6.1 Soil File
#'
#' Function compute Soil information to Aquacrop v6.1 soil file.
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
#' # Write Aquacrop v6 Soil file
#' soil_sample = group_by(soil, NL) %>% sample_n(1)
#' write_soil_aquacrop(path = ".", id_name = "test_soil", soil_data = soil_sample)
#'
## Update the details for the return value
#' @return This function returns a \code{logical} if files created in path folder.
#'
# @seealso \link[sirad]{se}


write_soil_aquacrop <- function(path = ".", id_name, soil_data, model_version = 6.1) {

    data <- as.data.frame(soil_data)

    #CN: Curve number (dimensionless)
    CN <- data[1,] %>%
      mutate(CN = case_when(Ksat <= 10 ~ 85,
                            Ksat > 10 & Ksat <=50 ~ 80,
                            Ksat > 50 & Ksat <=250 ~ 75,
                            Ksat > 250 ~ 65)) %>% pull(CN)


    # REW: Readily Evaporable Water (mm)
    REW <- data[1,] %>%
      mutate(REW_cal = (10*(FC - WP/2)*0.04),
             REW = case_when(REW_cal >=15 ~ 15,
                             REW_cal < 0 ~ 0,
                             TRUE ~ REW_cal)) %>% pull(REW) %>% sprintf("%1.f", .)

    sink(paste0(path, "/", id_name, ".SOL"), F)
    cat(paste0(id_name, " AquaCrop soil file - by https://github.com/jrodriguez88"))
    cat('\n')
    cat(paste0("        ", model_version,"                 : AquaCrop Version (May 2018)"), sep = "\n")
    cat(paste0("       ", CN, "                   : CN (Curve Number)") , sep = "\n")
    cat(paste0("       ", REW, "                   : Readily evaporable water from top layer (mm)"), sep = "\n")
    cat(paste0("        ", nrow(data), "                   : number of soil horizons") , sep = "\n")
    cat(paste0("       -9                   : variable no longer applicable"), sep = "\n")
    cat(paste0("  Thickness  Sat   FC    WP     Ksat   Penetrability  Gravel  CRa       CRb           description"), sep = "\n")
    cat(paste0("  ---(m)-   ----(vol %)-----  (mm/day)      (%)        (%)    -----------------------------------------"), sep = "\n")
    write.table(data.frame(Thickness    = sprintf("%8.2f", data[["Thickness"]]    ),
                           Sat          = sprintf("%7.1f", data[["Sat"]]          ),
                           FC           = sprintf("%5.1f", data[["FC"]]           ),
                           WP           = sprintf("%5.1f", data[["WP"]]           ),
                           Ksat         = sprintf("%7.1f", data[["Ksat"]]         ),
                           Penetrability= sprintf("%10.0f",data[["Penetrability"]]),
                           Gravel       = sprintf("%9.0f", data[["Gravel"]]       ),
                           CRa          = sprintf("%13.6f",data[["CRa"]]          ),
                           CRb          = sprintf("%9.6f", data[["CRb"]]          ),
                           description  = sprintf("%16s",  data[["description"]]  )),
                row.names = F, quote = F, col.names = F)
    sink()

}



# helpers -----------------------------------------------------------------
# Function to tidy soil data
tidy_soil_aquacrop <- function(soil_data, max_depth = 200){


  soil_data %>% ifelse()
    #  mutate(SOC = case_when(SOC > 20 ~ SOC/5,
    #                       TRUE ~ SOC)) %>%
    mutate(Penetrability = 100,
           TKL = c(DEPTH/100),
           bdod = SBDM, Gravel = 0,
           OM = (100/58)*SOC/10, # Organic matter (%) = Total organic carbon (%) x 1.72
           SSKS = pmap_dbl(.l = list(SAND, CLAY, OM, SBDM), SSKS_cal)*24,
           CRa = case_when(str_detect(STC, "Sa|LoSa|SaLo") ~ (-0.3112 - SSKS*10^(-5)),
                           str_detect(STC, "Lo|SiLo|Si") ~ (-0.4986 + SSKS*9*10^(-5)),
                           str_detect(STC, "SaCl|SaClLo|ClLo") ~ (-0.5677 - SSKS*4*10^(-5)),
                           str_detect(STC, "SiClLo|SiCl|Cl") ~ (-0.6366 + SSKS*8*10^(-4))),
           CRb = case_when(str_detect(STC, "Sa|LoSa|SaLo") ~ (-1.4936 + 0.2416*log(SSKS)),
                           str_detect(STC, "Lo|SiLo|Si") ~ (-2.1320 + 0.4778*log(SSKS)),
                           str_detect(STC, "SaCl|SaClLo|ClLo") ~ (-3.7189 + 0.5922*log(SSKS)),
                           str_detect(STC, "SiClLo|SiCl|Cl") ~ (-1.9165 + 0.7063*log(SSKS)))) %>%
    dplyr::select(id_name = site, TKL, WCST, WCFC, WCWP, SSKS, Penetrability, Gravel, CRa, CRb, STC) %>%
    setNames(c("id_name", "Thickness", "Sat", "FC", "WP", "Ksat", "Penetrability", "Gravel", "CRa", "CRb", "description"))





}



