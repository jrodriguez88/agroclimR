#' Write an AquaCrop soil file
#'
#' Formats a soil profile as an AquaCrop `.SOL` file. The writer derives Curve
#' Number and readily evaporable water from the supplied hydraulic properties and
#' writes one soil profile per file.
#'
#' @param path Character. Directory where the `.SOL` file will be written.
#' @param id_name Character. Soil profile identifier used as the output file
#'   name, without extension.
#' @param soil_data Data frame with one row per soil layer. Expected columns
#'   include `LOC_ID`, `DEPTH`, `SBDM`, `SOC`, `SSKS`, `WCST`, `WCFC`, `WCWP`,
#'   and `STC`; see the example data set [soil].
#' @param model_version Numeric or character. AquaCrop version label written in
#'   the file header.
#' @import dplyr
#' @import stringr
#' @export
#' @examples
#' # Write Aquacrop v6 Soil file
#' soil_sample = dplyr::group_by(soil, NL) |> dplyr::sample_n(1)
#' soil_files_created <- write_soil_aquacrop(
#' path = tempdir(),
#' id_name = "soil_aquacrop",
#' soil_data = soil_sample)
#'
#' readLines(soil_files_created[1], n = 15) |> writeLines()
#' file.remove(soil_files_created)
#'
#' @returns Character vector with the path of the AquaCrop soil file created.
#'
# @seealso \link[sirad]{se}


write_soil_aquacrop <- function(path = ".", id_name, soil_data, model_version = 6.1) {

    data <- as.data.frame(soil_data) %>%
      tidy_soil_aquacrop()

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

    file_name <- paste0(path, "/", id_name, ".SOL")

    sink(file_name, F)
    cat(paste0(id_name, " AquaCrop soil file - by agroclimR"))
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

    message(paste("AquaCrop soil Files created in ", path, " : \n",
                  paste(file_name, collapse = " ,")))
    file_name

}



# helpers -----------------------------------------------------------------
# Function to tidy soil data
tidy_soil_aquacrop <- function(soil_data, max_depth = 200){


  soil_data %>%

    #  mutate(SOC = case_when(SOC > 20 ~ SOC/5,
    #                       TRUE ~ SOC)) %>%
    mutate(Penetrability = 100,
           TKL = c(DEPTH/100),
           bdod = SBDM, Gravel = 0,
           OM = (100/58)*SOC/10, # Organic matter (%) = Total organic carbon (%) x 1.72
           SSKS = SSKS*24,
           CRa = case_when(str_detect(STC, "Sa|LoSa|SaLo") ~ (-0.3112 - SSKS*10^(-5)),
                           str_detect(STC, "Lo|SiLo|Si") ~ (-0.4986 + SSKS*9*10^(-5)),
                           str_detect(STC, "SaCl|SaClLo|ClLo") ~ (-0.5677 - SSKS*4*10^(-5)),
                           str_detect(STC, "SiClLo|SiCl|Cl") ~ (-0.6366 + SSKS*8*10^(-4))),
           CRb = case_when(str_detect(STC, "Sa|LoSa|SaLo") ~ (-1.4936 + 0.2416*log(SSKS)),
                           str_detect(STC, "Lo|SiLo|Si") ~ (-2.1320 + 0.4778*log(SSKS)),
                           str_detect(STC, "SaCl|SaClLo|ClLo") ~ (-3.7189 + 0.5922*log(SSKS)),
                           str_detect(STC, "SiClLo|SiCl|Cl") ~ (-1.9165 + 0.7063*log(SSKS)))) %>%
    dplyr::select(id_name = LOC_ID, TKL, WCST, WCFC, WCWP, SSKS, Penetrability, Gravel, CRa, CRb, STC) %>%
    setNames(c("id_name", "Thickness", "Sat", "FC", "WP", "Ksat", "Penetrability", "Gravel", "CRa", "CRb", "description"))





}




