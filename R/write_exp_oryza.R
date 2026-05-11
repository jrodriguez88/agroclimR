#' Write an ORYZA v3 experimental file
#'
#' Creates an ORYZA v3 experimental (`.exp`) file from prepared agronomic,
#' fertilization, phenology and plant observation inputs. This is the low-level
#' writer used after experiment tables have been tidied into one row per
#' experiment.
#'
#' @param exp_file Character. Full path to the ORYZA `.exp` file to create.
#' @param LOC_ID Character. Location or experiment identifier.
#' @param CULTIVAR Character. ORYZA cultivar name or code.
#' @param PDAT Date. Planting date.
#' @param ESTAB Character or numeric. Crop establishment method code used by
#'   ORYZA.
#' @param SBDUR Numeric. Seedbed duration.
#' @param NPLDS Numeric. Number of plants or seedlings, as expected by the ORYZA
#'   template.
#' @param CROP_SYS Character. Crop system descriptor written in the experimental
#'   file.
#' @param TRDAT Date or `NA`. Transplanting date when applicable.
#' @param FERT_obs Data frame with fertilization observations for the experiment,
#'   typically including day/date and nitrogen amount columns.
#' @param PHEN_obs Data frame with phenology observations for the experiment.
#' @param PLANT_obs Data frame with plant growth observations for the experiment.
#' @param ET_mod Character. Evapotranspiration method. Supported template labels
#'   include `"PENMAN"`, `"PRIESTLY TAYLOR"`, and `"MAKKINK"`.
#' @details Use `tidy_exp_oryza()` to join raw agronomic, phenology, plant,
#'   fertilization and yield tables into the row-wise inputs consumed by this
#'   writer.
#' @import dplyr
#' @import stringr
#' @import purrr
#' @import tibble
#' @import tidyr
#' @export
#' @examples
#' \dontrun{
#' # Experimental tidy data
#' tidy_exp_data <- tidy_exp_oryza(
#'   agro, phenol, plant, fertil, yield, path = tempdir())
#'
#' # Write ORYZA experimental files
#' exp_files_created <- tidy_exp_data %>%
#'   mutate(file = pmap(., write_exp_oryza)) %>%
#'   pull(exp_file)
#'
#' exp_files_created
#' file.remove(exp_files_created)
#' }
#'
#' @returns Character vector with the path of the ORYZA experimental file created.

write_exp_oryza <- function(exp_file, LOC_ID, CULTIVAR, PDAT, ESTAB, SBDUR, NPLDS, CROP_SYS, TRDAT, FERT_obs, PHEN_obs, PLANT_obs, ET_mod = "PRIESTLY TAYLOR") {

    # #tabla de experimentos crea nombre de archivos experimentales == ID
    # exp_data <- agro %>%
    #     mutate_at(.vars = vars(LOC_ID, CULTIVAR, PROJECT, TR_N), .funs = ~str_replace_all(., "_", "")) %>%
    #     mutate(PDAT = as.Date(PDAT),
    #            exp_file  = paste(LOC_ID, CULTIVAR, PROJECT, TR_N, sep = "_") %>%
    #              paste0(path, .,".exp"))
    #
    #
    #
    # # Extrae datos por componente del archivo experiental
    #
    # # Datos  de fertilizacion
    # FERT <- nest(fertil, FERT_obs = - ID)
    #
    # # Datos de Fenologia
    # PHEN <- nest(phenol, PHEN_obs = - ID)
    #
    # # Datos de crecimiento y desarrollo
    # PLANT <- nest(plant, PLANT_obs = - ID)
    #
    # # Datos de Rendimiento
    # YIELD <-  nest(yield, YIELD_obs = - ID)
    #
    # if(any(colnames(exp_data) == "SBDUR")){} else {
    #     exp_data <- mutate(exp_data, SBDUR  = NA)
    # }
    #
    # if(any(colnames(exp_data) == "TRDAT")){} else {
    #     exp_data <- mutate(exp_data, TRDAT  = NA)
    # }
    #
    #
    # to_write_exp <- purrr::reduce(list(exp_data, FERT, PHEN, PLANT, YIELD), left_join, by = "ID")



    ########################################
    ### 0. Head_exp
    ########################################

    head_exp_oryza <- function(exp_file, CULTIVAR, PDAT){
        sink(file = exp_file, append = T)
        cat('*--------------------------------------------------------------------*',sep = '\n')
        cat('* EXPERIMENTAL DATA FILE                                             *',sep = '\n')
        cat('*                                                                    *',sep = '\n')
        cat(paste0('* File name        : ', str_replace(exp_file, path, ""), '                     *'), sep = '\n')
        cat(paste0('* Crop             : ', CULTIVAR, '                                       *') ,sep = '\n')
        cat(paste0('* Year/Season      : ', year(PDAT), '                                            *') ,sep = '\n')
        cat(paste0('* Additional info  : ', 'Create with agroclimR', '     *') ,sep = '\n')
        cat('*--------------------------------------------------------------------*',sep = '\n')
        cat('\n')

        sink()
    }
    #head_exp_oryza()
    ########################################
    ### 1. Selection of modes of running ###
    ########################################
    runmodes_oryza <- function(exp_file, ET_mod){
        sink(file = exp_file, append = T)
        cat('*--------------------------------------------------------------------*',sep = '\n')
        cat('* 1. Selection of modes of running                                   *',sep = '\n')
        cat('*--------------------------------------------------------------------*',sep = '\n')
        cat(paste0("RUNMODE = ","'EXPERIMENT'"),sep = '\n')
        cat(paste0("PRODENV = ", "'WATER BALANCE'"),sep = '\n')
        cat(paste0("WATBAL = ", "'PADDY'"), sep = '\n')
        cat(paste0("NITROENV = ", "'NITROGEN BALANCE'"),sep = '\n')
        cat(paste0("ETMOD = ", "'", ET_mod, "'"), sep = '\n')
        cat('\n')
        sink()
    }
    #runmodes_oryza()

    #####################################
    ### 2. Timer data for simulation  ###
    #####################################
    ## puede requerir If cuando STTIME < 0,
    #then IYEAR=IYEAR-1 ((as.POSIXlt(PHEN_obs$PDAT)$year + 1899)
    #and STTIME=365-(yday(PHEN_obs$PDAT)[i])
    #i <- 8
    timer_oryza <- function(exp_file, PHEN_obs){
        #    if (yday(INPUT_data$PHEN_obs$PDAT)[i]-3<0){
        #    a=year(INPUT_data$PHEN_obs$PDAT)[i]-1
        #    b=365+(yday(INPUT_data$PHEN_obs$PDAT)[i]-3)} else {
        #       a= year(INPUT_data$PHEN_obs$PDAT)[i]
        #       b= yday(INPUT_data$PHEN_obs$PDAT)[i]-3
        #    }
        a= year(PHEN_obs$EDAT)
        b= yday(PHEN_obs$EDAT)-3




        sink(file = exp_file, append = T)
        #for (i in 1:1){
        cat('*--------------------------------------------------------------------*',sep = '\n')
        cat('* 2. Timer data for simulation                                       *',sep = '\n')
        cat('*--------------------------------------------------------------------*',sep = '\n')
        cat(paste0("IYEAR = ", a),sep = '\n')
        cat(paste0("STTIME = ", ifelse(b<=0, 1, b), '.'),sep = '\n')
        cat(paste0("FINTIM = ", "1000."),sep = '\n')
        cat(paste0("DELT = ", "1."),sep = '\n')
        cat('\n')
        #}
        sink()
    }
    #timer_oryza()

    ############################################################
    ### 3. Weather station and climatic data for simulation  ###
    ############################################################

    wtrdir_oryza <- function(exp_file, LOC_ID){
        sink(file = exp_file, append = T)
        #    for (i in 1:1){
        cat('*--------------------------------------------------------------------*',sep = '\n')
        cat('* 3. Weather station and climatic data for simulation                *',sep = '\n')
        cat('*--------------------------------------------------------------------*',sep = '\n')
        cat( paste0(  "WTRDIR = "   ,"' WTH","\\",  "'"),sep = '\n')
        cat( paste0(  "CNTR = "     , "'", LOC_ID,"'")  ,sep = '\n')
        cat( paste0(  "ISTN = "     , 1 ) ,sep = '\n')
        cat( paste0(  "MULTIY = "   , "'NO'")              ,sep = '\n')
        cat( paste0(  "ANGA = "     , "0.29"),sep = '\n')
        cat( paste0(  "ANGB = "     , "0.45"),sep = '\n')
        cat( paste0(  "TMINCTB = "  ),sep = '\n')
        cat(paste0("0., ","0."),sep = '\n')
        cat(paste0("366., ","0."),sep = '\n')
        cat('\n')
        cat(paste0(  "TMAXCTB = "  ),sep = '\n')
        cat(paste0("0., ","0.,"),sep = '\n')
        cat(paste0("366., ","0."),sep = '\n')
        cat('\n')
        cat(paste0("FAOF = ", "1."),sep = '\n')
        cat( paste0("TMPSB = ", "0."),sep = '\n')
        cat('\n')
        #    }
        sink()
    }
    #wtrdir_oryza()

    ##############################
    ### 4. Establishment data  ###
    ##############################


    estab_oryza <- function(exp_file, ESTAB, SBDUR, PHEN_obs){
        sink(file = exp_file, append = T)
        #    for (i in 1:1){
        cat('*--------------------------------------------------------------------*',sep = '\n')
        cat('* 4. Establishment data                                              *',sep = '\n')
        cat('*--------------------------------------------------------------------*',sep = '\n')
        cat(paste0("ESTAB = ", "'",ESTAB, "'"),sep = '\n')
        cat(paste0("EMD    = ", yday(PHEN_obs$EDAT)),sep = '\n')
        cat(paste0("EMYR   = ", year(PHEN_obs$EDAT)),sep = '\n')
        cat(paste0("SBDUR  = ", if (ESTAB=="TRANSPLANT"){SBDUR}else{0}), sep = '\n')
        cat('\n')
        #    }
        sink()
    }
    #estab_oryza()

    ###################################
    ### 5. Management parameters  ###
    ###################################

    management_oryza <- function(exp_file, NPLDS){
        sink(file = exp_file, append = T)
        #    for (i in 1:1){
        cat('*--------------------------------------------------------------------*',sep = '\n')
        cat('* 5. Management parameters                                           *',sep = '\n')
        cat('*--------------------------------------------------------------------*',sep = '\n')
        #cat("NPLH   = ","2.0"),sep = '\n')
        #cat("NH     = ","33.0"),sep = '\n')
        #cat("NPLSB  = ","1000."),sep = '\n')
        cat(paste0("NPLDS  = ", sprintf("%1.f", NPLDS),"."), sep = '\n')
        cat(paste0("LAPE   = ", "0.0001"), sep = '\n')
        cat(paste0("DVSI   = ", "0.0"), sep = '\n')
        cat(paste0("WLVGI  = ", "0.0"), sep = '\n')
        cat(paste0("WSTI   = ", "0.0"), sep = '\n')
        cat(paste0("WRTI   = ", "0.0"), sep = '\n')
        cat(paste0("WSOI   = ", "0.0"), sep = '\n')
        cat(paste0("ZRTI   = ", "0.0001"),sep = '\n')
        cat(paste0("ZRTTR  = ", "0.05"), sep = '\n')
        cat('\n')
        #    }
        sink()
    }
    #management_oryza()

    ################################
    ### 6. Irrigation parameters ###
    ################################

    irrig_oryza <- function(exp_file, CROP_SYS){
        dvmx <- if (CROP_SYS=="RAINFED"){"0.0"} else{1.8}
        sink(file = exp_file, append = T)
        #    for (i in 1:1){
        cat('*--------------------------------------------------------------------*',sep = '\n')
        cat('* 6. Irrigation parameters                                           *',sep = '\n')
        cat('*--------------------------------------------------------------------*',sep = '\n')
        cat('* ',sep = '\n')
        cat(paste0("DVSIMAX = ",dvmx), sep = '\n')
        ##ICOMBA switch critical (1:4)-->c(yday,DVS,DVS-yday, DAE)
        cat(paste0("ICOMBA = ", 1), sep = '\n')
        cat("IRMTAB = ", sep = '\n')
        cat(paste0("0., ",if (CROP_SYS=="RAINFED"){0} else{2},".0,"),sep = '\n')
        cat(paste0("366., ",if (CROP_SYS=="RAINFED"){0} else{2}, ".0"),sep = '\n')
        cat(paste0("AUTODEPT = ", "-10.0"),sep = '\n')

        ##SWITIR Irrigation Settongs (1:6)---Automatic use SWITIR=2

        cat(paste0("SWITIR = ", (if (CROP_SYS=="RAINFED"){0} else{2})),sep = '\n')
        cat(paste0("RIRRIT ="),sep = '\n')
        cat(paste0("1., ",0,".0,"),sep = '\n')
        cat(paste0("365., ",0, ".0"),sep = '\n')
        cat('\n')
        cat(paste0("IRRI2   = ",50,"."),sep = '\n')
        cat(paste0("WL0MIN  = ",1,"."),sep = '\n')
        cat(paste0("IRRI3   = ",50,"."),sep = '\n')
        cat(paste0("KPAMIN  = ",70,"."),sep = '\n')
        cat(paste0("SLMIN3  = ",3),sep = '\n')
        cat(paste0("IRRI4   = ",50,"."),sep = '\n')
        cat(paste0("WCMIN   = ",0.30),sep = '\n')
        cat(paste0("SLMIN4  = ",3),sep = '\n')
        cat(paste0("IRRI5   = ",50,"."),sep = '\n')
        cat(paste0("WL0DAY  = ",5),sep = '\n')
        cat(paste0("IRRI6   = ",50, "."),sep = '\n')
        cat(paste0("SLMIN6  = ",3),sep = '\n')
        cat('\n')
        cat(paste0("ISTAGET ="),sep = '\n')
        cat(paste0("0.00, 0.20, 5.,"),sep = '\n')
        cat(paste0("1.70, 1.80, 5." ),sep = '\n')
        cat('\n')
        #    }
        sink()
    }
    #irrig_oryza()

    ###############################
    ### 7. Nitrogen parameters  ###
    ###############################

    nitrogen_oryza <- function(exp_file){
        sink(file = exp_file, append = T)
        #    for (i in 1:1){
        cat('*--------------------------------------------------------------------*',sep = '\n')
        cat('* 7. Nitrogen parameters                                             *',sep = '\n')
        cat('*--------------------------------------------------------------------*',sep = '\n')
        cat('*  ',sep = '\n')
        cat(paste0("NUTRIENT = ","'", "GENERAL SOM", "'"),sep = '\n')
        #"NUTRIENT = ","'", "FIXED SUPPLY", "'"
        cat(paste0("RECNIT ="),sep = '\n')
        cat(paste0("0.0, 0.30,"),sep = '\n')
        cat(paste0("0.2, 0.35,"),sep = '\n')
        cat(paste0("0.4, 0.50,"),sep = '\n')
        cat(paste0("0.8, 0.75,"),sep = '\n')
        cat(paste0("1.0, 0.75,"),sep = '\n')
        cat(paste0("2.5, 0.75"),sep = '\n')
        cat('\n')
        cat(paste0("SOILSP = ", 0.8),sep = '\n')
        cat('\n')

        #    }
        sink()
    }
    #nitrogen_oryza()

    Fert_tb <- function(exp_file, FERT_obs){

        nit <- dplyr::select(FERT_obs, DDE, N)

        a <- sprintf("%.1f", pull(nit, DDE))
        b <- sprintf("%.1f", pull(nit, N))
        c <- matrix(cbind(a,", ",b,","), ncol = 4)

        sink(file = exp_file, append = T)
        cat(paste0("FERTIL ="),sep = '\n')
        cat(paste0("0.0, 0.0,"),sep = '\n')
        write.table(c, col.names = F, sep="",row.names = F, quote = F)
        cat(paste0("366.0, 0.0"),sep = '\n')
        cat('\n')
        sink()
    }  # Need review. i
    #Fert_tb()

    ###############################################
    ### 8. Measured data for model calibration  ###
    ###############################################

    measure_phen_oryza <- function(exp_file, ESTAB, TRDAT, PHEN_obs){

        sink(file = exp_file, append = T)
        #    for (i in 1:1){
        cat('*--------------------------------------------------------------------*',sep = '\n')
        cat('* 8. Measured data for model calibration and comparison              *',sep = '\n')
        cat('*    And option to force measured LAI during simulation              *',sep = '\n')
        cat('*    (instead of using simulated values)                             *',sep = '\n')
        cat('*--------------------------------------------------------------------*',sep = '\n')
        cat('* Observed phenology: only required if program DRATES is run!!',sep = '\n')
        cat('\n')
        cat(paste0("IDOYTR = ", if (ESTAB=="DIRECT-SEED"){0}else{yday(TRDAT)}),  sep = '\n')
        cat(paste0("IYRTR  = ", if (ESTAB=="DIRECT-SEED"){0}else{year(TRDAT)}), sep = '\n')
        cat(paste0("IDOYPI = ", yday(PHEN_obs$IDAT)), sep = '\n')
        cat(paste0("IYRPI  = ", year(PHEN_obs$IDAT)), sep = '\n')
        cat(paste0("IDOYFL = ", yday(PHEN_obs$FDAT)), sep = '\n')
        cat(paste0("IYRFL  = ", year(PHEN_obs$FDAT)), sep = '\n')
        cat(paste0("IDOYM  = ", yday(PHEN_obs$MDAT)-7), sep = '\n') # resta 7 dias a la fecha de cosecha
        cat(paste0("IYRM   = ", year(PHEN_obs$MDAT)), sep = '\n')
        cat('\n')
        #    }
        sink()
    }
    #measure_phen_oryza()

    ##*!* Leaf Area Index (m2 leaf / m2 ground):
    LAI_tb <- function(exp_file, PLANT_obs){

        var <- cbind(year(PLANT_obs$SAMPLING_DATE),yday(PLANT_obs$SAMPLING_DATE), PLANT_obs$LAI_OBS)%>%
            na.omit()
        var2 <- var[-length(var[,1]),]

        if (length(var2)<1){

            message(paste0("No LAI in exp_file: ", exp_file ))

        } else {
            a <- sprintf("%.1f", var2[,1])
            b <- sprintf("%.1f", var2[,2])
            c <- sprintf("%.1f", var2[,3])
            d <- matrix(cbind(a,", ",b,", ",c,","), ncol = 6)

            sink(file = exp_file, append = T)
            cat('*!* Leaf Area Index (m2 leaf / m2 ground):',sep = '\n')
            cat(paste0("LAI_OBS ="),sep = '\n')
            write.table(d, col.names = F, sep="",row.names = F, quote = F)
            cat(paste0(sprintf("%.1f", var[length(var[,1]),1]),", ", sprintf("%.1f", var[length(var[,1]),2]), ", ", sprintf("%.1f", var[length(var[,1]),3])),sep = '\n')
            cat('\n')
            cat(paste0("LAI_FRC = ", 0),sep = '\n') # 0:No forcing ; 2: Forcing
            cat('\n')
            sink()
        }

    }       #*!* Leaf Area Index (m2 leaf / m2 ground):
    #LAI_tb()
    #Var=list()
    WLVG_tb <- function(exp_file, PLANT_obs){
        #    Var <- split(INPUT_data$PLANT_obs, INPUT_data$PLANT_obs$ID)
        if (any(is.na(PLANT_obs$WLVG_OBS)) || any(is.na(PLANT_obs$WST_OBS)) || any(is.na(PLANT_obs$WLVD_OBS)) || any(is.na(PLANT_obs$WSO_OBS)) || any(is.na(PLANT_obs$WAGT_OBS))){
            PLANT_obs <-na.omit(PLANT_obs)
            var <- cbind(year(PLANT_obs$SAMPLING_DATE),yday(PLANT_obs$SAMPLING_DATE), PLANT_obs$WLVG_OBS)%>%
                na.omit()
        } else {
            var <- cbind(year(PLANT_obs$SAMPLING_DATE),yday(PLANT_obs$SAMPLING_DATE), PLANT_obs$WLVG_OBS)
        }
        var2 <- var[-length(var[,1]),]

        a <- sprintf("%.1f", var2[,1])
        b <- sprintf("%.1f", var2[,2])
        c <- sprintf("%.1f", var2[,3])
        d <- matrix(cbind(a,", ",b,", ",c,","), ncol = 6)

        sink(file = exp_file, append = T)
        cat('*!* Green leaf dry wt (kg/ha)',sep = '\n')
        cat(paste0("WLVG_OBS ="),sep = '\n')
        write.table(d, col.names = F, sep="",row.names = F, quote = F)
        cat(paste0(sprintf("%.1f", var[length(var[,1]),1]),", ", sprintf("%.1f", var[length(var[,1]),2]), ", ", sprintf("%.1f", var[length(var[,1]),3])),sep = '\n')
        cat('\n')
        sink()
    }      #*!* Green leaf dry wt (kg/ha)
    #WLVG_tb()

    WLVD_tb <- function(exp_file, PLANT_obs){
        #    Var <- split(INPUT_data$PLANT_obs, INPUT_data$PLANT_obs$ID)
        if (any(is.na(PLANT_obs$WLVG_OBS)) || any(is.na(PLANT_obs$WST_OBS)) || any(is.na(PLANT_obs$WLVD_OBS)) || any(is.na(PLANT_obs$WSO_OBS)) || any(is.na(PLANT_obs$WAGT_OBS))){
            PLANT_obs <-na.omit(PLANT_obs)
            var <- cbind(year(PLANT_obs$SAMPLING_DATE),yday(PLANT_obs$SAMPLING_DATE), PLANT_obs$WLVD_OBS)%>%
                na.omit()
        } else {
            var <- cbind(year(PLANT_obs$SAMPLING_DATE),yday(PLANT_obs$SAMPLING_DATE), PLANT_obs$WLVD_OBS)
        }
        var2 <- var[-length(var[,1]),]

        a <- sprintf("%.1f", var2[,1])
        b <- sprintf("%.1f", var2[,2])
        c <- sprintf("%.1f", var2[,3])
        d <- matrix(cbind(a,", ",b,", ",c,","), ncol = 6)

        sink(file = exp_file, append = T)
        cat('*!* Dead leaf dry wt (kg/ha)',sep = '\n')
        cat(paste0("WLVD_OBS ="),sep = '\n')
        write.table(d, col.names = F, sep="",row.names = F, quote = F)
        cat(paste0(sprintf("%.1f", var[length(var[,1]),1]),", ", sprintf("%.1f", var[length(var[,1]),2]), ", ", sprintf("%.1f", var[length(var[,1]),3])),sep = '\n')
        cat('\n')
        sink()
    }      #*!* Dead leaf dry wt (kg/ha)
    #WLVD_tb()

    WST_tb <- function(exp_file, PLANT_obs){
        #    Var <- split(INPUT_data$PLANT_obs, INPUT_data$PLANT_obs$ID)
        if (any(is.na(PLANT_obs$WLVG_OBS)) || any(is.na(PLANT_obs$WST_OBS)) || any(is.na(PLANT_obs$WLVD_OBS)) || any(is.na(PLANT_obs$WSO_OBS)) || any(is.na(PLANT_obs$WAGT_OBS))){
            PLANT_obs <-na.omit(PLANT_obs)
            var <- cbind(year(PLANT_obs$SAMPLING_DATE),yday(PLANT_obs$SAMPLING_DATE), PLANT_obs$WST_OBS)%>%
                na.omit()
        } else {
            var <- cbind(year(PLANT_obs$SAMPLING_DATE),yday(PLANT_obs$SAMPLING_DATE), PLANT_obs$WST_OBS)
        }
        var2 <- var[-length(var[,1]),]

        a <- sprintf("%.1f", var2[,1])
        b <- sprintf("%.1f", var2[,2])
        c <- sprintf("%.1f", var2[,3])
        d <- matrix(cbind(a,", ",b,", ",c,","), ncol = 6)

        sink(file = exp_file, append = T)
        cat('*!* Stem dry wt (kg/ha)',sep = '\n')
        cat(paste0("WST_OBS ="),sep = '\n')
        write.table(d, col.names = F, sep="",row.names = F, quote = F)
        cat(paste0(sprintf("%.1f", var[length(var[,1]),1]),", ", sprintf("%.1f", var[length(var[,1]),2]), ", ", sprintf("%.1f", var[length(var[,1]),3])),sep = '\n')
        cat('\n')
        sink()
    }       #*!* Stem dry wt (kg/ha)
    #WST_tb()

    WSO_tb <- function(exp_file, PLANT_obs){
        #   Var <- split(INPUT_data$PLANT_obs, INPUT_data$PLANT_obs$ID)
        if (any(is.na(PLANT_obs$WLVG_OBS)) || any(is.na(PLANT_obs$WST_OBS)) || any(is.na(PLANT_obs$WLVD_OBS)) || any(is.na(PLANT_obs$WSO_OBS)) || any(is.na(PLANT_obs$WAGT_OBS))){
            PLANT_obs <-na.omit(PLANT_obs)
            var <- cbind(year(PLANT_obs$SAMPLING_DATE),yday(PLANT_obs$SAMPLING_DATE), PLANT_obs$WSO_OBS)%>%
                na.omit()
        } else {
            var <- cbind(year(PLANT_obs$SAMPLING_DATE),yday(PLANT_obs$SAMPLING_DATE), PLANT_obs$WSO_OBS)
        }
        var2 <- var[-length(var[,1]),]

        a <- sprintf("%.1f", var2[,1])
        b <- sprintf("%.1f", var2[,2])
        c <- sprintf("%.1f", var2[,3])
        d <- matrix(cbind(a,", ",b,", ",c,","), ncol = 6)

        sink(file = exp_file, append = T)
        cat('*!* Panicle dry wt (kg/ha)',sep = '\n')
        cat(paste0("WSO_OBS ="),sep = '\n')
        write.table(d, col.names = F, sep="",row.names = F, quote = F)
        cat(paste0(sprintf("%.1f", var[length(var[,1]),1]),", ", sprintf("%.1f", var[length(var[,1]),2]), ", ", sprintf("%.1f", var[length(var[,1]),3])),sep = '\n')
        cat('\n')
        sink()
    }       #*!* Panicle dry wt (kg/ha)
    #WSO_tb()

    WAGT_tb <- function(exp_file, PLANT_obs){
        #    Var <- split(INPUT_data$PLANT_obs, INPUT_data$PLANT_obs$ID)
        if (any(is.na(PLANT_obs$WLVG_OBS)) || any(is.na(PLANT_obs$WST_OBS)) || any(is.na(PLANT_obs$WLVD_OBS)) || any(is.na(PLANT_obs$WSO_OBS)) || any(is.na(PLANT_obs$WAGT_OBS))){
            PLANT_obs <-na.omit(PLANT_obs)
            var <- cbind(year(PLANT_obs$SAMPLING_DATE),yday(PLANT_obs$SAMPLING_DATE), PLANT_obs$WAGT_OBS)%>%
                na.omit()
            message(paste0("## Exist NA in Growth Tables! ##-->EXP:", exp_file))
        } else {
            var <- cbind(year(PLANT_obs$SAMPLING_DATE),yday(PLANT_obs$SAMPLING_DATE), PLANT_obs$WAGT_OBS)
        }
        var2 <- var[-length(var[,1]),]

        a <- sprintf("%.1f", var2[,1])
        b <- sprintf("%.1f", var2[,2])
        c <- sprintf("%.1f", var2[,3])
        d <- matrix(cbind(a,", ",b,", ",c,","), ncol = 6)

        sink(file = exp_file, append = T)
        cat('*!* Total dry wt (kg/ha)',sep = '\n')
        cat(paste0("WAGT_OBS ="),sep = '\n')
        write.table(d, col.names = F, sep="",row.names = F, quote = F)
        cat(paste0(sprintf("%.1f", var[length(var[,1]),1]),", ", sprintf("%.1f", var[length(var[,1]),2]), ", ", sprintf("%.1f", var[length(var[,1]),3])),sep = '\n')  #sprintf("%.1f", as.numeric(var[length(var[,1]),2])-7)
        cat('\n')
        sink()
    }


    ###### Write EXP file

    write_exp <- function(exp_file, LOC_ID, CULTIVAR, PDAT, ESTAB, SBDUR, NPLDS, CROP_SYS, TRDAT, FERT_obs, PHEN_obs, PLANT_obs, ET_mod){

        head_exp_oryza(exp_file, CULTIVAR, PDAT)
        runmodes_oryza(exp_file, ET_mod)
        timer_oryza(exp_file, PHEN_obs)
        wtrdir_oryza(exp_file, LOC_ID )
        estab_oryza(exp_file, ESTAB, SBDUR, PHEN_obs )
        management_oryza(exp_file, NPLDS)
        irrig_oryza(exp_file, CROP_SYS)
        nitrogen_oryza(exp_file)
        Fert_tb(exp_file, FERT_obs)
        measure_phen_oryza(exp_file, ESTAB, TRDAT, PHEN_obs)
        LAI_tb(exp_file, PLANT_obs)
        WLVG_tb(exp_file, PLANT_obs)
        WLVD_tb(exp_file, PLANT_obs)
        WST_tb(exp_file, PLANT_obs)
        WSO_tb(exp_file, PLANT_obs)
        WAGT_tb(exp_file, PLANT_obs)

    }

    write_exp(exp_file, LOC_ID, CULTIVAR, PDAT, ESTAB, SBDUR, NPLDS, CROP_SYS, TRDAT, FERT_obs, PHEN_obs, PLANT_obs, ET_mod)

    # dplyr::select(to_write_exp, exp_file, LOC_ID, CULTIVAR, PDAT, ESTAB, SBDUR, NPLDS, CROP_SYS, TRDAT, FERT_obs, PHEN_obs, PLANT_obs)  %>%
    #     mutate(file = pmap(., write_exp
    exp_file

}

#
# tidy_exp_oryza(agro, phenol, plant, fertil, yield, path = "./")  %>%
#   mutate(file = pmap(., write_exp_oryza)) %>% pull(exp_file)


  # helpers -----------------------------------------------------------------
# Function to tidy experimental data to create Oryza experimental files
tidy_exp_oryza <- function(agro, phenol, plant, fertil, yield, path = "./"){

  exp_data <- agro %>%
    mutate_at(.vars = vars(LOC_ID, CULTIVAR, PROJECT, TR_N), .funs = ~str_replace_all(., "_", "")) %>%
    mutate(PDAT = as.Date(PDAT),
           exp_file  = file.path(path, paste0(paste(LOC_ID, CULTIVAR, PROJECT, TR_N, sep = "_"), ".exp")))

  if(any(colnames(exp_data) == "SBDUR")){} else {
      exp_data <- mutate(exp_data, SBDUR  = NA)
  }

  if(any(colnames(exp_data) == "TRDAT")){} else {
      exp_data <- mutate(exp_data, TRDAT  = NA)
  }

  # Extrae datos por componente del archivo experiental

  # Datos  de fertilizacion
  FERT <- nest(fertil, FERT_obs = - ID)

  # Datos de Fenologia
  PHEN <- nest(phenol, PHEN_obs = - ID)

  # Datos de crecimiento y desarrollo
  PLANT <- nest(plant, PLANT_obs = - ID)

  # Datos de Rendimiento
  YIELD <-  nest(yield, YIELD_obs = - ID)

  purrr::reduce(list(exp_data, FERT, PHEN, PLANT, YIELD), left_join, by = "ID") %>%
    dplyr::select(exp_file, LOC_ID, CULTIVAR, PDAT, ESTAB, SBDUR, NPLDS, CROP_SYS, TRDAT, FERT_obs, PHEN_obs, PLANT_obs)
}
