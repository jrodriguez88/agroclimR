#' Extract Development rates and growth parameters
#'
#' This function help to setup and compute the Development rates and Rice growing parameters requiere in ORYZA model
#'
#'
#'
#' @noRd

extract_drates_param <-
function(exp_names, path) {


    wd <- getwd()
    setwd(path)

    #file <- "DRATE.OUT"
    #file <- "param.out"

    ################################
    ### Create EXP data.frame
    ################################
    # Make Master Table called "exp_df"

    #exp_names <- list.files("EXP",pattern = "\\.exp$")
    #exp_names <- str_subset(list.files("EXP",pattern = "\\.exp$"), cultivar)
    exp_df <- exp_names %>%
        str_sub(1,-5) %>% enframe(value = "exp_file", name = NULL)


    ################################
    ### Create DVR data.frame
    ################################

    read_DVR_drate <- function(file){

        find_DVR <- file %>%
            read_lines() %>%
            str_detect(pattern = "crop development") %>%
            which()


        DVR <- list()
        for (i in 1:length(find_DVR)){
            DVR[[i]] <- read_lines(file, skip = find_DVR[i], n_max = 4) %>%
                str_split(pattern = "=") %>%
                sapply("[",2) %>%
                as.numeric() %>%
                matrix(ncol = 4) %>%
                as_tibble()%>%
                bind_cols(exp_df[i,])

        }

        #    pmap(list(file=file, skip=find_DVR, n_max=4), read_lines) %>%
        #        map(., ~ str_split(., pattern = "=")) %>% map(., ~ bind_rows(.))

        DVR_df <- bind_rows(DVR) %>%
            set_names(c("DVRJ", "DVRI", "DVRP", "DVRR", "exp_file")) %>%
            dplyr::select(exp_file, everything())

        return(DVR_df)

    }

    #str(DVR)

    ################################
    ### Create PHEN data.frame
    ################################

    read_PHEN_drate <- function(file){
        find_TS <- file %>%
            read_lines() %>%
            str_detect(pattern = "TSTR") %>%
            which()

        TS <- list()
        for (i in 1:length(find_TS)){
            TS[[i]] <- read_lines(file, skip = find_TS[i], n_max = 1) %>%
                str_split(pattern = ",")

            TS[[i]] <- as.data.frame(do.call("rbind",TS[[i]]))
        }

        GDD <- list()
        for (i in 1:length(find_TS)){
            GDD[[i]] <- read_lines(file, skip = find_TS[i]+2, n_max = 1) %>%
                str_split(pattern = ",")
            GDD[[i]] <- as.data.frame(do.call("rbind",GDD[[i]]))
        }

        DAE <- list()
        for (i in 1:length(find_TS)){
            DAE[[i]] <- read_lines(file, skip = find_TS[i]+4, n_max = 1) %>%
                str_split(pattern = ",")
            DAE[[i]] <- as.data.frame(do.call("rbind",DAE[[i]]))
        }
        PHEN <- list()
        for (i in 1:length(find_TS)) {
            PHEN[[i]] <- cbind(TS[[i]], GDD[[i]], DAE[[i]]) %>%
                bind_cols(exp_df[i,])
        }



        PHEN_df <- bind_rows(PHEN)%>%
            `colnames<-`(c("TSTR", "TSPI", "TSF",  "TSM",
                           "TGDDTR","TGDDPI","TGDDF","TGDDM",
                           "DAETR" , "DAEPI" , "DAEF"  , "DAEM" ,
                           "exp_file")) %>%
            mutate_at(1:12, as.character) %>% mutate_at(1:12, as.numeric) %>%
            dplyr::select(exp_file, everything())

        return(PHEN_df)

    }
    #
    #str(PHEN_df)

    ###############################
    ### Create Biomass data.frame
    ###############################

    read_biomass_param <- function(file){

        find_BM <- file %>%
            read_lines() %>%
            str_detect(pattern ="biomass values") %>%
            which()
        end_BM <- file %>%
            read_lines()%>%
            str_detect(pattern = " Observed LAI values")%>%
            which()%>%-1

        BM <- list()
        for (i in 1:length(find_BM)){

            BM[[i]] <- suppressWarnings(fread(file, skip = find_BM[i], nrows = end_BM[i]-find_BM[i], showProgress = FALSE)) %>%
                as_tibble() %>%
                mutate(DVS  = as.numeric(DVS),
                       WLVG = as.numeric(WLVG),
                       WLVD = as.numeric(WLVD),
                       WST  = as.numeric(WST),
                       WSO  = as.numeric(WSO),
                       exp_file =  pull(exp_df[i,]))
        }

        BM_df <- bind_rows(BM) %>%
            dplyr::select(exp_file, everything())

        return(BM_df)

    }

    #str(Biomass)

    ###############################
    ### Create Biomass Partition data.frame
    ###############################

    read_biomass_partition_param <- function(file){

        find_PF <- file %>%
            read_lines() %>%
            str_detect(pattern ="FSO") %>%
            which()%>%-1
        end_PF <- file %>%
            read_lines() %>%
            str_detect(pattern = "Calculated relative death rate leaves ") %>%
            which()%>%-1


        PF <- list()
        for (i in 1:length(find_PF)){

            PF[[i]] <- suppressWarnings(fread(file, skip = find_PF[i] , nrows = end_PF[i]-find_PF[i], showProgress = FALSE)) %>%
                mutate(DVSM  = as.numeric(DVSM),
                       FLV = as.numeric(FLV),
                       FST = as.numeric(FST),
                       FSO  = as.numeric(FSO),
                       exp_file =  pull(exp_df[i,]))
        }

        PF_df <- bind_rows(PF) %>%
            dplyr::select(exp_file, everything())

        return(PF_df)

    }

    #str(BPartition_f)

    ###############################
    ### Create LAI data.frame
    ###############################

    read_LAI_param <- function(file){
        find_LAI <- file %>%
            read_lines() %>%
            str_detect(pattern = "Observed LAI") %>%
            which()

        End_LAI <- file %>%
            read_lines() %>%
            str_detect(pattern = "Calculated ORYZA1") %>%
            which() %>%
            -2
        LAI <- list()
        for (i in 1:length(find_LAI)){
            LAI[[i]] <- suppressMessages(fread(file, skip = find_LAI[i], nrows = End_LAI[i]-find_LAI[i])) %>%
                as_tibble()%>%
                mutate(exp_file =  pull(exp_df[i,]))

            if (nrow(LAI[[i]])<1){
                LAI[[i]]<- data.frame(DVS=NA, LAI=NA) %>%
                    mutate(exp_file =  pull(exp_df[i,]))
            }


        } #Extract LAI from param.out --> list()

        LAI_df <- bind_rows(LAI) %>%
            dplyr::select(exp_file, everything())
        #        na.omit()
        #    `colnames<-`(c("DVS", "LAI","ID","LOC_ID", "CULTIVAR", "PROJECT", "TR_N"))

        #   as.numeric(gsub("([0-9]+).*$", "\\1", LAI_df$DVS))
        return(LAI_df)

    }

    #str(LAI)
    #LAI_SLA <- dplyr::left_join(LAI, Biomass, by="ID")
    #file <- "DRATE.OUT"



    ###############################
    ### Create FSTR data.frame
    ###############################

    read_FSTR_param <- function(file) {

        find_FSTR <- file %>%
            read_lines() %>%
            str_detect(pattern = "FSTR") %>%
            which()-1

        pmap(list(file=file, skip=find_FSTR, nrows=1), fread) %>%
            bind_rows() %>%
            bind_cols(exp_df) %>%
            dplyr::select(exp_file, everything())

    }



    ###############################
    ### Create DRLV data.frame
    ###############################

    read_DRLV_param <- function(file) {

        find_DRLV <- file %>%
            read_lines() %>%
            str_detect(pattern = "DRLV") %>%
            which()-1

        drlv_n <- file %>%
            read_lines() %>%
            str_detect(pattern = "Calculated fraction stem reserves") %>%
            which()-2

        drlv_list <- pmap(list(file=file, skip=find_DRLV, nrows=drlv_n - find_DRLV), fread)
        names(drlv_list) <- exp_df$exp_file

        drlv_list %>% bind_rows(.id="exp_file")
    }


    ##############################
    ### Create csv files
    ##############################

    #file.names <-list("DVR_df", "PHEN_df", "BMASS_df", "BPART_df", "LAI_df")
    #    write.csv.df <- function(df){
    #        df.name <- deparse(substitute(df))
    #        write.csv(df, paste0(path,"//_OutPut_Df//",df.name,".csv"),row.names=F,quote =F)
    #    }
    #    write.csv.df(DVR_df)
    #    write.csv.df(PHEN_df)
    #    write.csv.df(BMASS_df)
    #    write.csv.df(BPART_df)
    #    write.csv.df(LAI_df)

    data_extracted <- list(

        DVR_df = read_DVR_drate("DRATE.OUT"),
        PHEN_df = read_PHEN_drate("DRATE.OUT"),
        BMASS_df = read_biomass_param("param.out"),
        BPART_df = read_biomass_partition_param("param.out"),
        LAI_df = read_LAI_param("param.out"),
        FSTR_df = read_FSTR_param("param.out"),
        DRLV_df = read_DRLV_param("param.out")
    )

    # set project directory
    setwd(wd)
    return(data_extracted)



}
