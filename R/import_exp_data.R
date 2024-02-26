#' Import Experimental data from agroclimR data workbook
#'
#' Function to extract experimental data from agroclimR data workbook
#'
#' @param path A string indicating path folder or working directory
#' @param files A String vector. Vector of strings from workbooks names
#' @param model A string of workbook names
#' @import dplyr
#' @import stringr
#' @import readxl
#' @import purrr
#' @import tibble
#' @export
#' @examples
#' # Import Experimental data from agroclimR data workbook
#' #Assuming you have n workbooks with agroclimR data workbook format in a specific folder \code{path}
#' path = "data/"
#' files <- list.files(path = path, pattern = "xls")
#' obs_data <- import_exp_data(path = path, files, model = "oryza")
#'
## Update the details for the return value
#' @return This function returns a \code{list} of raw data and observed data by component (phen, dry_matter, lai and yield)
#'
# @seealso \link[sirad]{se}



import_exp_data <- function(path = ., files, model = "oryza"){

  # listar los archivos o listas de experimentos disponibles en la carpeta de datos

  # files_cultivar <- list.files(path, pattern = fixed(cultivar))

  ## Importa datos de trabajo
  data <- files %>%
    enframe(name = NULL, value = "file") %>%
 #   mutate(loc_cul = str_sub(file, 1,-6)) %>%
  #  separate(col = loc_cul, into = c("site", "cultivar"), sep = "_") %>%
    mutate(input_data =  map(file, ~read_agroclimr_data(paste0(path, .))))


  ## Extrae agro data
  agro <- data$input_data %>% map("AGRO_man") %>% bind_rows() %>%
    dplyr::select(LOC_ID, contains("LAT"), contains("LONG"), starts_with("A")) %>%
    distinct() %>% set_names(c("site", "lat", "lon", "elev")) %>%
    group_by(site) %>% slice(1) %>%
    ungroup()


  ## Extrae datos de suelo
  soil <- data %>%
    mutate(soil_data = map(input_data, ~.x$SOIL_obs))  %>%
    dplyr::select(-input_data)  %>%
    unnest(soil_data) %>%
   # mutate(LOC_ID = str_sub(ID, 1, 4)) %>%
    group_by(LOC_ID, NL) %>%
    summarize_if(is.numeric, .funs = mean) %>%
    mutate(ID=LOC_ID, STC = get_STC(SAND, CLAY)) %>% ungroup() %>%
    split(.$ID) %>%
    enframe(name = "site", value = "soil")


  ## Extrae datos de clima

  wth <- data %>% mutate(wth = map(input_data, ~.x$WTH_obs )) %>%
    select(file, wth) %>%
    #  group_by(localidad) %>% slice(1) %>%
    unnest(wth) %>%
    mutate(DATE = as.Date(DATE)) %>%
    #  mutate(wspd = suppressWarnings(as.numeric(WVEL))) %>% ###### si existen ?
    set_names(tolower(names(.))) %>%
    dplyr::select(-file) %>%
    dplyr::distinct() %>%
    nest(wth = -c(loc_id, ws_id)) %>% rename(site = loc_id ) %>%
    left_join(agro, by = "site") #%>% rename(stn = ws_id) #%>%
  #  mutate(path = "data/OUTPUTS/WTH/") %>%
  #  select(path, id_name, wth_data, lat, lon, elev, stn)

  # Extract and plot - Growth and Development observed data by component

  # Crop Phenology
  phen <- extract_obs_var(data$input_data, "phen", model)
  #  plot_phen_obs(phen) %>% ggplotly()

  #Leaf Area Index
  lai <- extract_obs_var(data$input_data, "lai", model)
  #  plot_lai_obs(lai) %>% ggplotly()

  #Shoot Dry Matter
  dry_matter <- extract_obs_var(data$input_data, "dry_matter", model)
  #  plot_drymatter_obs(dry_matter) %>% ggplotly()

  #Yield dry matter
  yield <- extract_obs_var(data$input_data, "yield", model)
  #  plot_yield_obs(yield) %>% ggplotly()



  # crea lista de datos extraidos
  data_list <- list(

    data =  data %>% left_join(soil, by = "site") %>% left_join(wth, by = "site"),

    phen =  phen, lai = lai, dry_matter = dry_matter, yield = yield

  )




  return(data_list)


}


# helpers -----------------------------------------------------------------


# 'read_agroclimr_data' function to read xlsx files ---->  c(LOC_ID, cultivar), base_raw_data
# file <- agroclimR data_workbook file
read_agroclimr_data <- function(file) {

  sheets <- readxl::excel_sheets(file)
  x <-    lapply(sheets, function(X) readxl::read_excel(file, sheet = X))
  names(x) <- sheets

  return(x)

}



# variable  <- c("phen", "lai", "dry_matter", "yield")
#' Extract from base data - agroclimR data format xlsx
#'
#' Function to extract experimental data by variable from agroclimR data workbook
#'
#' @param obs_data A list of agroclimR data_workbooks read by  \code{read_agroclimr_data} function.
#' @param variable A String value. options = variable  <- c("phen", "lai", "dry_matter", "yield")
#' @param model A string of model name. options = model  <- c("oryza", "dssat", "aquacrop"),
#' @import dplyr
#' @import stringr
#' @import readxl
#' @import purrr
#' @import tibble
#' @export
#' @examples
#' # Extract Experimental data by variable from agroclimR data workbook
#'
#' obs_data  = list(list(AGRO_man = agro,
#' FERT_obs = fertil,
#' PHEN_obs = phenol,
#' PLANT_obs = plant,
#' YIELD_obs = yield,
#' SOIL_obs = soil,
#' WTH_obs = weather))
#'
#' phenological_data <- extract_obs_var(obs_data, "phen", model = "oryza")
#'
#' print(phenological_data)
#'
#'
## Update the details for the return value
#' @return This function returns a \code{tibble} with de data for the specific selected variable ( "phen", "lai", "dry_matter" or  "yield") and model.
#'
# @seealso \link[https://jrodriguez88.github.io/agroclimR/reference/import_exp_data.html]{se}

extract_obs_var <- function(obs_data, variable, model = "oryza") {

  # vars select sheet names required
  vars <- switch(variable,
                 dry_matter = "PLANT_gro",
                 lai = "PLANT_gro",
                 yield = "YIELD_obs",
                 phen = "PHEN_obs")

  date_to_dae <- function(data) {

    edate <- data %>% filter(var == "EDAT") %>% pull(value)

    data %>% mutate(value = as.numeric(value - edate)) %>%
      dplyr::filter(var != "PDAT", var != "EDAT")

  }

  #   set <- exp_set %>%
  #       str_sub(1,-5) %>% enframe(name = NULL, value = "exp_file") %>%
  #       separate(exp_file, c("LOC_ID", "CULTIVAR","PROJECT", "TR_N"), sep = "_", remove = F) %>%
  #       mutate(ID = paste0(LOC_ID, TR_N, PROJECT))

  remove_unders <- function(var){str_replace_all(var, "_", "")}

  set <- obs_data %>%
    map(., ~.[["AGRO_man"]]) %>% bind_rows() %>%
    mutate_at(.vars = vars(LOC_ID, CULTIVAR, PROJECT, TR_N), .funs = remove_unders) %>%
    mutate(exp_file = paste(LOC_ID, CULTIVAR, PROJECT, TR_N, sep = "_")) %>%
    dplyr::select(c(ID,	exp_file, LOC_ID,	PROJECT,	CULTIVAR,	TR_N))





  obs_data2 <- obs_data %>%
    map(., ~.[[vars]]) %>%
    bind_rows() %>%
    dplyr::select(-LOC_ID, -CULTIVAR) %>%
    nest(data = -c(ID)) %>% right_join(set, by= "ID") %>% unnest(data) %>%
    select(-c(LOC_ID, CULTIVAR, PROJECT, TR_N))






  op <- switch(variable,
               dry_matter = obs_data2 %>%
                 mutate(SAMPLING_DATE =  as.Date(SAMPLING_DATE)) %>%
                 rename(date = SAMPLING_DATE) %>%
                 select(ID:WAGT_SE, exp_file, -contains("LAI")) %>%
                 gather(var, value, -c(ID, exp_file, date)) %>%
                 separate(var, c("var", "metric"), sep = "_") %>%
                 spread(metric, value) %>%
                 rename(value = OBS, se = SE) %>%
                 dplyr::select(exp_file, date, var, value, se),
               lai = obs_data2 %>%
                 mutate(SAMPLING_DATE =  as.Date(SAMPLING_DATE)) %>%
                 rename(date=SAMPLING_DATE) %>%
                 select(exp_file, date, contains("LAI")) %>%
                 mutate(var = "LAI") %>%
                 rename(value=LAI_OBS, se=LAI_SE) %>%
                 dplyr::select(exp_file, date, var, value, se),
               yield = obs_data2 %>%
                 dplyr::select(exp_file, YIELD_AVG, YIELD_MIN, YIELD_MAX) %>%
                 rename(value = YIELD_AVG, ymin = YIELD_MIN, ymax = YIELD_MAX) %>%
                 mutate(var = "YIELD", diff_min = value - ymin,
                        diff_max = ymax - value,
                        se = (diff_max+diff_min)/2) %>%
                 dplyr::select(exp_file, var, value, se),
               phen = obs_data2 %>%
                 dplyr::select(-ID) %>%
                 gather(var, value, -exp_file) %>%
                 mutate(value = as.Date(value)) %>%
                 nest(data = -exp_file) %>%
                 mutate(phen_dae = map(data, ~date_to_dae(.x))) %>%
                 unnest(phen_dae) %>%
                 mutate(value = if_else(var == "MDAT", value - 7 , value)))



  if(all(variable == "lai", model == "aquacrop")){

    phen_data <- obs_data %>%
      map(., ~.[["PHEN_obs"]]) %>%
      bind_rows() %>% left_join(set, by = join_by(ID, LOC_ID, CULTIVAR)) %>%
      dplyr::select(exp_file, IDAT, FDAT) %>% mutate(across(contains("DAT"), as.Date))



    # canopy cover data.frame
    # Convert CC = 1 - exp(-k*LAI))

    op <- op %>% na.omit() %>% nest(data = -exp_file) %>% left_join(phen_data, by = join_by(exp_file)) %>%
      mutate(
        data = pmap(list(data, IDAT, FDAT), function(a,b,c){
          a %>%
            mutate(
              k = case_when(
                date <= b ~ 0.4,
                date >= c ~ 0.6,
                TRUE ~ 0.5),
              canopy = (1 - exp(-k*value))*100,
              se = canopy*(se/value))})) %>% unnest(data) %>%
      dplyr::select(exp_file, date, var, value = canopy, se)

  }




  return(op)


}



