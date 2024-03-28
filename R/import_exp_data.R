#' Import Experimental Data from agroclimR Data Workbook
#'
#' `import_exp_data()` performs extraction of experimental data from the agroclimR data workbook, including
#' location, soil, climate data, and observed data for phenology, leaf area index,
#' dry matter, and yield.
#'
# @param path String indicating the path folder or working directory.
#' @param files Vector of strings with workbook names (.xls extension file included).
#' @param model String indicating the model name, default "oryza",options: ("dssat", "aquacrop").
# @import magrittr
#' @import dplyr
#' @import stringr
# @import readxl
#' @import purrr
#' @import tibble
#' @import tidyr
#' @importFrom readxl excel_sheets
#' @importFrom readxl read_excel
#' @export
#' @examples
#' #' # File names vector, extension include
#' name_file = c("agroclimR_workbook.xlsx")
#'
#' # Files directory
#' test_file = system.file("extdata", name_file, package = "agroclimR")
#'
#' # Import data to R lists and tibble formats
#' obs_data = import_exp_data(test_file, model = "oryza")
#'
#' head(obs_data)
#' @returns List containing raw and observed data by component ("data", "soil", "wth", "phen", "lai", "dry_matter", "yield").
# @seealso \link[?sirad]{sirad}


import_exp_data <- function(files, model = "oryza"){


  # Import and process data files
  data <- files %>%
    tibble::enframe(name = NULL, value = "file") %>%
 #   mutate(loc_cul = str_sub(file, 1,-6)) %>%
  #  separate(col = loc_cul, into = c("site", "cultivar"), sep = "_") %>%
    dplyr::mutate(input_data =  purrr::map(file, ~ agroclimR::read_agroclimr_data(.x)))


  ## Extrae location data
  location_data <- data$input_data %>% purrr::map("AGRO_man") %>% dplyr::bind_rows() %>%
    dplyr::select(LOC_ID, LAT, LONG, ALT) %>% #contains("LAT"), contains("LONG"), starts_with("A")) %>%
    distinct() %>% set_names(c("site", "lat", "lon", "elev")) %>%
    group_by(site) %>% slice(1) %>%
    ungroup()


  # Extract soil data

  soil_data <- data %>%
    dplyr::mutate(soil_data = map(input_data, ~ .x$SOIL_obs))  %>%
    dplyr::select(-input_data)  %>%
    tidyr::unnest(soil_data) %>%
   # mutate(LOC_ID = str_sub(ID, 1, 4)) %>%
    group_by(LOC_ID, NL) %>%
    summarize_if(is.numeric, .funs = mean) %>%
    mutate(ID=LOC_ID, STC = agroclimR::get_STC(SAND, CLAY)) %>% ungroup() %>%
    split(.$ID) %>%
    tibble::enframe(name = "site", value = "soil") %>%
    left_join(location_data, by = "site")


  ## Extrae datos de clima

  wth_data <- data %>% dplyr::mutate(wth = purrr::map(input_data, ~.x$WTH_obs )) %>%
    select(file, wth) %>%
    #  group_by(localidad) %>% slice(1) %>%
    tidyr::unnest(wth) %>%
    dplyr::mutate(DATE = as.Date(DATE)) %>%
    #  mutate(wspd = suppressWarnings(as.numeric(WVEL))) %>% ###### si existen ?
    set_names(tolower(names(.))) %>%
    dplyr::select(-file) %>%
    dplyr::distinct() %>%
    nest(wth = -contains("id")) %>% rename(site = loc_id ) %>%
    left_join(location_data, by = "site") #%>% rename(stn = ws_id) #%>%
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

    data =  data , soil = soil_data, wth = wth_data,

    phen =  phen, lai = lai, dry_matter = dry_matter, yield = yield

  )




  return(data_list)


}


# Helper Functions --------------------------------------------------------

#' Read agroclimR Data
#'
#' Reads data from agroclimR workbook sheets.
#'
#' @param workbook_name String with the workbook name (full name).
#' @returns agroclimR list. A List of data frames for each sheet in the workbook.
#' @export
#' @rdname import_exp_data
#' @examples
#' #' # File names vector, extension include
#' name_file = c("agroclimR_workbook.xlsx")
#'
#' # Files directory
#' test_file = system.file("extdata", name_file, package = "agroclimR")
#'
#' # Import data to R lists and tibble formats
#' agroclimr_list = read_agroclimr_data(test_file)
#' agroclimr_list
#'
read_agroclimr_data <- function(workbook_name) {
  sheets <- readxl::excel_sheets(workbook_name)
  data <- lapply(sheets, function(sheet) readxl::read_excel(workbook_name, sheet = sheet))
  names(data) <- sheets
  return(data)
}



#' Extract Experimental Data by Variable from agroclimR Data Workbook
#'
#' Extracts experimental data based on specified variables from a set of agroclimR data workbooks. This function is tailored to handle data in the agroclimR xlsx format, facilitating the retrieval of agricultural research data for different variables and models. It supports a range of variables such as phenological data, leaf area index, dry matter, and yield, across various crop models.
#'
#' @param obs_data A list containing one or more agroclimR data workbooks, as read by the `read_agroclimr_data()` function. Each element of the list should be a named list representing a single workbook, where each name-value pair corresponds to a specific type of observational data.
#' @param variable A character string specifying the variable to extract. Valid options are "phen" (phenological data), "lai" (leaf area index), "dry_matter" (dry matter), and "yield". This parameter determines which type of data the function will extract from the provided workbooks.
#' @param model A character string specifying the crop model for which data is being extracted. Valid options include "oryza" (for rice), "dssat" (for various crops), and "aquacrop" (for water-driven crop growth). This parameter allows the function to tailor the extraction process to the data structure used by different crop models.
#' @returns A `tibble` containing the extracted data for the specified variable and model. The returned tibble is structured to facilitate further analysis and visualization, making it a valuable resource for agricultural researchers and analysts.
#' @examples
#' # Prepare a sample list of agroclimR data workbooks
#'
#' agroclimR_list <- list(AGRO_man = agro,
#'     FERT_obs = fertil,
#'     PHEN_obs = phenol,
#'     PLANT_obs = plant,
#'     YIELD_obs = yield,
#'     SOIL_obs = soil,
#'     WTH_obs = weather
#'   )
#'
#' obs_data = list(agroclimR_list)
#'
#' # Extract phenological data for the "oryza" model
#' phenological_data <- extract_obs_var(obs_data, "phen", model = "oryza")
#' phenological_data
#'
#' # Extract yield data for the "oryza" model
#' yield_data <- extract_obs_var(obs_data, "yield", model = "oryza")
#' yield_data
#'
#' @export
# @seealso [?agroclimR] for more information on the agroclimR package and its data structure.

extract_obs_var <- function(obs_data, variable, model = "oryza") {

  # vars select sheet names required
  vars <- switch(variable,
                 dry_matter = "PLANT_obs",
                 lai = "PLANT_obs",
                 yield = "YIELD_obs",
                 phen = "PHEN_obs")

  date_to_dae <- function(data) {

    edate <- data %>% filter(var == "EDAT") %>% pull(value)

    data %>% dplyr::mutate(value = as.numeric(value - edate)) %>%
      dplyr::filter(var != "PDAT", var != "EDAT")

  }

  #   set <- exp_set %>%
  #       str_sub(1,-5) %>% enframe(name = NULL, value = "exp_file") %>%
  #       separate(exp_file, c("LOC_ID", "CULTIVAR","PROJECT", "TR_N"), sep = "_", remove = F) %>%
  #       mutate(ID = paste0(LOC_ID, TR_N, PROJECT))

  remove_unders <- function(var){str_replace_all(var, "_", "")}

  set <- obs_data %>%
    purrr::map(~ .x[["AGRO_man"]]) %>% dplyr::bind_rows() %>%
    dplyr::mutate_at(.vars = vars(LOC_ID, CULTIVAR, PROJECT, TR_N), .funs = remove_unders) %>%
    dplyr::mutate(exp_file = paste(LOC_ID, CULTIVAR, PROJECT, TR_N, sep = "_")) %>%
    dplyr::select(c(ID,	exp_file, LOC_ID,	PROJECT,	CULTIVAR,	TR_N))





  obs_data2 <- obs_data %>%
    purrr::map( ~ .x[[vars]]) %>%
    dplyr::bind_rows() %>%
    dplyr::select(-LOC_ID, -CULTIVAR) %>%
    nest(data = -c(ID)) %>% right_join(set, by= "ID") %>% tidyr::unnest(data) %>%
    select(-c(LOC_ID, CULTIVAR, PROJECT, TR_N))



   ## Revisa columnas plant_gro -- INPUT data cambios
#  for(y in 1:length(obs_data2$data)){
#
#    obs_data2$data[[y]]$PLANT_obs = obs_data2$data[[y]]$PLANT_obs %>%
#      set_names(colnames(obs_data2$data[[y]]$PLANT_obs) %>%
#                  str_replace_all(pattern = "_S_S", replacement = "delete") %>%
#                  str_replace_all(pattern = "_SD", replacement = "_SE")) %>%
#      dplyr::select(-contains("delete"))}



  op <- switch(variable,
               dry_matter = obs_data2 %>%
                 dplyr::mutate(SAMPLING_DATE =  as.Date(SAMPLING_DATE)) %>%
                 rename(date = SAMPLING_DATE) %>%
                 select(ID:WAGT_SE, exp_file, -contains("LAI")) %>%
                 gather(var, value, -c(ID, exp_file, date)) %>%
                 separate(var, c("var", "metric"), sep = "_") %>%
                 spread(metric, value) %>%
                 rename(value = OBS, se = SE) %>%
                 dplyr::select(exp_file, date, var, value, se),
               lai = obs_data2 %>%
                 dplyr::mutate(SAMPLING_DATE =  as.Date(SAMPLING_DATE)) %>%
                 rename(date=SAMPLING_DATE) %>%
                 select(exp_file, date, contains("LAI")) %>%
                 dplyr::mutate(var = "LAI") %>%
                 rename(value=LAI_OBS, se=LAI_SE) %>%
                 dplyr::select(exp_file, date, var, value, se),
               yield = obs_data2 %>%
                 dplyr::select(exp_file, YIELD_AVG, YIELD_MIN, YIELD_MAX) %>%
                 rename(value = YIELD_AVG, ymin = YIELD_MIN, ymax = YIELD_MAX) %>%
                 dplyr::mutate(var = "YIELD", diff_min = value - ymin,
                        diff_max = ymax - value,
                        se = (diff_max+diff_min)/2) %>%
                 dplyr::select(exp_file, var, value, se),
               phen = obs_data2 %>%
                 dplyr::select(-ID) %>%
                 gather(var, value, -exp_file) %>%
                 dplyr::mutate(value = as.Date(value)) %>%
                 nest(data = -exp_file) %>%
                 dplyr::mutate(phen_dae = purrr::map(data, ~date_to_dae(.x))) %>%
                 tidyr::unnest(phen_dae) %>%
                 dplyr::mutate(value = if_else(var == "MDAT", value - 7 , value)))



  if(all(variable == "lai", model == "aquacrop")){

    phen_data <- obs_data %>%
      purrr::map(., ~.[["PHEN_obs"]]) %>%
      dplyr::bind_rows() %>% left_join(set, by = join_by(ID, LOC_ID, CULTIVAR)) %>%
      dplyr::select(exp_file, IDAT, FDAT) %>%
      dplyr::mutate(across(contains("DAT"), as.Date))



    # canopy cover data.frame
    # Convert CC = 1 - exp(-k*LAI))

    op <- op %>% na.omit() %>% nest(data = -exp_file) %>% left_join(phen_data, by = join_by(exp_file)) %>%
      dplyr::mutate(
        data = purrr::pmap(list(data, IDAT, FDAT), function(a,b,c){
          a %>%
            dplyr::mutate(
              k = case_when(
                date <= b ~ 0.4,
                date >= c ~ 0.6,
                TRUE ~ 0.5),
              canopy = (1 - exp(-k*value))*100,
              se = canopy*(se/value))})) %>% tidyr::unnest(data) %>%
      dplyr::select(exp_file, date, var, value = canopy, se)

  }




  return(op)


}



