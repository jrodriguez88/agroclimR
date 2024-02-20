#' Import Experimental data from INPUT_data workbook
#'
#' Function to extract experimental data from INPUT_data workbook
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
#' # Import Experimental data from INPUT_data workbook
#' path = "data/"
#' files <- list.files(path = "data/", pattern = "xls")
#' import_exp_data <- function(path = ., files, model = "oryza")
#'
## Update the details for the return value
#' @return This function returns a \code{logical} if files created in path folder.
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
    mutate(input_data =  map(file, ~read_INPUT_data(paste0(path, .))))

  ## Extrae datos de suelo
  soil <- data %>% mutate(soil_data = map(input_data, ~.x$SOIL_obs))  %>% dplyr::select(-input_data)  %>%
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
    left_join(

      data$input_data %>% map("AGRO_man") %>% bind_rows() %>%
        dplyr::select(LOC_ID, contains("LAT"), contains("LONG"), starts_with("A")) %>%
        distinct() %>% set_names(c("site", "lat", "lon", "elev")) %>%
        group_by(site) %>% slice(1) %>%
        ungroup(),
      by = "site") #%>% rename(stn = ws_id) #%>%
  #  mutate(path = "data/OUTPUTS/WTH/") %>%
  #  select(path, id_name, wth_data, lat, lon, elev, stn)

  # Extract and plot - Growth and Development observed data by component

  # Crop Phenology
  phen <- extract_obs_var(data$input_data, "phen")
  #  plot_phen_obs(phen) %>% ggplotly()

  #Leaf Area Index
  lai <- extract_obs_var(data$input_data, "lai")
  #  plot_lai_obs(lai) %>% ggplotly()

  #Shoot Dry Matter
  dry_matter <- extract_obs_var(data$input_data, "dry_matter")
  #  plot_drymatter_obs(dry_matter) %>% ggplotly()

  #Yield dry matter
  yield <- extract_obs_var(data$input_data, "yield")
  #  plot_yield_obs(yield) %>% ggplotly()



  # crea lista de datos extraidos
  data_list <- list(

    data =  data %>% left_join(soil, by = "site") %>% left_join(wth, by = "site"),

    phen =  phen, lai = lai, dry_matter = dry_matter, yield = yield

  )




  return(data_list)


}


# helpers -----------------------------------------------------------------


# 'read_INPUT_data' function to read xlsx files ---->  c(LOC_ID, cultivar), base_raw_data
read_INPUT_data <- function(file) {

  sheets <- readxl::excel_sheets(file)
  x <-    lapply(sheets, function(X) readxl::read_excel(file, sheet = X))
  names(x) <- sheets

  return(x)

}



