crop_name_setup <-
function(id_name, crop){
  
  base_tb <- tibble(
    crop_name = c("rice", "maize", "barley", "sorghum", "wheat", "bean", "fababean", "teff"),
    CR = c("RI", "MZ", "BA", "SG", "WH", "BN", "FB",  "TF"),
    model = c(paste0(c("RI", "MZ", "BA", "SG", "WH"), "CER"), rep("CRGRO", 2), "TFAPS"))
  
  cul <- base_tb %>% 
    dplyr::filter(crop_name %in% all_of(crop)) %>%
    mutate(crop_name =  toupper(crop_name),
           ext = paste0(id_name, ".", CR, "X"))
  
  return(cul)
  
  
}
