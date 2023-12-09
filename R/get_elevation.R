get_elevation <-
function(lat, lon, dataset = "aster30m"){
  
  elev_raw <- fromJSON(
    paste0("https://api.opentopodata.org/v1/", dataset, "?locations=", lat, ",", lon)
  )
  
  return(elev_raw$results$elevation)
  
  
}
