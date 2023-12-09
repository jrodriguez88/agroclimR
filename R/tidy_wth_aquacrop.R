tidy_wth_aquacrop <-
function(wth_data){
    
    var_names <- colnames(wth_data)
    stopifnot(class(wth_data$date)=="Date" & all(c("tmax", "tmin", "rain") %in%  var_names))
    
    impute_mean_wth(wth_data) 

}
