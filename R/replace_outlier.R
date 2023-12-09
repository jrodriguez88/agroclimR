replace_outlier <-
function(data, fill = "na"){
    
    fill <- switch(fill,
                   na = NA_real_,
                   median = median(data, na.rm = T),
                   mean = mean(data, na.rm = T))
    
    data[data %in% boxplot.stats(data)$out] = fill
    
    return(data)
    
}
