HUH_cal <-
function(tmax, tmin, tbase = 8, topt = 30, thigh = 42.5) {
    
    tav <- (tmin + tmax)/2
    
    h <- 1:24
    
    Td <- tav + (tmax - tmin)*cos(0.2618*(h - 14))/2 
    
    huh <- Td %>% enframe(name = NULL, "td") %>%
        mutate(HUH = case_when(td <= tbase | td >= thigh ~ 0,
                               td > tbase | td <= topt ~ (td - tbase)/24,
                               td > topt | td < thigh ~ (topt-(td - topt)*(topt - tbase)/(thigh - topt))/24))
    
    sum(huh$HUH)   
    
}
