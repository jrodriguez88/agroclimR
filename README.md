
<!-- README.md is generated from README.Rmd. Please edit that file -->

# agroclimR: A R-Toolkit for Agroclimatic Data Management and Crop Model File Creation

<!-- badges: start -->
<!-- badges: end -->

agroclimR is a package designed to streamline the management,
manipulation, and generation of process-based model files for systems
like [DSSAT v 4.8](https://dssat.net/), [ORYZA
v3.0](https://www.irri.org/oryza), and [Aquacrop
v6.0](https://www.fao.org/aquacrop/).

## Installation

You can install the development version of agroclimR from
[GitHub](https://github.com/jrodriguez88/agroclimR) with:

``` r
# install.packages("devtools")
devtools::install_github("jrodriguez88/agroclimR")
```

## Example

This is a basic example which shows you how to create a crop model
weather file with agroclimR:

``` r
library(agroclimR)

path = "."
id_name = "TEST"
wth_data = weather
lat = 3.8
lon = -76.5
elev = 650

## basic example code to create ORYZA weather file

write_wth_oryza(path, id_name, wth_data, lat, lon, elev)
#> Early morning vapor pressure (VP; kPa) derived from relative humidity data
#> Wind Speed is not Available - Set as NA: -99
#> [1] TRUE

## basic example code to create DSSAT weather file
write_wth_dssat(path, id_name, wth_data, lat, lon, elev)
#> [1] TRUE
 
## basic example code to create AquaCrop weather file  
write_wth_aquacrop(path, id_name, wth_data, lat, lon, elev)
#> Wind Speed = 2m/s was used
#> Reference evapotranspiration (ETo) Method: FAO Penman-Monteith equation  +  Assumption: Wind Speed mean = 2m/s
#> [1] TRUE
```

This is a basic example which shows you how to create a crop model Soil
file with agroclimR:

``` r

library(agroclimR)
library(magrittr)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

soil_sample = group_by(soil, NL) %>% sample_n(1)


## basic example code to create ORYZA soil file

write_soil_oryza(path = ".", id_name, soil_sample, ZRTMS = 0.50, WL0I = 0, WCLI = 'FC' , RIWCLI = 'NO', SATAV = 20)
#> [1] TRUE

## basic example code to create DSSAT soil file

#write_soil_dssat(path = ".", id_name, soil_sample)
 

 
 
```
