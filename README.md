
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
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jrodriguez88/agroclimR")
```

## Example

This is a basic example which shows you how to create a crop model
weather file with agroclimR:

``` r
library(agroclimR)

## basic example code

 write_wth_oryza(
   path = ".", id_name = "TEST", wth_data = weather,
   lat = 3.8, lon = -76.5, elev = 650)
#> Early morning vapor pressure (VP; kPa) derived from relative humidity data
#> Wind Speed is not Available - Set as NA: -99
#> [1] TRUE
 
 list.files(pattern = "TEST")
#> [1] "TEST1.013" "TEST1.014" "TEST1.015" "TEST1.016"
```