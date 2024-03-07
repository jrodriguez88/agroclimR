
<!-- README.md is generated from README.Rmd. Please edit that file -->

# agroclimR: Streamlining Agroclimatic Data for Crop Modeling

<!-- badges: start -->
<!-- badges: end -->

## Overview

`agroclimR` is an R package developed from years of agricultural
research and application, designed to simplify the management and
transformation of agroclimatic data into crop model-ready formats. It
supports major modeling systems like [DSSAT v 4.8](https://dssat.net/),
[ORYZA v3.0](https://www.irri.org/oryza), and [Aquacrop
v6.0](https://www.fao.org/aquacrop/). An R-toolkit for researchers and
students in agricultural sciences.

## Features

- **Efficient Data Management:** Offers a relational database structure
  for easy data format conversion. See
- **Tidyverse Integration:** Utilizes tidyverse functions for seamless
  data manipulation.
- **Visualization:** Leverages ggplot2 for insightful data
  visualization.

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
#> Warning: replacing previous import 'magrittr::set_names' by 'purrr::set_names'
#> when loading 'agroclimR'
#> Warning: replacing previous import 'magrittr::extract' by 'tidyr::extract' when
#> loading 'agroclimR'

path = "."
wth_data = weather
lat = 3.8
lon = -76.5
elev = 650

## basic example code to create ORYZA weather file

write_wth_oryza(path, "wth_oryza", wth_data, lat, lon, elev)
#> Early morning vapor pressure (VP; kPa) derived from relative humidity data
#> [1] TRUE

## basic example code to create DSSAT weather file
write_wth_dssat(path, "wth_dssat", wth_data, lat, lon, elev)
#> [1] TRUE
 
## basic example code to create AquaCrop weather file  
write_wth_aquacrop(path, "wth_aquacrop", wth_data, lat, lon, elev)
#> Reference evapotranspiration (ETo) Method: FAO Penman-Monteith equation
#> [1] TRUE
```

This is a basic example which shows you how to create a crop model Soil
file with agroclimR:

``` r

#library(agroclimR)
#library(magrittr)
#library(dplyr)

soil_sample = dplyr::group_by(soil, NL) |> 
  dplyr::sample_n(1) |> dplyr::ungroup()


## basic example code to create ORYZA soil file

write_soil_oryza(path = ".", "soil_oryza", soil_sample, ZRTMS = 0.50, WL0I = 0, WCLI = 'FC' , RIWCLI = 'NO', SATAV = 20)
#> [1] TRUE

## basic example code to create DSSAT soil file

write_soil_dssat(path = ".", "soil_dssat", soil_sample)
#> Minimun data are available
#> WCFC was estimated using Saxton-PTF
#> Porosity was estimated using 2.65g/cm3 as particle density
#> WCFC was estimated using Saxton-PTF
#> Porosity was estimated using 2.65g/cm3 as particle density
#> WCFC was estimated using Saxton-PTF
#> Porosity was estimated using 2.65g/cm3 as particle density
```

## Connect

For inquiries or feedback, contact Jeferson Rodriguez-Espinoza at
<jrodriguezespinoza@outlook.com>.

## Explore More

- [Documentation](https://jrodriguez88.github.io/agroclimR/)
- [DSSAT](https://dssat.net/)
- [ORYZA](https://www.irri.org/oryza)
- [Aquacrop](https://www.fao.org/aquacrop/)
