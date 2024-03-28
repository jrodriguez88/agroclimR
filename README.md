
<!-- README.md is generated from README.Rmd. Please edit that file -->

# agroclimR: Streamlining Agroclimatic Data for Crop Modeling

<!-- badges: start -->
<!-- badges: end -->

## Overview

`agroclimR` is an R package developed to facilitate the management and
transformation of agroclimatic data into crop model-ready formats. It
supports major modeling systems such as [DSSAT v
4.8](https://dssat.net/), [ORYZA v3.0](https://www.irri.org/oryza), and
[Aquacrop v6.0](https://www.fao.org/aquacrop/). This toolkit serves as a
resource for researchers and students specializing in agricultural
sciences.

## Features

- **Efficient Data Management:** Provides a relational database
  structure that simplifies data handling and seamlessly integrates with
  the agroclimR framework. Learn more about the data structure in
  [data-format](https://jrodriguez88.github.io/agroclimR/articles/Estructura_de_Datos_para_Modelacion_de_Cultivos_con_agroclimR.html)
- **Easy Data Transformation to Crop Model-Ready Formats:** Utilizes R
  to convert various model format files efficiently.
- **Tidyverse Integration:** Employs tidyverse functions for smooth data
  manipulation and offers insightful data visualization capabilities
  using ggplot2.

## Installation

You can install the development version of agroclimR from
[GitHub](https://github.com/jrodriguez88/agroclimR) with:

``` r
# install.packages("devtools")
devtools::install_github("jrodriguez88/agroclimR")
```

## Example: Creating Weather and Soil Model Format Files

### Weather Model Files

Here’s a basic example demonstrating how to create a crop model weather
file using agroclimR:

``` r
library(agroclimR)

path = "."
wth_data = weather
lat = 3.8
lon = -76.5
elev = 650

## basic example code to create ORYZA weather file
wth_files_oryza <- write_wth_oryza(path, "wth_oryza", wth_data, lat, lon, elev, tag = TRUE)
#> Early morning vapor pressure (VP; kPa) derived from relative humidity data
#> Oryza Weather Files created in  .  : 
#>  ./wth_oryza1.013 ,./wth_oryza1.014 ,./wth_oryza1.015 ,./wth_oryza1.016
readLines(wth_files_oryza[1], n = 24) |> writeLines()
#> *-----------------------------------------------------------
#> *  Station Name: wth_oryza
#> *  ORYZA Weather file - by agroclimR
#> *  Longitude: -76.5 -- Latitude: 3.8 -- Elevation: 650m
#> *-----------------------------------------------------------
#> *  Date: 2013-01-01 : 2016-12-31
#> *
#> *  Column    Daily Value
#> *     1      Station number
#> *     2      Year
#> *     3      Day
#> *     4      irradiance         KJ m-2 d-1
#> *     5      min temperature            oC
#> *     6      max temperature            oC
#> *     7      vapor pressure            kPa
#> *     8      mean wind speed         m s-1
#> *     9      precipitation          mm d-1
#> *-----------------------------------------------------------
#> -76.5,3.8,650,0,0
#> 1,2013,1,20889.29,25.73,36.03,3.07,-99,0
#> 1,2013,2,21632.15,25.08,35.68,3.31,-99,0.2
#> 1,2013,3,23106.91,23.99,38.49,3.01,-99,0
#> 1,2013,4,21408.98,22.91,35.71,2.98,-99,0
#> 1,2013,5,22908.94,23.81,38.81,3.43,-99,0
file.remove(wth_files_oryza)
#> [1] TRUE TRUE TRUE TRUE

## basic example code to create DSSAT weather file
wth_files_dssat <- write_wth_dssat(path, "wth_dssat", wth_data, lat, lon, elev)
#> DSSAT Weather Files created in  .  : 
#>  ./wth_dssat.WTH
readLines(wth_files_dssat[1], n = 10) |> writeLines()
#> *WEATHER DATA : wth_dssat DSSAT Weather file - by agroclimR
#> 
#> @ INSI      LAT     LONG  ELEV   TAV   AMP REFHT WNDHT
#>   ACRP    3.800  -76.500   650  28.7   9.9   2.0   2.0
#> @DATE  SRAD  TMAX  TMIN  RAIN  DEWP  WIND   PAR  EVAP  RHUM
#> 13001  20.9  36.0  25.7   0.0          NA              66.3
#> 13002  21.6  35.7  25.1   0.2          NA              73.3
#> 13003  23.1  38.5  24.0   0.0          NA              61.4
#> 13004  21.4  35.7  22.9   0.0          NA              68.9
#> 13005  22.9  38.8  23.8   0.0          NA              69.5
file.remove(wth_files_dssat)
#> [1] TRUE
 
## basic example code to create AquaCrop weather file  
wth_files_aquacrop <- write_wth_aquacrop(path, "wth_aquacrop", wth_data, lat, lon, elev)
#> Reference evapotranspiration (ETo) Method: FAO Penman-Monteith equation
#> Oryza Weather Files created in  .  : 
#>  ./wth_aquacrop.CLI ,./wth_aquacrop.Tnx ,./wth_aquacrop.PLU ,./wth_aquacrop.ETo
readLines(wth_files_aquacrop[1], n = 10) |> writeLines()
#> wth_aquacrop Station, lat: 3.8 long: -76.5 - by agroclimR
#> 6.0   : AquaCrop Version (March 2017)
#> wth_aquacrop.Tnx
#> wth_aquacrop.ETo
#> wth_aquacrop.PLU
#> MaunaLoa.CO2
readLines(wth_files_aquacrop[2], n = 13) |> writeLines()
#> wth_aquacrop : daily temperature data (01 enero 2013 - 31 diciembre 2016)
#>      1  : Daily records (1=daily, 2=10-daily and 3=monthly data)
#>      1  : First day of record (1, 11 or 21 for 10-day or 1 for months)
#>      1  : First month of record
#>   2013  : First year of record (1901 if not linked to a specific year)
#> 
#>   Tmin (C)   TMax (C)
#> =======================
#>       25.7       36.0
#>       25.1       35.7
#>       24.0       38.5
#>       22.9       35.7
#>       23.8       38.8
file.remove(wth_files_aquacrop)
#> [1] TRUE TRUE TRUE TRUE
```

### Soil Model Files

This is a basic example which shows you how to create a crop model Soil
file with agroclimR:

``` r


soil_sample = dplyr::group_by(soil, NL) |> 
  dplyr::sample_n(1) |> dplyr::ungroup()


# basic example code to create ORYZA soil file
soil_files_oryza <- write_soil_oryza(id_name = "soil_oryza", soil_data = soil_sample)
#> Oryza Experimental Files created in  .  : 
#>  ./soil_oryza.sol
readLines(soil_files_oryza[1], n = 27) |> writeLines()
#> **********************************************************************
#> * Template soil data file for PADDY soil water balance model.        *
#> **********************************************************************
#> * Soil        : soil_oryza - texture classes:c("Lo", "Lo", "SaLo")
#> * File name        : soil_oryza.sol
#> * Sampling date      : 2015-12-23
#> * Additional info  : Create with agroclimR
#> *--------------------------------------------------------------------*
#> 
#> SCODE = 'PADDY'
#> 
#> *---------------------------------------------------------------*
#> * 1. Various soil and management parameters
#> *---------------------------------------------------------------*
#> WL0MX = 100.   ! Bund height (mm)
#> NL = 3        ! Number of soil layers (maximum is 10) (-)
#> TKL = 0.20, 0.20, 0.20   ! Thickness of each soil layer (m)
#> ZRTMS = 0.5   ! Maximum rooting depth in the soil (m)
#> 
#> *---------------------------------------------------------------*
#> * 2. Puddling switch: 1=PUDDLED or 0=NON PUDDLED
#> *---------------------------------------------------------------*
#> SWITPD = 0  !Non puddled
#> NLPUD = 1
#> WCSTRP = 0.38, 0.38, 0.43
#> PFCR = 6.0
#> DPLOWPAN = 0.6
file.remove(soil_files_oryza)
#> [1] TRUE

# basic example code to create DSSAT soil file
soil_files_dssat <- write_soil_dssat(path = ".", "soil_dssat", soil_sample)
#> Minimun data are available
#> DSSAT soil Files created in  .  : 
#>  .soil_dssat.SOL
readLines(soil_files_dssat[1], n = 15) |> writeLines()
#> *SOILS: AgroclimR DSSAT Soil Input File - by agroclimR
#> 
#> *soil_dssat000001  AgroclimRV1   LO      60  AgroClimR soil_dssat
#> @SITE        COUNTRY          LAT     LONG SCS FAMILY
#>  soil_dssat  AgroclimR         -99    -99 USDA Texture: Lo
#> @ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
#>    -99  0.13     6   0.6    75  1.00  1.00 IB001 IB001 IB001
#> @  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
#>     20   -99 0.318 0.365 0.379 0.810 21.86  1.56  1.32  18.3  39.1   1.5   0.6   6.3   -99   9.6   -99
#>     40   -99 0.177 0.276 0.377 0.640 30.67  1.76  0.24  13.4  39.0   1.5   0.3   6.4   -99   6.7   -99
#>     60   -99 0.051 0.126 0.429 0.490 77.61  1.46  0.14  14.4   7.8   1.5   0.3   6.5   -99   7.9   -99
file.remove(soil_files_dssat)
#> [1] TRUE

# basic example code to create Aquacrop soil file
soil_files_aquacrop <- write_soil_aquacrop(path = ".", "soil_dssat", soil_sample)
#> AquaCrop soil Files created in  .  : 
#>  ./soil_dssat.SOL
readLines(soil_files_aquacrop[1], n = 15) |> writeLines()
#> soil_dssat AquaCrop soil file - by agroclimR
#>         6.1                 : AquaCrop Version (May 2018)
#>        65                   : CN (Curve Number)
#>        8                   : Readily evaporable water from top layer (mm)
#>         3                   : number of soil horizons
#>        -9                   : variable no longer applicable
#>   Thickness  Sat   FC    WP     Ksat   Penetrability  Gravel  CRa       CRb           description
#>   ---(m)-   ----(vol %)-----  (mm/day)      (%)        (%)    -----------------------------------------
#>     0.20    37.9  36.5  31.8   524.7        100         0     -0.451380  0.860353               Lo
#>     0.20    37.7  27.6  17.7   736.2        100         0     -0.432343  1.022189               Lo
#>     0.20    42.9  12.6   5.1  1862.6        100         0     -0.329826  0.325586             SaLo
file.remove(soil_files_aquacrop)
#> [1] TRUE
```

## Connect

For inquiries or feedback, contact Jeferson Rodriguez-Espinoza at
<jrodriguezespinoza@outlook.com>.

## Explore More

- [Documentation](https://jrodriguez88.github.io/agroclimR/)
- [DSSAT](https://dssat.net/)
- [ORYZA](https://www.irri.org/oryza)
- [Aquacrop](https://www.fao.org/aquacrop/)
