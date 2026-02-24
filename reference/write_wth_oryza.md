# Write ORYZA v3 Weather File (.WTH - .CLI)

Function compute weather information to ORYZA weather file.

## Usage

``` r
write_wth_oryza(
  path = ".",
  id_name,
  wth_data,
  lat,
  lon,
  elev,
  stn = 1,
  multiyear = F,
  tag = F
)
```

## Arguments

- path:

  A string indicating path folder or working directory

- id_name:

  A String 4 letters string of locality name. "AIHU" = Aipe, Huila

- wth_data:

  A Data frame Weather data. minimum = date, tmax, tmin, rain

- lat:

  Numeric. Latitude (decimal degrees)

- lon:

  Numeric. Longitude (decimal degrees)

- elev:

  Numeric. Elevation (meters above sea level)

- stn:

  Integer. Station number

- multiyear:

  A Logical. TRUE = ".cli" multiyear format or FALSE = yearly format
  (ie. 1998 = \*.998)

- tag:

  A Logical. TRUE = write information for each file

## Value

This function returns a vector of model files created in path folder.

## Examples

``` r
# Write wth file
wth_files_created <- write_wth_oryza(
  path = ".", id_name = "TEST", wth_data = weather,
  lat = 3.8, lon = -76.5, elev = 650)
#> Early morning vapor pressure (VP; kPa) derived from relative humidity data
#> Oryza Weather Files created in  .  : 
#>  ./TEST1.013 ,./TEST1.014 ,./TEST1.015 ,./TEST1.016

readLines(wth_files_created[1], n = 15) |> writeLines()
#> -76.5,3.8,650,0,0
#> 1,2013,1,20889.29,25.73,36.03,3.07,-99,0
#> 1,2013,2,21632.15,25.08,35.68,3.31,-99,0.2
#> 1,2013,3,23106.91,23.99,38.49,3.01,-99,0
#> 1,2013,4,21408.98,22.91,35.71,2.98,-99,0
#> 1,2013,5,22908.94,23.81,38.81,3.43,-99,0
#> 1,2013,6,24159.16,22.71,37.21,3,-99,1.2
#> 1,2013,7,22949.84,23.74,38.44,2.93,-99,0
#> 1,2013,8,22105.74,22.48,34.68,3.06,-99,0
#> 1,2013,9,22689.82,23.95,38.95,3.58,-99,0
#> 1,2013,10,22777.98,24.23,38.93,3.61,-99,0
#> 1,2013,11,21718.21,24.65,36.95,3.21,-99,0
#> 1,2013,12,20806.88,26.84,36.24,3.27,-99,0
#> 1,2013,13,20765.46,26.19,35.69,3.34,-99,0
#> 1,2013,14,17485.02,25.72,34.62,3.13,-99,0
file.remove(wth_files_created)
#> [1] TRUE TRUE TRUE TRUE

wth_files_created2 <- write_wth_oryza(
  path = ".", id_name = "TEST2", wth_data = weather,
  lat = 3.8, lon = -76.5, elev = 650, multiyear = TRUE, tag = TRUE)
#> Early morning vapor pressure (VP; kPa) derived from relative humidity data
#> Oryza Weather Files created in  .  : 
#>  ./TEST21.cli

readLines(wth_files_created2[1], n = 25) |> writeLines()
#> *-----------------------------------------------------------
#> *  Station Name: TEST2
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
#> 1,2013,6,24159.16,22.71,37.21,3,-99,1.2
file.remove(wth_files_created2)
#> [1] TRUE
```
