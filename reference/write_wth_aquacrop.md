# Write AquaCrop Weather File

Function to compute weather information AquaCrop v6.0 weather file.

## Usage

``` r
write_wth_aquacrop(
  path = ".",
  id_name,
  wth_data,
  lat,
  lon,
  elev,
  co2_file = "MaunaLoa.CO2"
)
```

## Arguments

- path:

  A string indicating the path folder or working directory where the
  weather files will be saved.

- id_name:

  A 4-letter string representing the locality name abbreviation. For
  example, "AIHU" stands for Aipe, Huila.

- wth_data:

  A data frame containing weather data with at least the following
  columns: date, tmax, tmin, rain.

- lat:

  Numeric. Latitude of the location in decimal degrees.

- lon:

  Numeric. Longitude of the location in decimal degrees.

- elev:

  Numeric. Elevation of the location in meters above sea level.

- co2_file:

  A string representing the CO2 file to be used. Default is
  "MaunaLoa.CO2". CO2 files are available in the Aquacrop default
  database.

## Value

This function returns a vector of model files created in path folder.

## Examples

``` r
# Write AquaCrop weather file
wth_files_created <- write_wth_aquacrop(
  path = ".", id_name = "wth_aquacrop", wth_data = weather,
  lat = 3.8, lon = -76.5, elev = 650)
#> Reference evapotranspiration (ETo) Method: FAO Penman-Monteith equation
#> Oryza Weather Files created in  .  : 
#>  ./wth_aquacrop.CLI ,./wth_aquacrop.Tnx ,./wth_aquacrop.PLU ,./wth_aquacrop.ETo

readLines(wth_files_created[1], n = 15) |> writeLines()
#> wth_aquacrop Station, lat: 3.8 long: -76.5 - by agroclimR
#> 6.0   : AquaCrop Version (March 2017)
#> wth_aquacrop.Tnx
#> wth_aquacrop.ETo
#> wth_aquacrop.PLU
#> MaunaLoa.CO2
readLines(wth_files_created[2], n = 15) |> writeLines()
#> wth_aquacrop : daily temperature data (01 January 2013 - 31 December 2016)
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
#>       22.7       37.2
#>       23.7       38.4
file.remove(wth_files_created)
#> [1] TRUE TRUE TRUE TRUE
```
