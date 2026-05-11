# Write AquaCrop weather files

Formats daily weather data as the set of AquaCrop climate input files:
`.CLI`, `.Tnx`, `.PLU`, and `.ETo`. Reference evapotranspiration is
computed when an `eto` column is not supplied.

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

  Character. Directory where the AquaCrop weather files will be written.

- id_name:

  Character. Site or station identifier used as the base output file
  name, without extension.

- wth_data:

  Data frame with daily weather data. Required columns are `date`
  (`Date`), `tmax`, `tmin`, and `rain`. If `eto` is absent, ETo is
  calculated with
  [`ETo_cal()`](https://jrodriguez88.github.io/agroclimR/reference/ETo_cal.md).

- lat, lon:

  Numeric. Latitude and longitude in decimal degrees.

- elev:

  Numeric. Elevation in meters above sea level.

- co2_file:

  Character. Name of the AquaCrop CO2 file referenced in the generated
  `.CLI` file. Defaults to `"MaunaLoa.CO2"`.

## Value

Character vector with the paths of the AquaCrop weather files created.

## Examples

``` r
# Write AquaCrop weather file
wth_files_created <- write_wth_aquacrop(
  path = tempdir(), id_name = "wth_aquacrop", wth_data = weather,
  lat = 3.8, lon = -76.5, elev = 650)
#> Reference evapotranspiration (ETo) Method: FAO Penman-Monteith equation
#> AquaCrop weather files created in  /tmp/RtmpJqE3M0  : 
#>  /tmp/RtmpJqE3M0/wth_aquacrop.CLI ,/tmp/RtmpJqE3M0/wth_aquacrop.Tnx ,/tmp/RtmpJqE3M0/wth_aquacrop.PLU ,/tmp/RtmpJqE3M0/wth_aquacrop.ETo

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
