# Write a DSSAT v4.8 weather file

Formats daily weather data as a DSSAT weather (`.WTH`) file. The input
data are standardized internally and optional wind speed and relative
humidity columns are written when available.

## Usage

``` r
write_wth_dssat(path = ".", id_name, wth_data, lat, lon, elev, ref_ht = 2)
```

## Arguments

- path:

  Character. Directory where the `.WTH` file will be written.

- id_name:

  Character. Station or site identifier used as the output file name,
  without extension.

- wth_data:

  Data frame with daily weather data. Required columns are `date`
  (`Date`), `tmax`, `tmin`, `rain`, and `srad`. Optional columns are
  `wspd` (m s-1) and `rhum` (%).

- lat, lon:

  Numeric. Latitude and longitude in decimal degrees.

- elev:

  Numeric. Elevation in meters above sea level.

- ref_ht:

  Numeric. Measurement height, in meters, used for DSSAT `REFHT` and
  `WNDHT`.

## Value

Character vector with the path of the DSSAT weather file created.

## References

- DSSAT Weather module: <https://dssat.net/weather-module/>

- Brakensiek et al. (1984) for SSKS_Brakensiek

## Examples

``` r
# Write DSSAT weather file
wth_files_created <- write_wth_dssat(
  path = tempdir(), id_name = "TEST", wth_data = weather,
  lat = 3.91, lon = -75.0, elev = 450)
#> DSSAT Weather Files created in  /tmp/RtmpEvHEAr  : 
#>  /tmp/RtmpEvHEAr/TEST.WTH

readLines(wth_files_created[1], n = 15) |> writeLines()
#> *WEATHER DATA : TEST DSSAT Weather file - by agroclimR
#> 
#> @ INSI      LAT     LONG  ELEV   TAV   AMP REFHT WNDHT
#>   ACRP    3.910  -75.000   450  28.7   9.9   2.0   2.0
#> @DATE  SRAD  TMAX  TMIN  RAIN  DEWP  WIND   PAR  EVAP  RHUM
#> 13001  20.9  36.0  25.7   0.0          NA              66.3
#> 13002  21.6  35.7  25.1   0.2          NA              73.3
#> 13003  23.1  38.5  24.0   0.0          NA              61.4
#> 13004  21.4  35.7  22.9   0.0          NA              68.9
#> 13005  22.9  38.8  23.8   0.0          NA              69.5
#> 13006  24.2  37.2  22.7   1.2          NA              65.9
#> 13007  22.9  38.4  23.7   0.0          NA              60.3
#> 13008  22.1  34.7  22.5   0.0          NA              74.3
#> 13009  22.7  38.9  23.9   0.0          NA              72.1
#> 13010  22.8  38.9  24.2   0.0          NA              72.2
file.remove(wth_files_created)
#> [1] TRUE
```
