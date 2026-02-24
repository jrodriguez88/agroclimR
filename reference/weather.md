# Observed Weather Data

A dataset capturing daily weather observations. It includes data on
temperature, precipitation, irradiance, relative humidity, and wind
speed, collected from the FEDEARROZ network of meteorological stations.

## Usage

``` r
weather
```

## Format

A `data.frame` with 1461 rows and 7 columns:

- LOC_ID:

  Locality ID - (`character`). Identifies the location of the trial.
  Example: `"LOC1"`.

- WS_ID:

  Weather Station ID - (`integer`). Identifies the number of weather
  station. Example: `"1"`.

- DATE:

  Date of observation - (`Date`). Captures daily dates of weather
  observations.

- TMAX:

  Maximum temperature observed - (`numeric`). Values are in degrees
  Celsius (`oC`).

- TMIN:

  Minimum temperature observed - (`numeric`). Values are in degrees
  Celsius (`oC`).

- RAIN:

  Precipitation amount - (`numeric`). Values are in millimeters per day
  (`mm d-1`).

- SRAD:

  Solar irradiance - (`numeric`). Values are in Megajoules per square
  meter per day (`MJ m-2 d-1`).

- RHUM:

  Relative humidity - (`numeric`). Percentage values (%).

- WSPD:

  Wind speed - (`numeric`). Values are in meters per second (`m/s`).

## Source

`Rodriguez-Espinoza J, (2024)` and `CIAT-MADR-FEDEARROZ, (2016)`

## Details

Observed Weather Data

This dataset encompasses daily observed weather data collected from the
FEDEARROZ network of Davis Vantage Pro2 meteorological stations. It is
designed to provide researchers and practitioners with detailed
environmental measurements critical for agricultural research, weather
analysis, and climatology studies.

## Examples

``` r
# Assuming `weather` is your dataset name
head(weather)
#> # A tibble: 6 × 9
#>   LOC_ID WS_ID DATE        TMAX  TMIN  RAIN  SRAD  RHUM WSPD 
#>   <chr>  <dbl> <date>     <dbl> <dbl> <dbl> <dbl> <dbl> <lgl>
#> 1 SDTO       1 2013-01-01  36.0  25.7   0    20.9  66.3 NA   
#> 2 SDTO       1 2013-01-02  35.7  25.1   0.2  21.6  73.3 NA   
#> 3 SDTO       1 2013-01-03  38.5  24.0   0    23.1  61.4 NA   
#> 4 SDTO       1 2013-01-04  35.7  22.9   0    21.4  68.9 NA   
#> 5 SDTO       1 2013-01-05  38.8  23.8   0    22.9  69.5 NA   
#> 6 SDTO       1 2013-01-06  37.2  22.7   1.2  24.2  65.9 NA   
```
