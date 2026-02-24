# Compute Reference evapotranspiration (ETo)

Function to compute Reference evapotranspiration (ETo) estimates using
different methods.

## Usage

``` r
ETo_cal(wth_data, lat, elev, ref_ht = 2, kRs = 0.175, ws_mean = 2)
```

## Arguments

- wth_data:

  A data frame containing weather data with columns: date, tmax, tmin,
  rain, rhum, srad, wspd (optional)

- lat:

  Numeric. Latitude (decimal degrees)

- elev:

  Numeric. Elevation (meters above sea level)

- ref_ht:

  Numeric. Sensors reference height.

- kRs:

  Numeric. Hargreaves empirical coefficient, Coastal coefficient

- ws_mean:

  Numeric. Wind Speed Mean, default = 2 m/s

## Value

This function returns a numeric vector representing ET0 values (mm).

## Examples

``` r
# Compute ETo with provided weather data
ETo_cal(wth_data = weather, lat = 3.8, elev = 650) |>
head(10)
#> Reference evapotranspiration (ETo) Method: FAO Penman-Monteith equation
#>  [1] 4.085653 4.105157 4.891880 4.419465 4.991884 4.777290 4.929282 4.264894
#>  [9] 5.026964 4.995741

# Compute into dplyr::mutate

weather %>% slice(1:10) %>%
  mutate(ETo = ETo_cal(., lat = 3.8, elev = 650))
#> Error in weather %>% slice(1:10) %>% mutate(ETo = ETo_cal(., lat = 3.8,     elev = 650)): could not find function "%>%"
```
