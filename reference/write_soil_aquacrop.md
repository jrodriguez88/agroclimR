# Write an AquaCrop soil file

Formats a soil profile as an AquaCrop `.SOL` file. The writer derives
Curve Number and readily evaporable water from the supplied hydraulic
properties and writes one soil profile per file.

## Usage

``` r
write_soil_aquacrop(path = ".", id_name, soil_data, model_version = 6.1)
```

## Arguments

- path:

  Character. Directory where the `.SOL` file will be written.

- id_name:

  Character. Soil profile identifier used as the output file name,
  without extension.

- soil_data:

  Data frame with one row per soil layer. Expected columns include
  `LOC_ID`, `DEPTH`, `SBDM`, `SOC`, `SSKS`, `WCST`, `WCFC`, `WCWP`, and
  `STC`; see the example data set
  [soil](https://jrodriguez88.github.io/agroclimR/reference/soil.md).

- model_version:

  Numeric or character. AquaCrop version label written in the file
  header.

## Value

Character vector with the path of the AquaCrop soil file created.

## Examples

``` r
# Write Aquacrop v6 Soil file
soil_sample = dplyr::group_by(soil, NL) |> dplyr::sample_n(1)
soil_files_created <- write_soil_aquacrop(
path = tempdir(),
id_name = "soil_aquacrop",
soil_data = soil_sample)
#> AquaCrop soil Files created in  /tmp/RtmpJqE3M0  : 
#>  /tmp/RtmpJqE3M0/soil_aquacrop.SOL

readLines(soil_files_created[1], n = 15) |> writeLines()
#> soil_aquacrop AquaCrop soil file - by agroclimR
#>         6.1                 : AquaCrop Version (May 2018)
#>        65                   : CN (Curve Number)
#>        4                   : Readily evaporable water from top layer (mm)
#>         3                   : number of soil horizons
#>        -9                   : variable no longer applicable
#>   Thickness  Sat   FC    WP     Ksat   Penetrability  Gravel  CRa       CRb           description
#>   ---(m)-   ----(vol %)-----  (mm/day)      (%)        (%)    -----------------------------------------
#>     0.20    46.4  15.0   8.0  1146.0        100         0     -0.395457  1.233652               Lo
#>     0.20    43.9  28.4  21.5   526.5        100         0     -0.451219  0.861978               Lo
#>     0.20    42.9  12.6   5.1  1862.6        100         0     -0.329826  0.325586             SaLo
file.remove(soil_files_created)
#> [1] TRUE
```
