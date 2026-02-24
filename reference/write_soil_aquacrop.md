# Write Aquacrop v6.1 Soil File

Function compute Soil information to Aquacrop v6.1 soil file.

## Usage

``` r
write_soil_aquacrop(path = ".", id_name, soil_data, model_version = 6.1)
```

## Arguments

- path:

  A string indicating path folder or working directory

- id_name:

  A String. 4 letters string of locality name. (ex. "JR")

- soil_data:

  A Data frame. Soil data. see `soil`

- salb:

  Numeric. Albedo, fraction

- evapL:

  Numeric. Evaporation limit, (mm)

- slnf:

  Numeric. Mineralization factor, 0 to 1 scale.

- slpf:

  Numeric. Photosynthesis factor, 0 to 1 scale

- multi:

  Logical. Soil annual average temperature of the first layers

- max_depth:

  description

## Value

This function returns a vector of model files created in path folder.

## Examples

``` r
# Write Aquacrop v6 Soil file
soil_sample = dplyr::group_by(soil, NL) |> dplyr::sample_n(1)
soil_files_created <- write_soil_aquacrop(
path = ".",
id_name = "soil_aquacrop",
soil_data = soil_sample)
#> AquaCrop soil Files created in  .  : 
#>  ./soil_aquacrop.SOL

readLines(soil_files_created[1], n = 15) |> writeLines()
#> soil_aquacrop AquaCrop soil file - by agroclimR
#>         6.1                 : AquaCrop Version (May 2018)
#>        65                   : CN (Curve Number)
#>        6                   : Readily evaporable water from top layer (mm)
#>         3                   : number of soil horizons
#>        -9                   : variable no longer applicable
#>   Thickness  Sat   FC    WP     Ksat   Penetrability  Gravel  CRa       CRb           description
#>   ---(m)-   ----(vol %)-----  (mm/day)      (%)        (%)    -----------------------------------------
#>     0.20    45.3  17.5   7.4  2260.3        100         0     -0.333803  0.372337             SaLo
#>     0.20    43.9  28.4  21.5   526.5        100         0     -0.451219  0.861978               Lo
#>     0.20    46.3  11.2   5.2  2103.8        100         0     -0.332238  0.355002             SaLo
file.remove(soil_files_created)
#> [1] TRUE
```
