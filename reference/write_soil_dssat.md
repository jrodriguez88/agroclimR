# Write DSSAT v4.8 Soil File

Function compute Soil information to DSSAT v4.8 soil file.

## Usage

``` r
write_soil_dssat(
  path = ".",
  id_name,
  soil_data,
  salb = 0.13,
  evapL = 6,
  sldr = 0.6,
  slnf = 1,
  slpf = 1,
  multi = FALSE
)
```

## Arguments

- path:

  A string indicating path folder or working directory

- id_name:

  A String. 4 letters string of locality name. (ex. "JR")

- soil_data:

  A Data frame. Soil data. See
  [`?soil`](https://jrodriguez88.github.io/agroclimR/reference/soil.md)

- salb:

  Numeric. Albedo, fraction

- evapL:

  Numeric. Evaporation limit, (mm)

- slnf:

  Numeric. Mineralization factor, 0 to 1 scale.

- slpf:

  Numeric. Photosynthesis factor, 0 to 1 scale

- multi:

  Logical. All soil profiles in the same file

- max_depth:

  description

## Value

This function returns a `logical` if files created in path folder.

## Examples

``` r
# Write DSSAT v4.8 Soil file
soil_sample = dplyr::group_by(soil, NL) |>
dplyr::sample_n(1) |> dplyr::ungroup()
soil_files_created <- write_soil_dssat(id_name = "soil_dssat", soil_data = soil_sample)
#> Minimun data are available
#> DSSAT soil Files created in  .  : 
#>  .soil_dssat.SOL

readLines(soil_files_created[1], n = 15) |> writeLines()
#> *SOILS: AgroclimR DSSAT Soil Input File - by agroclimR
#> 
#> *soil_dssat000001  AgroclimRV1   SA      60  AgroClimR soil_dssat
#> @SITE        COUNTRY          LAT     LONG SCS FAMILY
#>  soil_dssat  AgroclimR         -99    -99 USDA Texture: SaLo
#> @ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
#>    -99  0.13     6   0.6    65  1.00  1.00 IB001 IB001 IB001
#> @  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
#>     20   -99 0.074 0.175 0.453 0.810 94.18  1.40  0.25  11.6  17.6   1.5   0.4   5.8   -99   7.9   -99
#>     40   -99 0.215 0.284 0.439 0.640 21.94  1.80  0.22  18.1  36.9   1.5   1.6   6.7   -99   5.5   -99
#>     60   -99 0.171 0.364 0.696 0.490 27.91  1.66  0.65  15.2  44.5   1.5   0.4   6.5   -99  12.7   -99
file.remove(soil_files_created)
#> [1] TRUE
```
