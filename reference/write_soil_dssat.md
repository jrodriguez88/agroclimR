# Write a DSSAT v4.8 soil file

Formats a soil profile as a DSSAT `.SOL` file. The writer converts the
soil variables used by agroclimR to DSSAT layer variables and can append
multiple profiles to the same file when `multi = TRUE`.

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

  Character. Directory where the `.SOL` file will be written.

- id_name:

  Character. Soil profile identifier used as the output file name,
  without extension.

- soil_data:

  Data frame with one row per soil layer. Expected columns include
  `LOC_ID`, `NL`, `DEPTH`, `SBDM`, `SOC`, `SLON`, `SNH4`, `SNO3`,
  `WCFC`, `WCST`, `WCWP`, `CLAY`, `SILT`, `PH`, `SCEC`, `SSKS`, and
  `STC`; see the example data set
  [soil](https://jrodriguez88.github.io/agroclimR/reference/soil.md).

- salb:

  Numeric. Soil albedo (`SALB`) as a fraction.

- evapL:

  Numeric. Stage-one soil evaporation limit (`SLU1`) in mm.

- sldr:

  Numeric. Drainage rate (`SLDR`) as a fraction per day.

- slnf:

  Numeric. Mineralization factor (`SLNF`) on a 0 to 1 scale.

- slpf:

  Numeric. Photosynthesis factor (`SLPF`) on a 0 to 1 scale.

- multi:

  Logical. If `TRUE`, appends a profile to an existing DSSAT soil file.
  If `FALSE`, overwrites the file for `id_name`.

## Value

Character vector with the path of the DSSAT soil file created.

## Examples

``` r
# Write DSSAT v4.8 Soil file
soil_sample = dplyr::group_by(soil, NL) |>
dplyr::sample_n(1) |> dplyr::ungroup()
soil_files_created <- write_soil_dssat(
  path = tempdir(), id_name = "soil_dssat", soil_data = soil_sample)
#> Minimun data are available
#> DSSAT soil Files created in  /tmp/RtmpJqE3M0  : 
#>  /tmp/RtmpJqE3M0/soil_dssat.SOL

readLines(soil_files_created[1], n = 15) |> writeLines()
#> *SOILS: AgroclimR DSSAT Soil Input File - by agroclimR
#> 
#> *soil_dssat000001  AgroclimRV1   LO      60  AgroClimR soil_dssat
#> @SITE        COUNTRY          LAT     LONG SCS FAMILY
#>  soil_dssat  AgroclimR         -99    -99 USDA Texture: Lo
#> @ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE
#>    -99  0.13     6   0.6    75  1.00  1.00 IB001 IB001 IB001
#> @  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC
#>     20   -99 0.318 0.365 0.379 0.810 21.86  1.56  1.32  18.3  39.1   1.5   0.6   6.3   -99   9.6   -99
#>     40   -99 0.245 0.305 0.356 0.640 29.34  1.69  0.65  14.5  37.9   1.5   0.6   6.7   -99   9.7   -99
#>     60   -99 0.275 0.316 0.355 0.490 24.73  1.70  0.50  15.7  40.4   1.5   0.6   6.7   -99  14.0   -99
file.remove(soil_files_created)
#> [1] TRUE
```
