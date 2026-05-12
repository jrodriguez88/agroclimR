# Write an ORYZA v3 soil file

Formats a soil profile as an ORYZA v3 `.sol` file for the PADDY soil
water balance module. The writer fills hydrological, nitrogen and
initialization sections from the supplied layer data.

## Usage

``` r
write_soil_oryza(
  path = ".",
  id_name,
  soil_data,
  ZRTMS = 0.5,
  WL0I = 0,
  WCLI = "FC",
  RIWCLI = "NO",
  SATAV = 20
)
```

## Arguments

- path:

  Character. Directory where the `.sol` file will be written.

- id_name:

  Character. Soil profile identifier used as the output file name,
  without extension.

- soil_data:

  Data frame with one row per soil layer. Expected columns include
  `DEPTH`, `SBDM`, `SOC`, `SLON`, `SNH4`, `SNO3`, `WCST`, `WCFC`,
  `WCWP`, `WCAD`, `CLAY`, `SAND`, `SSKS`, `STC`, and `SAMPLING_DATE`;
  see the example data set
  [soil](https://jrodriguez88.github.io/agroclimR/reference/soil.md).

- ZRTMS:

  Numeric. Maximum rooting depth in the soil, in meters.

- WL0I:

  Numeric. Initial ponded water depth at the start of simulation, in mm.

- WCLI:

  Character or numeric. Initial volumetric water content. Use `"FC"` for
  field capacity, `"ST50"` for 50 percent of saturation, or a numeric
  fraction to repeat across layers.

- RIWCLI:

  Character. Re-initialization switch, usually `"YES"` or `"NO"`.

- SATAV:

  Numeric. Annual average soil temperature of the upper layers, in
  degrees Celsius.

## Value

Character vector with the path of the ORYZA soil file created.

## Examples

``` r
# Write ORYZA Soil file
soil_sample = dplyr::group_by(soil, NL) |> dplyr::sample_n(1)
soil_files_created <- write_soil_oryza(
  path = tempdir(), id_name = "soil_oryza", soil_data = soil_sample)
#> Oryza soil files created in  /tmp/RtmpEvHEAr  : 
#>  /tmp/RtmpEvHEAr/soil_oryza.sol

readLines(soil_files_created[1], n = 30) |> writeLines()
#> **********************************************************************
#> * Template soil data file for PADDY soil water balance model.        *
#> **********************************************************************
#> * Soil        : soil_oryza - texture classes:c("Lo", "Lo", "SaLo")
#> * File name        : soil_oryza.sol
#> * Sampling date      : 2015-12-23
#> * Additional info  : Create with agroclimR
#> *--------------------------------------------------------------------*
#> 
#> SCODE = 'PADDY'
#> 
#> *---------------------------------------------------------------*
#> * 1. Various soil and management parameters
#> *---------------------------------------------------------------*
#> WL0MX = 100.   ! Bund height (mm)
#> NL = 3        ! Number of soil layers (maximum is 10) (-)
#> TKL = 0.20, 0.20, 0.20   ! Thickness of each soil layer (m)
#> ZRTMS = 0.5   ! Maximum rooting depth in the soil (m)
#> 
#> *---------------------------------------------------------------*
#> * 2. Puddling switch: 1=PUDDLED or 0=NON PUDDLED
#> *---------------------------------------------------------------*
#> SWITPD = 0  !Non puddled
#> NLPUD = 1
#> WCSTRP = 0.38, 0.60, 0.46
#> PFCR = 6.0
#> DPLOWPAN = 0.6
#> 
#> *---------------------------------------------------------------*
#> * 3. Groundwater switch: 0=DEEP (i.e., not in profile), 1=DATA
file.remove(soil_files_created)
#> [1] TRUE
```
