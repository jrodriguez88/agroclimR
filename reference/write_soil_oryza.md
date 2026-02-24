# Write ORYZA v3 Soil File

Function compute Soil information to ORYZA soil file.

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

  A string indicating path folder or working directory

- id_name:

  A String 4 letters string of locality name. "AIHU" = Aipe, Huila

- soil_data:

  A Data frame. Soil data. see `soil`

- ZRTMS:

  Numeric. Maximum rooting depth in the soil (m)

- WL0I:

  Numeric. Initial pounded water depth at start of simulation (mm)

- WCLI:

  Numeric/String. WCLI can take 3 values: Field Capacity ('FC'), 50% of
  Soil Saturation ('ST50'), Fraction of water content ('0.0'- '1.0')

- RIWCLI:

  A String. Re-initialize switch RIWCLI is ('YES') or ('NO')

- SATAV:

  Numeric. Soil annual average temperature of the first layers

## Value

This function returns a vector of model files created in path folder.

## Examples

``` r
# Write ORYZA Soil file
soil_sample = dplyr::group_by(soil, NL) |> dplyr::sample_n(1)
soil_files_created <- write_soil_oryza(id_name = "soil_oryza", soil_data = soil_sample)
#> Oryza Experimental Files created in  .  : 
#>  ./soil_oryza.sol

readLines(soil_files_created[1], n = 30) |> writeLines()
#> **********************************************************************
#> * Template soil data file for PADDY soil water balance model.        *
#> **********************************************************************
#> * Soil        : soil_oryza - texture classes:c("SiLo", "SaLo", "SaLo")
#> * File name        : soil_oryza.sol
#> * Sampling date      : 2014-02-26
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
#> WCSTRP = 0.53, 0.45, 0.43
#> PFCR = 6.0
#> DPLOWPAN = 0.6
#> 
#> *---------------------------------------------------------------*
#> * 3. Groundwater switch: 0=DEEP (i.e., not in profile), 1=DATA
file.remove(soil_files_created)
#> [1] TRUE
```
