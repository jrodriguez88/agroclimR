# General information and Agronomic Data

A dataset for detailed agronomic and general information on experimental
trials for crop modeling. It is the master table for the workbook
proposed in the agroclimR package.

## Usage

``` r
agro
```

## Format

A `data.frame` with 7 rows and 14 columns:

- ID:

  Trial ID - (`character`). A unique identifier for each trial. Example:
  `"LOC1T1PROJ1"`.

- LOC_ID:

  Locality ID - (`character`). Indicates the location of the trial.
  Example: `"LOC1"`.

- PROJECT:

  Project ID - (`character`). Associates the trial with a specific
  project. Example: `"PROJ1"`.

- CULTIVAR:

  Cultivar name - (`character`). Specifies the plant cultivar used in
  the trial. Example: `"CULTIVAR1"`.

- TR_N:

  Treatment number - (`character`). Example: `"T1"`.

- LAT:

  Latitude in decimal degrees - (`numeric`).

- LONG:

  Longitude in decimal degrees - (`numeric`).

- ALT:

  Elevation in meters above sea level - (`numeric`).

- PDAT:

  Planting date in MM/DD/YYYY format - (`date`).

- CROP_SYS:

  Crop system - (`character`). Example: `"IRRIGATED-RAINFED"`.

- ESTAB:

  Establishment method - (`character`). Example:
  `"TRANSPLANT-DIRECT-SEED"`.

- NPLDS:

  Number of plants per square meter - (`numeric`). Example: `number/m2`.

- SBDUR:

  Seed-bed duration in days - (`numeric`).

- TRDAT:

  Transplanting date in MM/DD/YYYY format - (`date`).

## Source

`Rodriguez-Espinoza J, (2024)` and `CIAT-MADR-FEDEARROZ, (2016)`

## Details

General information and Agronomic Data

This dataset encompasses comprehensive agronomic and general information
about specific experimental trials aimed at crop modeling, serving as
the master table for the workbook proposed in the agroclimR package. It
provides a foundation for advanced analyses in agricultural research,
detailing trial identification, geographic locations, crop varieties,
agronomic treatments, and other pertinent variables.

## Examples

``` r
# Assuming `agro` is your dataset name
head(agro)
#> # A tibble: 6 × 14
#>   ID         LOC_ID PROJECT CULTIVAR TR_N    LAT  LONG   ALT PDAT       CROP_SYS
#>   <chr>      <chr>  <chr>   <chr>    <chr> <dbl> <dbl> <dbl> <date>     <chr>   
#> 1 SDTOS1MAD… SDTO   MADRI   FED2000  S1     3.91 -75.0   415 2013-04-29 IRRIGAT…
#> 2 SDTOS1MAD… SDTO   MADRII  FED2000  S1     3.91 -75.0   415 2015-06-05 IRRIGAT…
#> 3 SDTOS2MAD… SDTO   MADRI   FED2000  S2     3.91 -75.0   415 2013-12-05 IRRIGAT…
#> 4 SDTOS2MAD… SDTO   MADRII  FED2000  S2     3.91 -75.0   415 2015-11-03 IRRIGAT…
#> 5 SDTOS3COL  SDTO   COL     FED2000  S3     3.91 -75.0   415 2014-10-07 IRRIGAT…
#> 6 SDTOS3MAD… SDTO   MADRI   FED2000  S3     3.91 -75.0   415 2014-02-05 IRRIGAT…
#> # ℹ 4 more variables: ESTAB <chr>, NPLDS <dbl>, SBDUR <lgl>, TRDAT <lgl>
```
