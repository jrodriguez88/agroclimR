# Observed Fertilization Plan Data

A dataset detailing fertilization management or application data across
various experimental trials for research in agricultural productivity
and sustainability. It includes information on trial identification,
locality, project IDs, and specific details on fertilizer applications.

## Usage

``` r
fertil
```

## Format

A `data.frame` with 37 rows and 7 columns:

- ID:

  Trial ID - (`character`). Serves as a unique identifier for each
  trial. Example: `"LOC1T1PROJ1"`.

- LOC_ID:

  Locality ID - (`character`). Identifies the location of the trial.
  Example: `"LOC1"`.

- PROJECT:

  Project ID - (`character`). Associates the trial with a specific
  project. Example: `"PROJ1"`.

- FERT_No:

  Fertilizer application number - (`numeric`). Indicates the sequence
  number of the fertilizer application.

- DDE:

  Days after Emergence - (`numeric`). Specifies the number of days after
  crop emergence when the fertilizer was applied.

- N:

  Nitrogen - (`numeric`). Amount of nitrogen applied, in kilograms per
  hectare (`kg/ha`).

- P:

  Phosphate - (`numeric`). Amount of phosphate applied, in kilograms per
  hectare (`kg/ha`).

- K:

  Potassium - (`numeric`). Amount of potassium applied, in kilograms per
  hectare (`kg/ha`).

## Source

`Rodriguez-Espinoza J, (2024)` and `CIAT-MADR-FEDEARROZ, (2016)`

## Details

Observed Fertilization Plan Data

This dataset contains detailed information on fertilization management
or application for each experimental trial identified with an ID in the
workbook of the agroclimR package. It is designed to provide insights
into the nutrient management practices applied across different trials,
facilitating research in agricultural productivity and sustainability.

## Examples

``` r
# Assuming `fertil` is your dataset name
head(fertil)
#> # A tibble: 6 × 7
#>   ID           LOC_ID FERT_No   DDE     N     P     K
#>   <chr>        <chr>    <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 SDTOS1MADRI  SDTO         1    10  42    21.5    33
#> 2 SDTOS1MADRI  SDTO         2    23  58     1.5    33
#> 3 SDTOS1MADRI  SDTO         3    38  56.5   0      30
#> 4 SDTOS1MADRI  SDTO         4    52  51.2   0      15
#> 5 SDTOS1MADRI  SDTO         5    64  18.4   0       0
#> 6 SDTOS1MADRII SDTO         1     9  42    21.5    48
```
