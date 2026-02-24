# Observed Plant Phenological Data

A dataset containing key phenological dates for plant cultivars used in
crop modeling trials. Each record details the phenological stages of
rice growth (similar for others crops).

## Usage

``` r
phenol
```

## Format

A `data.frame` with 7 rows and 8 columns:

- ID:

  Trial ID - (`character`). Serves as a unique identifier for each
  trial. Example: `"LOC1T1PROJ1"`.

- LOC_ID:

  Locality ID - (`character`). Denotes the trial's location. Example:
  `"LOC1"`.

- CULTIVAR:

  Cultivar name - (`character`). Identifies the cultivar used in the
  trial. Example: `"CULTIVAR1"`.

- EDAT:

  Emergence date - (`date`). The date when the plants emerged. Format:
  MM/DD/YYYY.

- IDAT:

  Panicle initiation date - (`date`). The date when panicle initiation
  was observed. Format: MM/DD/YYYY.

- FDAT:

  Flowering date - (`date`). The date when flowering occurred. Format:
  MM/DD/YYYY.

- MDAT:

  Maturity date - (`date`). The date when the plants reached maturity.
  Format: MM/DD/YYYY.

## Source

`Rodriguez-Espinoza J, (2024)` and `CIAT-MADR-FEDEARROZ, (2016)`

## Details

Observed Plant Phenological Data

This dataset encompasses observed plant phenological dates for crop
modeling trials. It provides crucial milestones in the growth cycle of
plants (based on rice cultivars), including dates of emergence, panicle
initiation, flowering, and maturity. These phenological markers are
essential for understanding crop development stages, assessing the
impact of agronomic practices, and guiding management decisions in rice
cultivation.

## Examples

``` r
# Assuming `phenol` is your dataset name
head(phenol)
#> # A tibble: 6 × 8
#>   ID      LOC_ID CULTIVAR PDAT       EDAT       IDAT       FDAT       MDAT      
#>   <chr>   <chr>  <chr>    <date>     <date>     <date>     <date>     <date>    
#> 1 SDTOS1… SDTO   FED2000  2013-04-29 2013-05-08 2013-06-28 2013-07-28 2013-08-27
#> 2 SDTOS1… SDTO   FED2000  2015-06-05 2015-06-14 2015-08-01 2015-09-07 2015-10-14
#> 3 SDTOS2… SDTO   FED2000  2013-12-05 2013-12-12 2014-01-30 2014-03-07 2014-04-03
#> 4 SDTOS2… SDTO   FED2000  2015-11-03 2015-11-12 2015-12-22 2016-01-23 2016-02-26
#> 5 SDTOS3… SDTO   FED2000  2014-10-07 2014-10-16 2014-12-02 2015-01-03 2015-02-02
#> 6 SDTOS3… SDTO   FED2000  2014-02-05 2014-02-11 2014-03-24 2014-04-28 2014-06-05
```
