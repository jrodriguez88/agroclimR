# Observed Grain Yield Data

A dataset capturing detailed grain yield and yield components from
experimental trials. These observations are critical for evaluating the
effectiveness of cultivation practices and genetic performance across
different rice cultivars.

## Usage

``` r
yield
```

## Format

A `data.frame` with 7 rows and 20 columns:

- ID:

  Trial ID - (`character`). Uniquely identifies each trial. Example:
  `"LOC1T1PROJ1"`.

- LOC_ID:

  Locality ID - (`character`). Denotes the trial's location. Example:
  `"LOC1"`.

- CULTIVAR:

  Cultivar name - (`character`). Identifies the cultivar used in the
  trial. Example: `"CULTIVAR1"`.

- YIELD_AVG:

  Yield average - (`numeric`). The average yield, measured in kilograms
  per hectare (`kg/ha`).

- YIELD_MIN:

  Yield minimum - (`numeric`). The minimum observed yield, in `kg/ha`.

- YIELD_MAX:

  Yield maximum - (`numeric`). The maximum observed yield, in `kg/ha`.

- HIAM:

  Harvest index at maturity - (`numeric`). The ratio of grain yield to
  total biomass at maturity.

- HIAM_SE:

  Harvest index at maturity standard error - (`numeric`). Standard error
  of the harvest index at maturity.

- PAN_fert:

  Panicle fertility - (`numeric`). Percentage of fertile panicles (%).

- PAN_fert_SE:

  Panicle fertility standard error - (`numeric`). Standard error of the
  panicle fertility percentage (%).

- GW1000:

  1000-Grain weight - (`numeric`). Weight of 1000 grains, measured in
  grams (`g`).

- GW1000_SE:

  1000-Grain weight standard error - (`numeric`). Standard error of the
  1000-grain weight (`g`).

- ST_M2:

  Number of stems per square meter - (`numeric`). The density of stems,
  in `number/m²`.

- ST_M2_SE:

  Number of stems per square meter standard error - (`numeric`).
  Standard error of stem density (`number/m²`).

- PAN_M2:

  Number of panicles per square meter - (`numeric`). The density of
  panicles, in `number/m²`.

- PAN_M2_SE:

  Number of panicles per square meter standard error - (`numeric`).
  Standard error of panicle density (`number/m²`).

- GT_PAN:

  Number of total grains per panicle - (`numeric`). Total number of
  grains per panicle.

- GT_PAN_SE:

  Number of total grains per panicle standard error - (`numeric`).
  Standard error of the total grains per panicle.

- GF_PAN:

  Number of filled grains per panicle - (`numeric`). Number of filled
  grains per panicle.

- GF_PAN_SE:

  Number of filled grains per panicle standard error - (`numeric`).
  Standard error of the filled grains per panicle.

## Source

`Rodriguez-Espinoza J, (2024)` and `CIAT-MADR-FEDEARROZ, (2016)`

## Details

Observed Grain Yield Data

This dataset presents observed grain yield and yield components data
from crop modeling experimental trials. It includes detailed
measurements of yield averages, minimum and maximum yields, harvest
index, panicle fertility, grain weight, and other vital yield
components. Such comprehensive data are essential for analyzing the
performance of different cultivars under various agronomic practices and
environmental conditions, facilitating advancements in crop yield
optimization.

## Examples

``` r
# Assuming `yield` is your dataset name
summary(yield)
#>       ID               LOC_ID            CULTIVAR           YIELD_AVG   
#>  Length:7           Length:7           Length:7           Min.   :4474  
#>  Class :character   Class :character   Class :character   1st Qu.:4984  
#>  Mode  :character   Mode  :character   Mode  :character   Median :5114  
#>                                                           Mean   :5519  
#>                                                           3rd Qu.:5486  
#>                                                           Max.   :8103  
#>    YIELD_MIN      YIELD_MAX         HIAD           HIAD_SE         
#>  Min.   :3795   Min.   :5244   Min.   :0.3001   Min.   :0.0005648  
#>  1st Qu.:4325   1st Qu.:5457   1st Qu.:0.3197   1st Qu.:0.0050892  
#>  Median :4676   Median :5719   Median :0.3561   Median :0.0089026  
#>  Mean   :4928   Mean   :6117   Mean   :0.3580   Mean   :0.0116369  
#>  3rd Qu.:5024   3rd Qu.:6004   3rd Qu.:0.3975   3rd Qu.:0.0159823  
#>  Max.   :7331   Max.   :8931   Max.   :0.4155   Max.   :0.0298476  
#>     PAN_fert      PAN_fert_SE         GW1000        GW1000_SE      
#>  Min.   :81.28   Min.   :0.3925   Min.   :20.78   Min.   :0.07145  
#>  1st Qu.:87.25   1st Qu.:0.7777   1st Qu.:23.49   1st Qu.:0.20004  
#>  Median :88.47   Median :1.7136   Median :23.86   Median :0.20738  
#>  Mean   :88.78   Mean   :2.0115   Mean   :23.73   Mean   :0.24269  
#>  3rd Qu.:90.88   3rd Qu.:2.5408   3rd Qu.:24.55   3rd Qu.:0.26799  
#>  Max.   :95.46   Max.   :5.3375   Max.   :25.38   Max.   :0.48396  
#>      ST_M2          ST_M2_SE         PAN_M2        PAN_M2_SE     
#>  Min.   :308.0   Min.   : 4.00   Min.   :192.1   Min.   : 6.009  
#>  1st Qu.:356.9   1st Qu.:13.00   1st Qu.:301.8   1st Qu.:15.000  
#>  Median :373.5   Median :20.82   Median :333.9   Median :20.000  
#>  Mean   :394.8   Mean   :20.09   Mean   :346.5   Mean   :20.931  
#>  3rd Qu.:407.0   3rd Qu.:24.85   3rd Qu.:382.1   3rd Qu.:26.252  
#>  Max.   :554.0   Max.   :40.10   Max.   :531.8   Max.   :38.000  
#>      GT_PAN         GT_PAN_SE            GF_PAN        GF_PAN_SE      
#>  Min.   : 67.97   Min.   : 0.05277   Min.   :60.02   Min.   : 0.6436  
#>  1st Qu.: 68.86   1st Qu.: 1.77913   1st Qu.:60.57   1st Qu.: 1.5229  
#>  Median : 69.39   Median : 2.91998   Median :62.67   Median : 3.5495  
#>  Mean   : 75.86   Mean   : 7.37684   Mean   :66.90   Mean   : 6.5664  
#>  3rd Qu.: 72.23   3rd Qu.:12.34776   3rd Qu.:67.23   3rd Qu.: 9.4401  
#>  Max.   :111.52   Max.   :20.41136   Max.   :90.01   Max.   :19.8455  
```
