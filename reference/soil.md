# Observed Soil Data

A dataset of observed soil data providing detailed chemical and physical
properties of soil from experimental trials. These observations are
critical for crop modeling, allowing for an in-depth analysis of soil
conditions that affect agricultural productivity.

## Usage

``` r
soil
```

## Format

A `data.frame` with 18 rows and 22 columns:

- ID:

  Trial ID - (`character`). A unique identifier for each trial. Example:
  `"LOC1T1PROJ1"`.

- LOC_ID:

  Locality ID - (`character`). Indicates the location of the trial.
  Example: `"LOC1"`.

- SAMPLING_DATE:

  Sampling date - (`date`). The date when the plant sampling was
  conducted, in MM/DD/YYYY format.

- NL:

  Number of soil layers - (`numeric`). Indicates the maximum layers
  considered, up to 10.

- DEPTH:

  Thickness of each soil layer - (`numeric`). Measured in centimeters
  (`cm`).

- STC:

  Soil Texture Class (12-USDA) - (`character`). Represents the USDA
  texture class.

- SAND:

  Soil sand content - (`numeric`). Percentage of sand content (%).

- SILT:

  Soil silt content - (`numeric`). Percentage of silt content (%).

- CLAY:

  Soil clay content - (`numeric`). Percentage of clay content (%).

- SBDM:

  Soil Bulk Density - (`numeric`). Measured in grams per cubic
  centimeter (`g/cm³`).

- SOC:

  Soil organic carbon - (`numeric`). Measured in grams per kilogram
  (`g/kg`).

- SLON:

  Soil Organic Nitrogen - (`numeric`). Measured in milligrams per
  kilogram (`mg/kg`).

- SNH4:

  Ammonium (KCl, elemental N) - (`numeric`). Measured in milligrams per
  kilogram (`mg/kg`).

- SNO3:

  Nitrate (KCl, elemental N) - (`numeric`). Measured in milligrams per
  kilogram (`mg/kg`).

- PH:

  pH - (`numeric`). A measure of soil acidity or alkalinity.

- SCEC:

  Cation exchange capacity - (`numeric`). Measured in centimoles per
  kilogram (`cmol/kg`).

- WCST:

  Saturated volumetric water content - (`numeric`). Percentage of water
  content when soil is saturated (%).

- WCFC:

  Volumetric water content at field capacity - (`numeric`). Percentage
  of water content at field capacity (%).

- WCWP:

  Volumetric water content at wilting point - (`numeric`). Percentage of
  water content at wilting point (%).

- WCAD:

  Volumetric water content at air dryness - (`numeric`). Percentage of
  water content when soil is air dry (%).

- SSKS:

  Saturated hydraulic conductivity - (`numeric`). Measured in milimeters
  per hour (`mm/h`).

## Source

`Rodriguez-Espinoza J, (2024)` and `CIAT-MADR-FEDEARROZ, (2016)`

## Details

Observed Soil Data

This dataset contains comprehensive chemical and physical soil data from
the same experimental trials used in crop modeling within the agroclimR
package framework. It includes details on soil layers, texture, content,
and other vital soil properties, essential for understanding soil
characteristics that influence crop growth and development.

## Examples

``` r
# Assuming `soil` is your dataset name
summary(soil)
#>       ID               LOC_ID          SAMPLING_DATE              NL   
#>  Length:18          Length:18          Min.   :2013-05-10   Min.   :1  
#>  Class :character   Class :character   1st Qu.:2014-02-26   1st Qu.:1  
#>  Mode  :character   Mode  :character   Median :2014-06-01   Median :2  
#>                                        Mean   :2014-08-27   Mean   :2  
#>                                        3rd Qu.:2015-08-10   3rd Qu.:3  
#>                                        Max.   :2015-12-23   Max.   :3  
#>      DEPTH        STC                 SAND            SILT       
#>  Min.   :20   Length:18          Min.   :28.27   Min.   : 2.931  
#>  1st Qu.:20   Class :character   1st Qu.:41.63   1st Qu.: 8.775  
#>  Median :20   Mode  :character   Median :47.64   Median :38.443  
#>  Mean   :20                      Mean   :56.49   Mean   :29.258  
#>  3rd Qu.:20                      3rd Qu.:77.86   3rd Qu.:43.547  
#>  Max.   :20                      Max.   :83.14   Max.   :55.999  
#>       CLAY             SBDM            SOC              SLON        
#>  Min.   : 8.959   Min.   :1.401   Min.   : 0.400   Min.   :  94.47  
#>  1st Qu.:13.335   1st Qu.:1.496   1st Qu.: 1.425   1st Qu.: 299.97  
#>  Median :14.448   Median :1.575   Median : 3.500   Median : 450.21  
#>  Mean   :14.255   Mean   :1.595   Mean   : 4.365   Mean   : 596.37  
#>  3rd Qu.:15.522   3rd Qu.:1.696   3rd Qu.: 6.515   3rd Qu.: 650.02  
#>  Max.   :18.258   Max.   :1.795   Max.   :13.250   Max.   :2045.03  
#>       SNH4             SNO3              PH             SCEC       
#>  Min.   : 1.981   Min.   : 0.023   Min.   :5.180   Min.   : 1.740  
#>  1st Qu.: 4.270   1st Qu.: 1.563   1st Qu.:6.143   1st Qu.: 5.367  
#>  Median : 5.307   Median : 3.293   Median :6.310   Median : 7.947  
#>  Mean   : 6.114   Mean   : 7.364   Mean   :6.256   Mean   : 7.947  
#>  3rd Qu.: 6.114   3rd Qu.:10.281   3rd Qu.:6.508   3rd Qu.:11.273  
#>  Max.   :19.177   Max.   :49.553   Max.   :6.870   Max.   :14.000  
#>       WCST            WCFC            WCWP             WCAD       
#>  Min.   :35.52   Min.   :11.25   Min.   : 5.149   Min.   : 3.830  
#>  1st Qu.:38.34   1st Qu.:15.46   1st Qu.: 7.513   1st Qu.: 6.339  
#>  Median :43.84   Median :27.46   Median :16.628   Median : 6.954  
#>  Mean   :45.90   Mean   :25.05   Mean   :15.919   Mean   : 7.046  
#>  3rd Qu.:46.41   3rd Qu.:34.25   3rd Qu.:21.696   3rd Qu.: 8.080  
#>  Max.   :69.60   Max.   :40.77   Max.   :31.793   Max.   :10.317  
#>       SSKS      
#>  Min.   :21.86  
#>  1st Qu.:28.10  
#>  Median :36.32  
#>  Mean   :52.82  
#>  3rd Qu.:85.64  
#>  Max.   :98.00  
```
