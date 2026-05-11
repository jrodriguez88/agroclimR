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
  centimeter (`g/cm3`).

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
#>          ID           LOC_ID   SAMPLING_DATE              NL        DEPTH   
#>  Length   :18   Length   :18   Min.   :2013-05-10   Min.   :1   Min.   :20  
#>  N.unique : 6   N.unique : 1   1st Qu.:2014-02-26   1st Qu.:1   1st Qu.:20  
#>  N.blank  : 0   N.blank  : 0   Median :2014-06-01   Median :2   Median :20  
#>  Min.nchar:11   Min.nchar: 4   Mean   :2014-08-27   Mean   :2   Mean   :20  
#>  Max.nchar:12   Max.nchar: 4   3rd Qu.:2015-08-10   3rd Qu.:3   3rd Qu.:20  
#>                                Max.   :2015-12-23   Max.   :3   Max.   :20  
#>         STC          SAND            SILT             CLAY       
#>  Length   :18   Min.   :28.27   Min.   : 2.931   Min.   : 8.959  
#>  N.unique : 3   1st Qu.:41.63   1st Qu.: 8.775   1st Qu.:13.335  
#>  N.blank  : 0   Median :47.64   Median :38.443   Median :14.448  
#>  Min.nchar: 2   Mean   :56.49   Mean   :29.258   Mean   :14.255  
#>  Max.nchar: 4   3rd Qu.:77.86   3rd Qu.:43.547   3rd Qu.:15.522  
#>                 Max.   :83.14   Max.   :55.999   Max.   :18.258  
#>       SBDM            SOC              SLON              SNH4       
#>  Min.   :1.401   Min.   : 0.400   Min.   :  94.47   Min.   : 1.981  
#>  1st Qu.:1.496   1st Qu.: 1.425   1st Qu.: 299.97   1st Qu.: 4.270  
#>  Median :1.575   Median : 3.500   Median : 450.21   Median : 5.307  
#>  Mean   :1.595   Mean   : 4.365   Mean   : 596.37   Mean   : 6.114  
#>  3rd Qu.:1.696   3rd Qu.: 6.515   3rd Qu.: 650.02   3rd Qu.: 6.114  
#>  Max.   :1.795   Max.   :13.250   Max.   :2045.03   Max.   :19.177  
#>       SNO3              PH             SCEC             WCST      
#>  Min.   : 0.023   Min.   :5.180   Min.   : 1.740   Min.   :35.52  
#>  1st Qu.: 1.563   1st Qu.:6.143   1st Qu.: 5.367   1st Qu.:38.34  
#>  Median : 3.293   Median :6.310   Median : 7.947   Median :43.84  
#>  Mean   : 7.364   Mean   :6.256   Mean   : 7.947   Mean   :45.90  
#>  3rd Qu.:10.281   3rd Qu.:6.508   3rd Qu.:11.273   3rd Qu.:46.41  
#>  Max.   :49.553   Max.   :6.870   Max.   :14.000   Max.   :69.60  
#>       WCFC            WCWP             WCAD             SSKS      
#>  Min.   :11.25   Min.   : 5.149   Min.   : 3.830   Min.   :21.86  
#>  1st Qu.:15.46   1st Qu.: 7.513   1st Qu.: 6.339   1st Qu.:28.10  
#>  Median :27.46   Median :16.628   Median : 6.954   Median :36.32  
#>  Mean   :25.05   Mean   :15.919   Mean   : 7.046   Mean   :52.82  
#>  3rd Qu.:34.25   3rd Qu.:21.696   3rd Qu.: 8.080   3rd Qu.:85.64  
#>  Max.   :40.77   Max.   :31.793   Max.   :10.317   Max.   :98.00  
```
