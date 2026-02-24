# Soil Pedotransfer Function (PTF) to estimate Soil Saturated Hydraulic Conductivity (SSKS)

This function calculates the Saturated Hydraulic Conductivity (SSKS)
based on various soil pedotransfer functions (PTFs) using soil
properties as input parameters.

## Usage

``` r
SSKS_cal(
  S,
  C,
  SOM = 1.5,
  SBDM = 1.5,
  kmin = 0.1,
  kmax = 250,
  output = "bootmean"
)

SSKS_Suleiman_Ritchie(S, C, SOM, SBDM, SPOR = NULL, WCFC = NULL)

SSKS_Saxton(S, C, SOM, SBDM, WCFC = NULL, WCWP = NULL, WCST = NULL)
```

## Arguments

- S:

  Sand content (%) of the soil.

- C:

  Clay content (%) of the soil.

- SOM:

  Soil Organic Matter content (%) of the soil. Default is 1.5.

- SBDM:

  Soil Bulk Density (g/cm3) of the soil. Default is 1.5.

- kmin:

  Minimum threshold value for SSKS (mm/h). Default is 0.1.

- kmax:

  Maximum threshold value for SSKS (mm/h). Default is 250.

- output:

  Type of output to be returned. Options are 'data', 'summary',
  'bootmean', 'mean', 'bootmedian', 'median', 'min', 'max', and 'sd'.
  Default is 'bootmean'.

- SPOR:

  Total Soil porosity (%)

- WCFC:

  Water Content at Field Capacity (%) - (-33kPa)

- WCWP:

  Water Content at Wilting Point (%) - (-1500 kPa)

- WCST:

  Water Content at Saturation (%) - (-10kPa)

## Value

Returns a list or a numeric value of SSKS (mm/h) based on the 'output'
parameter. If 'output' is 'data', it returns the complete dataset. If
'output' is 'summary', it returns the summary statistics of SSKS values.
If 'output' is any of the other specified types, it returns the
corresponding statistic.

## References

SSKS Pedotransfer Functions:

- Brakensiek et al. (1984) for SSKS_Brakensiek

- Campbell and Shiozawa (1994) for SSKS_Campbell

- Cosby et al. (1984) for SSKS_Cosby

- Dane and Puckett (1994) for SSKS_Dane_Puck

- Ferrer Julià et al. (2004) for SSKS_Ferrer

- Jabro (1992) for SSKS_Jabro

- Puckett et al. (1985) for SSKS_Puckett

- RAWLS (1986) for SSKS_Rawls

- Saxton, K. E., & Rawls, W. J. (2006). for SSKS_Saxton.

- Suleiman, A. A., & Ritchie, J. T. (2001). for SSKS_Suleiman_Ritchie

- Vereecken et al. (1990) for SSKS_Vereecken

- Wösten et al. (1999) for SSKS_Wosten99

## Examples

``` r
# Function Arguments
S <- 28
C = 15.7
SOM <- 2
SBDM = 1.5

# SSKS multi PTF
SSKS_cal(S, C)
#> WCFC was estimated using Saxton-PTF
#> Porosity was estimated using 2.65g/cm3 as particle density
#> [1] 9.474758
SSKS_cal(S, C, SOM, SBDM)
#> WCFC was estimated using Saxton-PTF
#> Porosity was estimated using 2.65g/cm3 as particle density
#> [1] 8.743532
SSKS_cal(S, C, output = 'summary')
#> WCFC was estimated using Saxton-PTF
#> Porosity was estimated using 2.65g/cm3 as particle density
#> # A tibble: 1 × 7
#>   ssks_bootmean ssks_mean ssks_bootmedian ssks_median ssks_min ssks_max ssks_sd
#>           <dbl>     <dbl>           <dbl>       <dbl>    <dbl>    <dbl>   <dbl>
#> 1          9.95      9.55            10.0        8.98    0.256     31.7    8.58
SSKS_cal(S = 28.3, C = 15.7, output = 'mean')
#> WCFC was estimated using Saxton-PTF
#> Porosity was estimated using 2.65g/cm3 as particle density
#> [1] 9.584273

#' #For specific SSKS PTF:
SSKS_Saxton(S, C, SOM, SBDM)
#> [1] 10.61161
SSKS_Suleiman_Ritchie(S, C, SOM, SBDM)
#> WCFC was estimated using Saxton-PTF
#> Porosity was estimated using 2.65g/cm3 as particle density
#> [1] 8.923457
```
