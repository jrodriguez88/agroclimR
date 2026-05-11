# Soil Pedotransfer Function (PTF) to estimate Soil Water Content (WC -v/v (%))

This functions calculates the Saturated Hydraulic Conductivity (SSKS)
based on various soil pedotransfer functions (PTFs) using soil
properties as input parameters.

## Usage

``` r
WCWP_Saxton(S, C, SOM)

WCFC_Saxton(S, C, SOM)

WCST_Saxton(SBDM)

WCST_Tomasella(S, C, SBDM, GWCFC)

WCFC_Tomasella(SBDM, GWCFC)

WCWP_Tomasella(C, SBDM, GWCFC)

WCST_Minasny(S, SBDM)

WCFC_Minasny(S, SBDM)

WCWP_Minasny(C, SOM)
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

- GWCFC:

  Gravimetric Water Content at Field Capacity (%)

- SPOR:

  Total Soil porosity (%)

## Value

Returns a list or a numeric value of SSKS (mm/h) based on the 'output'
parameter. If 'output' is 'data', it returns the complete dataset. If
'output' is 'summary', it returns the summary statistics of SSKS values.
If 'output' is any of the other specified types, it returns the
corresponding statistic.

## References

- Saxton, K. E., & Rawls, W. J. (2006). Soil Water Characteristic
  Estimates by Texture and Organic Matter for Hydrologic Solutions. Soil
  Science Society of America Journal, 70(5), 1569-1578.
  https://doi.org/10.2136/sssaj2005.0117

- Minasny, B., & Hartemink, A. E. (2011). Predicting soil properties in
  the tropics. Earth-Science Reviews, 106(1-2), 52-62.
  https://doi.org/10.1016/j.earscirev.2011.01.005

- Tomasella, J., & Hodnett, M. (2004). Pedotransfer functions for
  tropical soils. Developments in Soil Science, 30, 415-429.
  https://doi.org/10.1016/S0166-2481(04)30021-8

- Pachepsky, Y., & Rawls, W. J. (Eds.). (2004). Development of
  pedotransfer functions in soil hydrology (Vol. 30). Elsevier.
  https://www.elsevier.com/books/development-of-pedotransfer-functions-in-soil-hydrology/pachepsky/978-0-444-51705-0

- Ghanbarian, B., Taslimitehrani, V., & Pachepsky, Y. A. (2017).
  Accuracy of sample dimension-dependent pedotransfer functions in
  estimation of soil saturated hydraulic conductivity. Catena, 149,
  374-380. https://doi.org/10.1016/j.catena.2016.10.015

- Ferrer Julia, M., T.E. Monreal, A.S. del Corral Jimenez, and E. Garcia
  Melendez. 2004. Constructing a saturated hydraulic conductivity map of
  Spain using pedotransfer functions and spatial prediction. Geoderma
  123:257-277. doi:10.1016/j.geoderma.2004.02.011

- Suleiman, A. A., & Ritchie, J. T. (2001). Estimating saturated
  hydraulic conductivity from soil porosity. Transactions of the ASAE,
  44(2), 235. https://doi.org/10.13031/2013.4683

## Examples

``` r
# Function Arguments
S <- 28
C <- 15.7
SOM <- 2
SBDM <- 1.5
GWCFC <- 25

WCFC_Saxton(S, C, SOM)
#> [1] 28.28278

WCST_Tomasella(S, C, SBDM, GWCFC)
#> [1] 43.11959

WCWP_Minasny(C, SOM)
#> [1] 14.014
```
