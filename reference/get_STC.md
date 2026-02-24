# Calculate Soil Texture Class

This function calculates the soil texture class based on the percentages
of sand and clay using the specified soil classification system (default
is USDA).

## Usage

``` r
get_STC(S, C, sysclass = "USDA")
```

## Arguments

- S:

  Numeric vector indicating the percentage of sand in the soil.

- C:

  Numeric vector indicating the percentage of clay in the soil.

- sysclass:

  Character string specifying the soil classification system to use.
  Default is "USDA", but can be changed if other systems are supported
  by the `soiltexture` package.

## Value

String with the calculated soil texture class name, based on the input
percentages of sand, clay, and silt.

## Note

This function requires the `soiltexture` package to calculate the soil
texture class. Ensure that the `soiltexture` package is installed and
loaded into your R session.

## References

Soil Texture, The Soil Texture Wizard:

## Examples

``` r
# Calculate the soil texture class for a soil with 30% sand and 20% clay using USDA system
get_STC(30, 20)
#> [1] "Lo, SiLo"
```
