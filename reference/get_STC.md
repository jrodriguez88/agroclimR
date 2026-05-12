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
  Default is "USDA". Other systems require the optional `soiltexture`
  package.

## Value

String with the calculated soil texture class name, based on the input
percentages of sand, clay, and silt.

## Note

The USDA classifier is implemented internally to avoid loading GUI
dependencies during package checks. The optional `soiltexture` package
is used only when `sysclass` is not `"USDA"`.

## References

Soil Texture, The Soil Texture Wizard:

## Examples

``` r
# Calculate the soil texture class for a soil with 30% sand and 20% clay using USDA system
get_STC(30, 20)
#> [1] "SiLo"
```
