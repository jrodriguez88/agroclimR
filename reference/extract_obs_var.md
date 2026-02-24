# Extract Experimental Data by Variable from agroclimR Data Workbook

Extracts experimental data based on specified variables from a set of
agroclimR data workbooks. This function is tailored to handle data in
the agroclimR xlsx format, facilitating the retrieval of agricultural
research data for different variables and models. It supports a range of
variables such as phenological data, leaf area index, dry matter, and
yield, across various crop models.

## Usage

``` r
extract_obs_var(obs_data, variable, model = "oryza")
```

## Arguments

- obs_data:

  A list containing one or more agroclimR data workbooks, as read by the
  [`read_agroclimr_data()`](https://jrodriguez88.github.io/agroclimR/reference/import_exp_data.md)
  function. Each element of the list should be a named list representing
  a single workbook, where each name-value pair corresponds to a
  specific type of observational data.

- variable:

  A character string specifying the variable to extract. Valid options
  are "phen" (phenological data), "lai" (leaf area index), "dry_matter"
  (dry matter), and "yield". This parameter determines which type of
  data the function will extract from the provided workbooks.

- model:

  A character string specifying the crop model for which data is being
  extracted. Valid options include "oryza" (for rice), "dssat" (for
  various crops), and "aquacrop" (for water-driven crop growth). This
  parameter allows the function to tailor the extraction process to the
  data structure used by different crop models.

## Value

A `tibble` containing the extracted data for the specified variable and
model. The returned tibble is structured to facilitate further analysis
and visualization, making it a valuable resource for agricultural
researchers and analysts.

## Examples

``` r
# Prepare a sample list of agroclimR data workbooks

agroclimR_list <- list(AGRO_man = agro,
    FERT_obs = fertil,
    PHEN_obs = phenol,
    PLANT_obs = plant,
    YIELD_obs = yield,
    SOIL_obs = soil,
    WTH_obs = weather
  )

obs_data = list(agroclimR_list)

# Extract phenological data for the "oryza" model
phenological_data <- extract_obs_var(obs_data, "phen", model = "oryza")
phenological_data
#> # A tibble: 21 × 4
#>    exp_file               data             var   value
#>    <chr>                  <list>           <chr> <dbl>
#>  1 SDTO_FED2000_MADRI_S1  <tibble [5 × 2]> IDAT     51
#>  2 SDTO_FED2000_MADRI_S1  <tibble [5 × 2]> FDAT     81
#>  3 SDTO_FED2000_MADRI_S1  <tibble [5 × 2]> MDAT    104
#>  4 SDTO_FED2000_MADRII_S1 <tibble [5 × 2]> IDAT     48
#>  5 SDTO_FED2000_MADRII_S1 <tibble [5 × 2]> FDAT     85
#>  6 SDTO_FED2000_MADRII_S1 <tibble [5 × 2]> MDAT    115
#>  7 SDTO_FED2000_MADRI_S2  <tibble [5 × 2]> IDAT     49
#>  8 SDTO_FED2000_MADRI_S2  <tibble [5 × 2]> FDAT     85
#>  9 SDTO_FED2000_MADRI_S2  <tibble [5 × 2]> MDAT    105
#> 10 SDTO_FED2000_MADRII_S2 <tibble [5 × 2]> IDAT     40
#> # ℹ 11 more rows

# Extract yield data for the "oryza" model
yield_data <- extract_obs_var(obs_data, "yield", model = "oryza")
yield_data
#> # A tibble: 7 × 4
#>   exp_file               var   value    se
#>   <chr>                  <chr> <dbl> <dbl>
#> 1 SDTO_FED2000_MADRI_S1  YIELD 8103.  800.
#> 2 SDTO_FED2000_MADRI_S2  YIELD 4474.  724.
#> 3 SDTO_FED2000_MADRI_S3  YIELD 5032.  852.
#> 4 SDTO_FED2000_COL_S3    YIELD 4935.  481.
#> 5 SDTO_FED2000_COL_S4    YIELD 5542.  565.
#> 6 SDTO_FED2000_MADRII_S1 YIELD 5430.  299.
#> 7 SDTO_FED2000_MADRII_S2 YIELD 5114.  437.
```
