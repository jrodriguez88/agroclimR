# Write ORYZA v3 Experimental File (.EXP)

`write_exp_oryza()` performs transformation from experimental data to
ORYZA v3 file model format.

## Usage

``` r
write_exp_oryza(agroclimr_list, path, ET_mod = "PRIESTLY TAYLOR")
```

## Arguments

- agroclimr_list:

  R list imported from excel workbook using
  [`read_agroclimr_data()`](https://jrodriguez88.github.io/agroclimR/reference/import_exp_data.md).

- path:

  A string indicating path folder or working directory

- ET_mod:

  String indicating is method for evapotranspiration calculation,
  'PENMAN' = Penman-based (Van Kraalingen& Stol,1996), 'PRIESTLY TAYLOR'
  = Priestly-Taylor ("),

## Value

This function returns a `vector` of model files created in path folder.

## Examples

``` r
#' # File names vector, extension include
name_file = c("agroclimR_workbook.xlsx")

# Files directory
test_file = system.file("extdata", name_file, package = "agroclimR")

# Import data to R lists and tibble formats
agroclimr_list = read_agroclimr_data(test_file)

# Write Oryza Experimental Files
exp_files_created <- write_exp_oryza(agroclimr_list, path = "./")
#> No LAI in exp_file: ./SDTO_FED2000_MADRI_S1.exp

exp_files_created
#> [1] "./SDTO_FED2000_MADRI_S1.exp"  "./SDTO_FED2000_MADRII_S1.exp"
#> [3] "./SDTO_FED2000_MADRI_S2.exp"  "./SDTO_FED2000_MADRII_S2.exp"
#> [5] "./SDTO_FED2000_COL_S3.exp"    "./SDTO_FED2000_MADRI_S3.exp" 
#> [7] "./SDTO_FED2000_COL_S4.exp"   
file.remove(exp_files_created)
#> [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE
```
