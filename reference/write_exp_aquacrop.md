# Write AquaCrop project files

Creates AquaCrop project (`.PRM`) files that link climate, CO2, crop,
irrigation, management, soil and initial-condition files for one or more
simulation cycles. The function expects the referenced AquaCrop input
files to exist in the project directory.

## Usage

``` r
write_exp_aquacrop(
  path_proj,
  id_name,
  clim_name,
  soil_name,
  cultivar,
  sowing_date,
  harvest_date,
  co2_name = "MaunaLoa",
  irri_name = "rainfed",
  man_agro = "none",
  ini_cond = "none",
  version = "6.x"
)
```

## Arguments

- path_proj:

  Character. AquaCrop project directory where the `LIST` subdirectory
  and project file will be written.

- id_name:

  Character. Project identifier used as the `.PRM` file name.

- clim_name:

  Character. Base name of the AquaCrop climate file set, without
  extension.

- soil_name:

  Character. Base name of the AquaCrop soil `.SOL` file, without
  extension.

- cultivar:

  Character. Base name of the AquaCrop crop `.CRO` file, without
  extension.

- sowing_date:

  Date vector. Sowing date or dates used to build simulation runs.

- harvest_date:

  Date vector. Harvest date or dates used to build simulation runs.

- co2_name:

  Character. Base name of the AquaCrop CO2 file, without extension.
  Defaults to `"MaunaLoa"`.

- irri_name:

  Character. Irrigation file base name, or `"rainfed"` when no
  irrigation file should be linked.

- man_agro:

  Character. Management file base name, or `"none"` when no management
  file should be linked.

- ini_cond:

  Character. Initial-condition file base name, or `"none"` when no
  initial-condition file should be linked.

- version:

  Character. AquaCrop version label used by the caller; defaults to
  `"6.x"`.

## Value

Character vector with the path of the AquaCrop project file created.

## Examples

``` r
if (FALSE) { # \dontrun{
prm_file <- write_exp_aquacrop(
  path_proj = tempdir(),
  id_name = "AIHU_FED2000_MADRI_S1",
  clim_name = "AIHU",
  soil_name = "AIHU",
  cultivar = "F2000",
  sowing_date = as.Date("2020-04-01"),
  harvest_date = as.Date("2020-08-15")
)
prm_file
} # }
```
