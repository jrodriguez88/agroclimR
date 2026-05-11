# Write an ORYZA v3 experimental file

Creates an ORYZA v3 experimental (`.exp`) file from prepared agronomic,
fertilization, phenology and plant observation inputs. This is the
low-level writer used after experiment tables have been tidied into one
row per experiment.

## Usage

``` r
write_exp_oryza(
  exp_file,
  LOC_ID,
  CULTIVAR,
  PDAT,
  ESTAB,
  SBDUR,
  NPLDS,
  CROP_SYS,
  TRDAT,
  FERT_obs,
  PHEN_obs,
  PLANT_obs,
  ET_mod = "PRIESTLY TAYLOR"
)
```

## Arguments

- exp_file:

  Character. Full path to the ORYZA `.exp` file to create.

- LOC_ID:

  Character. Location or experiment identifier.

- CULTIVAR:

  Character. ORYZA cultivar name or code.

- PDAT:

  Date. Planting date.

- ESTAB:

  Character or numeric. Crop establishment method code used by ORYZA.

- SBDUR:

  Numeric. Seedbed duration.

- NPLDS:

  Numeric. Number of plants or seedlings, as expected by the ORYZA
  template.

- CROP_SYS:

  Character. Crop system descriptor written in the experimental file.

- TRDAT:

  Date or `NA`. Transplanting date when applicable.

- FERT_obs:

  Data frame with fertilization observations for the experiment,
  typically including day/date and nitrogen amount columns.

- PHEN_obs:

  Data frame with phenology observations for the experiment.

- PLANT_obs:

  Data frame with plant growth observations for the experiment.

- ET_mod:

  Character. Evapotranspiration method. Supported template labels
  include `"PENMAN"`, `"PRIESTLY TAYLOR"`, and `"MAKKINK"`.

## Value

Character vector with the path of the ORYZA experimental file created.

## Details

Use `tidy_exp_oryza()` to join raw agronomic, phenology, plant,
fertilization and yield tables into the row-wise inputs consumed by this
writer.

## Examples

``` r
if (FALSE) { # \dontrun{
# Experimental tidy data
tidy_exp_data <- tidy_exp_oryza(
  agro, phenol, plant, fertil, yield, path = tempdir())

# Write ORYZA experimental files
exp_files_created <- tidy_exp_data %>%
  mutate(file = pmap(., write_exp_oryza)) %>%
  pull(exp_file)

exp_files_created
file.remove(exp_files_created)
} # }
```
