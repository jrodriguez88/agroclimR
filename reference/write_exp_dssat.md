# Write a DSSAT experimental file (X file)

Creates a DSSAT experiment file from already prepared experiment
metadata, planting information, soil/weather links, irrigation flags and
fertilization inputs. This is the low-level writer used after experiment
data have been tidied into one row per DSSAT treatment setup.

## Usage

``` r
write_exp_dssat(
  path,
  id_name,
  crop,
  cultivar,
  soil,
  wth_station,
  planting_details,
  irri,
  fert_in,
  start_date,
  planting_date,
  emergence_date,
  treatments_number
)
```

## Arguments

- path:

  Character. Directory where the DSSAT experiment file will be written.

- id_name:

  Character vector used to build the DSSAT experiment name and output
  file name. The first element is the site or experiment identifier.

- crop:

  Character. DSSAT crop name, for example `"rice"`, `"maize"`,
  `"barley"`, `"sorghum"`, `"wheat"`, `"bean"`, `"fababean"`, or
  `"teff"`.

- cultivar:

  Character vector of DSSAT cultivar codes, one per treatment when
  multiple treatments are written.

- soil:

  Character vector of DSSAT soil profile identifiers.

- wth_station:

  Character vector of DSSAT weather station identifiers.

- planting_details:

  List or data frame with DSSAT planting details such as `PPOP`, `PPOE`,
  `PLME`, `PLDS`, `PLRS`, `PLRD`, and `PLDP`.

- irri:

  Logical. `TRUE` for irrigated experiments and `FALSE` for rainfed
  experiments.

- fert_in:

  Data frame or `NULL`. Fertilization schedule with DSSAT fields such as
  `FDATE`, `FMCD`, `FACD`, `FDEP`, `FAMN`, `FAMP`, `FAMK`, `FAMC`,
  `FAMO`, `FOCD`, and `FERNAME`.

- start_date:

  Date. Simulation start date.

- planting_date:

  Date vector. Planting date for each treatment.

- emergence_date:

  Date vector or `NULL`. Emergence date for each treatment.

- treatments_number:

  Integer. Number of DSSAT treatments to write.

## Value

Character vector with the path of the DSSAT experiment file created.

## Details

Use `tidy_exp_dssat()` to prepare the row-wise inputs consumed by this
writer.

## Examples

``` r
if (FALSE) { # \dontrun{
# Experimental tidy data
tidy_exp_data <- tidy_exp_dssat(agro, phenol, fertil, path = tempdir())

# Write DSSAT experimental files
exp_files_created <- tidy_exp_data %>%
  mutate(file = pmap_chr(., write_exp_dssat)) %>%
  pull(file)

exp_files_created
file.remove(exp_files_created)
} # }
```
