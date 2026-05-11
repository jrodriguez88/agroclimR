test_that("weather writers create model files", {
  out_dir <- file.path(tempdir(), "agroclimR-weather-writers")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  unlink(file.path(out_dir, "*"), recursive = TRUE)

  dssat_files <- suppressMessages(
    write_wth_dssat(out_dir, "WTDS", weather, lat = 3.91, lon = -75, elev = 450)
  )
  expect_true(all(file.exists(dssat_files)))
  expect_match(readLines(dssat_files[1], n = 1), "WEATHER DATA")

  aquacrop_files <- suppressMessages(
    write_wth_aquacrop(out_dir, "WTAQ", weather, lat = 3.8, lon = -76.5, elev = 650)
  )
  expect_length(aquacrop_files, 4)
  expect_true(all(file.exists(aquacrop_files)))
  expect_true(any(grepl("[.]CLI$", aquacrop_files)))

  oryza_files <- suppressMessages(
    write_wth_oryza(out_dir, "WTOR", weather, lat = 3.8, lon = -76.5, elev = 650)
  )
  expect_true(all(file.exists(oryza_files)))
  expect_match(readLines(oryza_files[1], n = 1), "-76.5,3.8,650")

  unlink(c(dssat_files, aquacrop_files, oryza_files))
})

test_that("soil writers create model files", {
  out_dir <- file.path(tempdir(), "agroclimR-soil-writers")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  unlink(file.path(out_dir, "*"), recursive = TRUE)
  on.exit({
    if (exists("idsoilAR", envir = .GlobalEnv)) {
      rm(idsoilAR, envir = .GlobalEnv)
    }
  }, add = TRUE)

  soil_sample <- dplyr::group_by(soil, NL) |>
    dplyr::sample_n(1) |>
    dplyr::ungroup()

  dssat_file <- suppressMessages(
    write_soil_dssat(out_dir, "SODS", soil_sample)
  )
  expect_true(file.exists(dssat_file))
  expect_match(readLines(dssat_file, n = 1), "SOILS")

  aquacrop_file <- suppressMessages(
    write_soil_aquacrop(out_dir, "SOAQ", soil_sample)
  )
  expect_true(file.exists(aquacrop_file))
  expect_match(readLines(aquacrop_file, n = 1), "AquaCrop soil file")

  oryza_file <- suppressMessages(
    write_soil_oryza(out_dir, "SOOR", soil_sample)
  )
  expect_true(file.exists(oryza_file))
  expect_match(readLines(oryza_file, n = 2)[2], "PADDY soil water balance")

  unlink(c(dssat_file, aquacrop_file, oryza_file))
})
