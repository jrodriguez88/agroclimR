test_that("get_STC classifies USDA textures without optional GUI dependencies", {
  expect_equal(get_STC(39.2144, 15.15228), "Lo")
  expect_equal(get_STC(70.87598, 11.56432), "SaLo")
  expect_equal(get_STC(28.27207, 15.72915), "SiLo")
})

test_that("get_STC validates input lengths", {
  expect_error(get_STC(c(30, 40), 20), "length")
})
