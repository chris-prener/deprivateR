context("test get_deprivation function")

library(sf)

# test inputs ------------------------------------------------

test_that("correctly specified functions execute without error", {
  expect_error(get_deprivation(geography = "county", variables = "gini", output = "tidy", state = "MO", year = 2019, debug = TRUE), NA)
})

result1 <- get_deprivation(geography = "county", variables = "gini", output = "tidy", state = "MO", year = 2019, debug = TRUE)
result2 <- get_deprivation(geography = "county", variables = "gini", state = "MO", year = 2019, output = "wide", geometry = TRUE, keep_geo_vars = TRUE, units = "mi", debug = TRUE)

test_that("correctly specified functions execute without error", {
  expect_s3_class(result1, "tbl_df")
  expect_s3_class(result2, "sf")
})
