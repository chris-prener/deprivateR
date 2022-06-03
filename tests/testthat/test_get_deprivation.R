context("test get_deprivation function")

# test inputs ------------------------------------------------

test_that("correctly specified functions execute without error", {
  expect_error(get_deprivation(geography = "state", variables = "gini", state = "29", year = 2019), NA)
})

result1 <- get_deprivation(geography = "state", variables = "gini", year = 2019, state = "29")
result2 <- get_deprivation(geography = "state", variables = "gini", year = 2019, output = "wide", state = "29", geometry = TRUE, keep_geo_vars = TRUE, units = "mi")

test_that("correctly specified functions execute without error", {
  expect_s3_class(result1, "tbl_df")
  expect_s3_class(result2, "sf")
})
