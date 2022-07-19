context("test dep_build_varlist function")

# test inputs ------------------------------------------------

test_that("correctly specified functions execute without error", {
  expect_error(dep_build_varlist(geography = "county", index = "gini", year = 2019, survey = "acs5"), NA)
})
