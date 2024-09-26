test_that("Args are checked properly :", {
  expect_error(standardizedDifference(mean.treatment = 2))
  expect_error(standardizedDifference(mean.treatment = 2, sd.treatment = 0.5))
  expect_error(standardizedDifference(prop.treatment = 2))
  expect_error(standardizedDifference(mean.treatment = 2, prop.treatment = 0.5))
})

test_that("Output types are ok :", {
  expect_type(standardizedDifference(mean.treatment = 64, sd.treatment = 2.3, mean.control = 61.7, sd.control = 3.7), "list")
  expect_type(standardizedDifference(mean.treatment = 64, sd.treatment = 7.1, mean.control = 63.7, sd.control = 9.2), "list")
  expect_type(standardizedDifference(prop.treatment = 0.80, prop.control = 0.82), "list")
  expect_type(standardizedDifference(mean.treatment = 64, sd.treatment = 2.3, mean.control = 61.7, sd.control = 3.7)$d, "double")
  expect_type(standardizedDifference(mean.treatment = 64, sd.treatment = 7.1, mean.control = 63.7, sd.control = 9.2)$d, "double")
  expect_type(standardizedDifference(prop.treatment = 0.80, prop.control = 0.82)$d, "double")
  expect_type(standardizedDifference(mean.treatment = 64, sd.treatment = 2.3, mean.control = 61.7, sd.control = 3.7)$diagnostic, "character")
  expect_type(standardizedDifference(mean.treatment = 64, sd.treatment = 7.1, mean.control = 63.7, sd.control = 9.2)$diagnostic, "character")
  expect_type(standardizedDifference(prop.treatment = 0.80, prop.control = 0.82)$diagnostic, "character")
})

test_that("Lists have 2 elements :", {
  expect_length(standardizedDifference(mean.treatment = 64, sd.treatment = 2.3, mean.control = 61.7, sd.control = 3.7), 2)
  expect_length(standardizedDifference(mean.treatment = 64, sd.treatment = 7.1, mean.control = 63.7, sd.control = 9.2), 2)
  expect_length(standardizedDifference(prop.treatment = 0.80, prop.control = 0.82), 2)
})
