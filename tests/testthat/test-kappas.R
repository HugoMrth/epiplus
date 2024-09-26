test_that("Args are check properly :",  {
  expect_error(kappa_cohen(NULL))
  expect_error(kappa_cohen(-tab22))
  expect_error(kappa_cohen(tab33))
  expect_error(kappa_fleiss(-tab22))
  expect_error(kappa_fleiss(matrix(c(1, 2, 3, 4), ncol = 2)))
})

test_that("Output are numerics :", {
  expect_type(kappa_cohen(tab22), "double")
  expect_type(kappa_fleiss(tab22), "double")
  expect_type(kappa_fleiss(tab33), "double")
})

test_that("Single output :", {
  expect_length(kappa_cohen(tab22), 1)
  expect_length(kappa_fleiss(tab22), 1)
  expect_length(kappa_fleiss(tab33), 1)
})

test_that("Bounded within -1 and 1 :", {
  for (i in 1:10) {
    expect_true(kappa_cohen(tab22) > -1)
    expect_true(kappa_cohen(tab22) < 1)
    expect_true(kappa_fleiss(tab22) < 1)
    expect_true(kappa_fleiss(tab33) < 1)
  }
})

test_that("Only works with numeric matrix :", {
  tab <- matrix(data = as.character(sample(1:100, 4)), ncol = 2)
  expect_error(kappa_cohen(tab))
  expect_error(kappa_fleiss(tab))
})

test_that("Does not work with vectors :", {
  expect_error(kappa_cohen(as.numeric(tab22)))
  expect_error(kappa_fleiss(as.numeric(tab22)))
  expect_error(kappa_fleiss(as.numeric(tab22)))
})

test_that("Matrix conversion ok :", {
  expect_no_error(kappa_cohen(as.data.frame(tab22)))
  expect_no_error(kappa_fleiss(as.data.frame(tab22)))
  expect_no_error(kappa_fleiss(as.data.frame(tab22)))
})

