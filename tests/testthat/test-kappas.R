test_that("Args are check properly :",  {
  expect_error(cohenKappa(NULL))
  expect_error(cohenKappa(-tab22))
  expect_error(cohenKappa(tab33))
  expect_error(yuleQ(NULL))
  expect_error(yuleQ(-tab22))
  expect_error(yuleQ(tab33))
  expect_error(fleissKappa(-tab22))
  expect_error(fleissKappa(matrix(c(1, 2, 3, 4), ncol = 2)))
})

test_that("Output are numerics :", {
  expect_type(cohenKappa(tab22), "double")
  expect_type(yuleQ(tab22), "double")
  expect_type(fleissKappa(tab22), "double")
  expect_type(fleissKappa(tab33), "double")
})

test_that("Single output :", {
  expect_length(cohenKappa(tab22), 1)
  expect_length(yuleQ(tab22), 1)
  expect_length(fleissKappa(tab22), 1)
  expect_length(fleissKappa(tab33), 1)
})

test_that("Bounded within -1 and 1 :", {
  for (i in 1:10) {
    expect_true(cohenKappa(tab22) > -1)
    expect_true(cohenKappa(tab22) < 1)
    expect_true(yuleQ(tab22) > -1)
    expect_true(yuleQ(tab22) < 1)
    expect_true(fleissKappa(tab22) < 1)
    expect_true(fleissKappa(tab33) < 1)
  }
})

test_that("Only works with numeric matrix :", {
  tab <- matrix(data = as.character(sample(1:100, 4)), ncol = 2)
  expect_error(cohenKappa(tab))
  expect_error(yuleQ(tab))
  expect_error(fleissKappa(tab))
})

test_that("Does not work with vectors :", {
  expect_error(cohenKappa(as.numeric(tab22)))
  expect_error(yuleQ(as.numeric(tab22)))
  expect_error(fleissKappa(as.numeric(tab22)))
  expect_error(fleissKappa(as.numeric(tab22)))
})

test_that("Matrix conversion ok :", {
  expect_no_error(cohenKappa(as.data.frame(tab22)))
  expect_no_error(yuleQ(as.data.frame(tab22)))
  expect_no_error(fleissKappa(as.data.frame(tab22)))
  expect_no_error(fleissKappa(as.data.frame(tab22)))
})

