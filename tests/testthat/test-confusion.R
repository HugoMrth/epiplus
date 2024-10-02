test_that("Args are check properly :",  {
  expect_error(confusionMetrics(
    x.true = x1,
    x.test = sample(c(TRUE, FALSE), 10, replace = TRUE)
  ))
  expect_error(confusionMetrics(
    x.true = x1,
    x.test = sample(c(0, 1, 2), 100, replace = TRUE)
  ))
  expect_error(confusionMetrics(
    x.test = x2,
    x.true = sample(c(0, 1, 2), 100, replace = TRUE)
  ))
  expect_error(confusionMetrics(
    tab = table(
      sample(c(TRUE, FALSE), 100, replace = TRUE),
      sample(c(0, 1, 2), 100, replace = TRUE))
  ))
  expect_error(confusionMetrics(
    x.true = sample(c("red", "blue"), 100, replace = TRUE),
    x.test = sample(c("red", "blue"), 100, replace = TRUE)
  ))
  expect_error(confusionMetrics())
})


test_that("Output is a list :", {
  expect_type(confusionMetrics(
    x.true = x1,
    x.test = x2
  ), "list")
})

test_that("List has 3 elements :", {
  expect_length(confusionMetrics(
    x.true = x1,
    x.test = x2
  ), 6)
  expect_true(all(names(
    confusionMetrics(
      x.true = x1,
      x.test = x2
    )
  ) == c("measures", "PPV", "NPV", "OR", "RR", "IRR")))
})


test_that("All elements are numerical vectors :", {
  expect_type(unlist(confusionMetrics(
    x.true = x1,
    x.test = x2
  )$measures), 'double')
  expect_type(unlist(confusionMetrics(
    x.true = x1,
    x.test = x2
  )$PPV), 'double')
  expect_type(unlist(confusionMetrics(
    x.true = x1,
    x.test = x2
  )$NPV), 'double')
  expect_type(unlist(confusionMetrics(
    x.true = x1,
    x.test = x2
  )$OR), 'double')
  expect_type(unlist(confusionMetrics(
    x.true = x1,
    x.test = x2
  )$RR), 'double')
  expect_type(unlist(confusionMetrics(
    x.true = x1,
    x.test = x2
  )$IRR), 'double')
})

test_that("Values are coherent :", {
  for (i in 1:10) {
    x1 <- sample(c(TRUE, FALSE), 100, replace = TRUE)
    x2 <- sample(c(TRUE, FALSE), 100, replace = TRUE)

    expect_true(all(unlist(confusionMetrics(x.true = x1, x.test = x2)$measures) >= 0))
    expect_true(all(unlist(confusionMetrics(x.true = x1, x.test = x2)$measures) <= 1))
    expect_true(all(unlist(confusionMetrics(x.true = x1, x.test = x2)$PPV) >= 0))
    expect_true(all(unlist(confusionMetrics(x.true = x1, x.test = x2)$PPV) <= 1))
    expect_true(all(unlist(confusionMetrics(x.true = x1, x.test = x2)$NPV) >= 0))
    expect_true(all(unlist(confusionMetrics(x.true = x1, x.test = x2)$NPV) <= 1))
    expect_true(all(unlist(confusionMetrics(x.true = x1, x.test = x2)$OR) >= 0))
    expect_true(all(unlist(confusionMetrics(x.true = x1, x.test = x2)$RR) >= 0))
    expect_true(all(unlist(confusionMetrics(x.true = x1, x.test = x2)$IRR) >= -1))
    expect_true(all(unlist(confusionMetrics(x.true = x1, x.test = x2)$IRR) <= 1))
    expect_true(unlist(confusionMetrics(x.true = x1, x.test = x2)$OR)["pval"] <= 1)
    expect_true(unlist(confusionMetrics(x.true = x1, x.test = x2)$RR)["pval"] <= 1)
  }
})
