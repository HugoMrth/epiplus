test_that("Args are checked properly :", {
  expect_error(adherence(n.days.supplied = "a"))
  expect_error(adherence(n.days.supplied = -1))
  expect_error(adherence(n.days.gaps = "a"))
  expect_error(adherence(n.days.gaps = -1))
  expect_error(adherence(n.days.period = "a"))
  expect_error(adherence(n.days.period = -1))
  expect_error(adherence(n.days.last.supply = "a"))
  expect_error(adherence(n.days.last.supply = -1))
  expect_error(adherence(surplus = "a"))
  expect_error(adherence(surplus = -1))
  expect_error(adherence(diff.first.last.supply = "a"))
  expect_error(adherence(diff.first.last.supply = -1))
  expect_error(adherence(quantity.supplied = "a"))
  expect_error(adherence(quantity.supplied = -1))
  expect_error(adherence(quantity.per.day = "a"))
  expect_error(adherence(quantity.per.day = -1))

  expect_error(CoC(-1, 1, 1, 1, 1, 1))
  expect_error(CoC(1, -1, 1, 1, 1, 1))
  expect_error(CoC(1, 1, 1, -1, 1, 1))
  expect_error(CoC(1, 1, 1, 1, -1, 1))
  expect_error(CoC(1, 1, 1, 1, 1, -1))
  expect_error(CoC("a", 1, 1, 1, 1, 1))
  expect_error(CoC(1, "a", 1, 1, 1, 1))
  expect_error(CoC(1, 1, 1, "a", 1, 1))
  expect_error(CoC(1, 1, 1, 1, "a", 1))
  expect_error(CoC(1, 1, 1, 1, 1, "a"))
})

test_that("Output type are ok :", {
  expect_type(adherence(1, 1, 1, 1, 1, 1, 1, 1), "list")
  expect_type(unlist(adherence(1, 1, 1, 1, 1, 1, 1, 1)), "double")
  expect_type(CoC(1, 1, 1, 1, 1, 1), "list")
  expect_type(unlist(CoC(1, 1, 1, 1, 1, 1)), "double")
})

test_that("Correct output format : ", {
  expect_length(adherence(), 11)
  expect_length(CoC(), 12)
})

test_that("Values are coherent", {
  expect_true(sum(is.na(adherence())) == 11)
  expect_true(sum(is.na(CoC())) == 12)
  expect_true(sum(is.na(adherence(1, 1, 1, 1, 1, 1, 1, 1))) == 0)

  expect_true(all(adher$PDC >= 0))
  expect_true(all(adher$RCR >= 0))
  expect_true(all(adher$CMA >= 0))
  expect_true(all(adher$CMG >= 0))
  expect_true(all(adher$CMOS >= 0))
  expect_true(all(adher$MPR >= 0))
  expect_true(all(adher$MPRm >= 0))
  expect_true(all(adher$MRA >= 0))
  expect_true(all(adher$CSA >= 0))


  expect_true(all(coc$CPC >= 0))
  expect_true(all(coc$UPC >= 0))
  expect_true(all(coc$HCC %in% c(0, 1)))
  expect_true(all(coc$HSC %in% c(0, 1)))
  expect_true(all(is.na(coc$HI)))
  expect_true(all(coc$CI %in% c(0, 0.5, 1)))
  expect_true(all(is.na(coc$COC)))
  expect_true(all(coc$PPC %in% c(0, 1)))
  expect_true(all(coc$INOP >= 0))
})



