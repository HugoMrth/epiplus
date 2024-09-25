test_that("Args are checked properly :", {
  expect_error(IR(pop_tot, nb_ref))
  expect_error(MR(pop_tot, nb_ref))
  expect_error(SMR(n.exp = 1, pop.obs = 2))
  expect_error(SMR(pop.obs = 1, n.obs = 2))
  expect_error(SMR(n.obs = pop_tot, n.ref = nb_ref, pop.obs = nb_ref, pop.ref = pop_tot))
  expect_error(SMR(n.obs = nb_ref, n.ref = pop_tot, pop.obs = pop_tot, pop.ref = nb_ref))
  expect_error(SMR(n.obs = nb_obs, pop.obs = pop_obs, MR.ref = -MR_ref, decimal = 3))
})

test_that("Output types are ok :", {
  expect_type(IR(nb_ref, pop_tot), "list")
  expect_type(MR(nb_ref, pop_tot), "list")

  expect_true(all(is.numeric(unlist(IR(nb_ref, pop_tot)$IR))))
  expect_true(all(is.numeric(unlist(IR(nb_ref, pop_tot)$CI))))
  expect_true(all(is.numeric(unlist(MR(nb_ref, pop_tot)$MR))))
  expect_true(all(is.numeric(unlist(MR(nb_ref, pop_tot)$CI))))

  expect_type(SMR(nb_obs, nb_exp, decimal = 3), "double")
  expect_type(SMR(n.obs = nb_obs, n.ref = nb_ref, pop.obs = pop_obs, pop.ref = pop_tot, decimal = 3), "double")
  expect_type(SMR(n.obs = nb_obs, pop.obs = pop_obs, MR.ref = MR_ref, decimal = 3), "double")

  expect_type(SIR(tabdata, value = "Sex", ref.value = "Female", loc = "Race", ref.loc = "White"), "double")
  expect_type(SIR(tabdata, value = "Sex", loc = "Race"), "double")
})


test_that("Lists have 2 elements :", {
  expect_length(IR(nb_ref, pop_tot), 2)
  expect_length(MR(nb_ref, pop_tot), 2)
  expect_true(all(names(IR(nb_ref, pop_tot)) == c("IR", "CI")))
  expect_true(all(names(MR(nb_ref, pop_tot)) == c("MR", "CI")))
})

test_that("Values are coherent :", {
  for (i in 1:10) {
    pop <- sample(10000:100000, 1)
    nb <- floor(pop*runif(1))
    expect_true(unlist(IR(nb, pop)$IR) >= 0)
    expect_true(unlist(IR(nb, pop)$CI["lower"]) >= 0)
    expect_true(unlist(IR(nb, pop)$CI["upper"]) >= 0)
    expect_true(unlist(MR(nb, pop)$MR) >= 0)
    expect_true(unlist(MR(nb, pop)$CI["lower"]) >= 0)
    expect_true(unlist(MR(nb, pop)$CI["upper"]) >= 0)
    expect_true(unlist(IR(nb, pop)$IR) == unlist(MR(nb, pop)$MR))
    expect_true(unlist(IR(nb, pop)$CI["lower"]) == unlist(MR(nb, pop)$CI["lower"]))
    expect_true(unlist(IR(nb, pop)$CI["upper"]) == unlist(MR(nb, pop)$CI["upper"]))
    expect_true(SMR(nb, pop) >= 0)
    expect_true(SMR(pop, nb) >= 0)
  }
  expect_true(SIR(tabdata, value = "Sex", ref.value = "Female", loc = "Race", ref.loc = "White") >= 0)
  expect_true(all(SIR(tabdata, value = "Sex", loc = "Race") >= 0))
})

