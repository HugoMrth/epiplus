test_that("Args are checked properly :", {
  expect_error(morbidity())
  expect_error(comorbidity())
  expect_error(
    morbidity(
      gender = "H",
      age = sample(50:90, 100, replace = TRUE),
      isch.heart = sample(c(0, 1), 100, replace = TRUE),
      cerebro.vasc = sample(c(0, 1), 100, replace = TRUE),
      heart.fail = sample(c(0, 1), 100, replace = TRUE),
      periph.vasc = sample(c(0, 1), 100, replace = TRUE),
      diabetes = sample(c(0, 1), 100, replace = TRUE),
      cancer = sample(c(0, 1), 100, replace = TRUE),
      cancer.hist = sample(c(0, 1), 100, replace = TRUE),
      schizo = sample(c(0, 1), 100, replace = TRUE),
      depression = sample(c(0, 1), 100, replace = TRUE),
      subst.abuse = sample(c(0, 1), 100, replace = TRUE),
      dementia = sample(c(0, 1), 100, replace = TRUE),
      parkinson = sample(c(0, 1), 100, replace = TRUE),
      mult.sclero = sample(c(0, 1), 100, replace = TRUE),
      epilespy = sample(c(0, 1), 100, replace = TRUE),
      chronic.resp = sample(c(0, 1), 100, replace = TRUE),
      infl.bowel = sample(c(0, 1), 100, replace = TRUE),
      rheum.arthr = sample(c(0, 1), 100, replace = TRUE),
      HIV = sample(c(0, 1), 100, replace = TRUE),
      es.renal = sample(c(0, 1), 100, replace = TRUE),
      liver.pancr = sample(c(0, 1), 100, replace = TRUE)
    )
  )
  expect_error(
    morbidity(
      gender = "M",
      age = as.character(sample(50:90, 100, replace = TRUE)),
      isch.heart = sample(c(0, 1), 100, replace = TRUE),
      cerebro.vasc = sample(c(0, 1), 100, replace = TRUE),
      heart.fail = sample(c(0, 1), 100, replace = TRUE),
      periph.vasc = sample(c(0, 1), 100, replace = TRUE),
      diabetes = sample(c(0, 1), 100, replace = TRUE),
      cancer = sample(c(0, 1), 100, replace = TRUE),
      cancer.hist = sample(c(0, 1), 100, replace = TRUE),
      schizo = sample(c(0, 1), 100, replace = TRUE),
      depression = sample(c(0, 1), 100, replace = TRUE),
      subst.abuse = sample(c(0, 1), 100, replace = TRUE),
      dementia = sample(c(0, 1), 100, replace = TRUE),
      parkinson = sample(c(0, 1), 100, replace = TRUE),
      mult.sclero = sample(c(0, 1), 100, replace = TRUE),
      epilespy = sample(c(0, 1), 100, replace = TRUE),
      chronic.resp = sample(c(0, 1), 100, replace = TRUE),
      infl.bowel = sample(c(0, 1), 100, replace = TRUE),
      rheum.arthr = sample(c(0, 1), 100, replace = TRUE),
      HIV = sample(c(0, 1), 100, replace = TRUE),
      es.renal = sample(c(0, 1), 100, replace = TRUE),
      liver.pancr = sample(c(0, 1), 100, replace = TRUE)
    )
  )
  expect_error(
    comorbidity(
      age = as.character(sample(50:90, 100, replace = TRUE)),
      myocard.infarct = sample(c(0, 1), 100, replace = TRUE),
      heart.fail = sample(c(0, 1), 100, replace = TRUE),
      periph.vasc = sample(c(0, 1), 100, replace = TRUE),
      cerebr = sample(c(0, 1), 100, replace = TRUE),
      diabetes = sample(c(0, 1), 100, replace = TRUE),
      dementia = sample(c(0, 1), 100, replace = TRUE),
      chronic.pulm = sample(c(0, 1), 100, replace = TRUE),
      connect.tissue = sample(c(0, 1), 100, replace = TRUE),
      ulcer = sample(c(0, 1), 100, replace = TRUE),
      mild.liver = sample(c(0, 1), 100, replace = TRUE),
      hemiplegia = sample(c(0, 1), 100, replace = TRUE),
      renal = sample(c(0, 1), 100, replace = TRUE),
      diabetes.org.dmg = sample(c(0, 1), 100, replace = TRUE),
      tumor = sample(c(0, 1), 100, replace = TRUE),
      leukemia = sample(c(0, 1), 100, replace = TRUE),
      lymphoma = sample(c(0, 1), 100, replace = TRUE),
      mode.sev.liver = sample(c(0, 1), 100, replace = TRUE),
      metast.tumor = sample(c(0, 1), 100, replace = TRUE),
      aids = sample(c(0, 1), 100, replace = TRUE)
    )
  )
  expect_error(
    morbidity(
      gender = "M",
      age = sample(50:90, 100, replace = TRUE),
      isch.heart = sample(c(0, 2), 100, replace = TRUE),
      cerebro.vasc = sample(c(0, 1), 100, replace = TRUE),
      heart.fail = sample(c(0, 1), 100, replace = TRUE),
      periph.vasc = sample(c(0, 1), 100, replace = TRUE),
      diabetes = sample(c(0, 1), 100, replace = TRUE),
      cancer = sample(c(0, 1), 100, replace = TRUE),
      cancer.hist = sample(c(0, 1), 100, replace = TRUE),
      schizo = sample(c(0, 1), 100, replace = TRUE),
      depression = sample(c(0, 1), 100, replace = TRUE),
      subst.abuse = sample(c(0, 1), 100, replace = TRUE),
      dementia = sample(c(0, 1), 100, replace = TRUE),
      parkinson = sample(c(0, 1), 100, replace = TRUE),
      mult.sclero = sample(c(0, 1), 100, replace = TRUE),
      epilespy = sample(c(0, 1), 100, replace = TRUE),
      chronic.resp = sample(c(0, 1), 100, replace = TRUE),
      infl.bowel = sample(c(0, 1), 100, replace = TRUE),
      rheum.arthr = sample(c(0, 1), 100, replace = TRUE),
      HIV = sample(c(0, 1), 100, replace = TRUE),
      es.renal = sample(c(0, 1), 100, replace = TRUE),
      liver.pancr = sample(c(0, 1), 100, replace = TRUE)
    )
  )
  expect_error(
    comorbidity(
      age = sample(50:90, 100, replace = TRUE),
      myocard.infarct = sample(c(0, 2), 100, replace = TRUE),
      heart.fail = sample(c(0, 1), 100, replace = TRUE),
      periph.vasc = sample(c(0, 1), 100, replace = TRUE),
      cerebr = sample(c(0, 1), 100, replace = TRUE),
      diabetes = sample(c(0, 1), 100, replace = TRUE),
      dementia = sample(c(0, 1), 100, replace = TRUE),
      chronic.pulm = sample(c(0, 1), 100, replace = TRUE),
      connect.tissue = sample(c(0, 1), 100, replace = TRUE),
      ulcer = sample(c(0, 1), 100, replace = TRUE),
      mild.liver = sample(c(0, 1), 100, replace = TRUE),
      hemiplegia = sample(c(0, 1), 100, replace = TRUE),
      renal = sample(c(0, 1), 100, replace = TRUE),
      diabetes.org.dmg = sample(c(0, 1), 100, replace = TRUE),
      tumor = sample(c(0, 1), 100, replace = TRUE),
      leukemia = sample(c(0, 1), 100, replace = TRUE),
      lymphoma = sample(c(0, 1), 100, replace = TRUE),
      mode.sev.liver = sample(c(0, 1), 100, replace = TRUE),
      metast.tumor = sample(c(0, 1), 100, replace = TRUE),
      aids = sample(c(0, 1), 100, replace = TRUE)
    )
  )
  expect_error(
    comorbidity(
      age = sample(50:90, 100, replace = TRUE),
      myocard.infarct = NULL,
      heart.fail = sample(c(0, 1), 100, replace = TRUE),
      periph.vasc = sample(c(0, 1), 100, replace = TRUE),
      cerebr = sample(c(0, 1), 100, replace = TRUE),
      diabetes = sample(c(0, 1), 100, replace = TRUE),
      dementia = sample(c(0, 1), 100, replace = TRUE),
      chronic.pulm = sample(c(0, 1), 100, replace = TRUE),
      connect.tissue = sample(c(0, 1), 100, replace = TRUE),
      ulcer = sample(c(0, 1), 100, replace = TRUE),
      mild.liver = sample(c(0, 1), 100, replace = TRUE),
      hemiplegia = sample(c(0, 1), 100, replace = TRUE),
      renal = sample(c(0, 1), 100, replace = TRUE),
      diabetes.org.dmg = sample(c(0, 1), 100, replace = TRUE),
      tumor = sample(c(0, 1), 100, replace = TRUE),
      leukemia = sample(c(0, 1), 100, replace = TRUE),
      lymphoma = sample(c(0, 1), 100, replace = TRUE),
      mode.sev.liver = sample(c(0, 1), 100, replace = TRUE),
      metast.tumor = sample(c(0, 1), 100, replace = TRUE),
      aids = sample(c(0, 1), 100, replace = TRUE)
    )
  )
  expect_error(
    morbidity(
      gender = "M",
      age = sample(50:90, 100, replace = TRUE),
      isch.heart = NULL,
      cerebro.vasc = sample(c(0, 1), 100, replace = TRUE),
      heart.fail = sample(c(0, 1), 100, replace = TRUE),
      periph.vasc = sample(c(0, 1), 100, replace = TRUE),
      diabetes = sample(c(0, 1), 100, replace = TRUE),
      cancer = sample(c(0, 1), 100, replace = TRUE),
      cancer.hist = sample(c(0, 1), 100, replace = TRUE),
      schizo = sample(c(0, 1), 100, replace = TRUE),
      depression = sample(c(0, 1), 100, replace = TRUE),
      subst.abuse = sample(c(0, 1), 100, replace = TRUE),
      dementia = sample(c(0, 1), 100, replace = TRUE),
      parkinson = sample(c(0, 1), 100, replace = TRUE),
      mult.sclero = sample(c(0, 1), 100, replace = TRUE),
      epilespy = sample(c(0, 1), 100, replace = TRUE),
      chronic.resp = sample(c(0, 1), 100, replace = TRUE),
      infl.bowel = sample(c(0, 1), 100, replace = TRUE),
      rheum.arthr = sample(c(0, 1), 100, replace = TRUE),
      HIV = sample(c(0, 1), 100, replace = TRUE),
      es.renal = sample(c(0, 1), 100, replace = TRUE),
      liver.pancr = sample(c(0, 1), 100, replace = TRUE)
    )
  )
})

test_that("Outputs type are ok :", {
  expect_type(morbid, "list")
  expect_type(morbid$MRMI, "double")
  expect_type(morbid$ERMI, "double")
  expect_type(comorbid, "double")
})

test_that("Lists have 2 elements :", {
  expect_length(morbid, 2)
})

test_that("Values are coherent :", {
  expect_true(all(morbid$MRMI >= 0))
  expect_true(all(morbid$ERMI >= 0))
  expect_true(all(comorbid >= 0))
})
