#### Values for kappas tests
tab22 <- matrix(data = NA, ncol = 2, nrow = 2)
tab22[, 1] <- sample(1:19, 2)
tab22[, 2] <- 20 - tab22[, 1]

tab33 <- matrix(data = NA, ncol = 3, nrow = 3)
tab33[, 1] <- sample(1:19, 3)
tab33[, 2] <- sample(1:19, 3)
tab33[, 3] <- 45 - rowSums(tab33[, 1:2])

#### Values for confusion metrics tests
x1 = sample(c(TRUE, FALSE), 100, replace = TRUE)
x2 = sample(c(TRUE, FALSE), 100, replace = TRUE)

#### Values for rates tests
tabdata <- tab::tabdata
pop_tot <- c(32648555, 32587453)
nb_ref <- c(61904, 59910)
MR_ref <- c(190, 183)
pop_obs <- c(300340, 301352)
nb_exp <- c(570, 551)
nb_obs <- c(200, 213)

#### Values for morbidities tests
morbid <- morbidity(gender = "M",
                    age = sample(50:90, 100, replace = TRUE),
                    isch.heart = sample(c(0,1), 100, replace = TRUE),
                    cerebro.vasc = sample(c(0,1), 100, replace = TRUE),
                    heart.fail = sample(c(0,1), 100, replace = TRUE),
                    periph.vasc = sample(c(0,1), 100, replace = TRUE),
                    diabetes = sample(c(0,1), 100, replace = TRUE),
                    cancer = sample(c(0,1), 100, replace = TRUE),
                    cancer.hist = sample(c(0,1), 100, replace = TRUE),
                    schizo = sample(c(0,1), 100, replace = TRUE),
                    depression = sample(c(0,1), 100, replace = TRUE),
                    subst.abuse = sample(c(0,1), 100, replace = TRUE),
                    dementia = sample(c(0,1), 100, replace = TRUE),
                    parkinson = sample(c(0,1), 100, replace = TRUE),
                    mult.sclero = sample(c(0,1), 100, replace = TRUE),
                    epilespy = sample(c(0,1), 100, replace = TRUE),
                    chronic.resp = sample(c(0,1), 100, replace = TRUE),
                    infl.bowel = sample(c(0,1), 100, replace = TRUE),
                    rheum.arthr = sample(c(0,1), 100, replace = TRUE),
                    HIV = sample(c(0,1), 100, replace = TRUE),
                    es.renal = sample(c(0,1), 100, replace = TRUE),
                    liver.pancr = sample(c(0,1), 100, replace = TRUE))
comorbid <- comorbidity(
  age = sample(50:90, 100, replace = TRUE),
  myocard.infarct = sample(c(0,1), 100, replace = TRUE),
  heart.fail = sample(c(0,1), 100, replace = TRUE),
  periph.vasc = sample(c(0,1), 100, replace = TRUE),
  cerebr = sample(c(0,1), 100, replace = TRUE),
  diabetes = sample(c(0,1), 100, replace = TRUE),
  dementia = sample(c(0,1), 100, replace = TRUE),
  chronic.pulm = sample(c(0,1), 100, replace = TRUE),
  connect.tissue = sample(c(0,1), 100, replace = TRUE),
  ulcer = sample(c(0,1), 100, replace = TRUE),
  mild.liver = sample(c(0,1), 100, replace = TRUE),
  hemiplegia = sample(c(0,1), 100, replace = TRUE),
  renal = sample(c(0,1), 100, replace = TRUE),
  diabetes.org.dmg = sample(c(0,1), 100, replace = TRUE),
  tumor = sample(c(0,1), 100, replace = TRUE),
  leukemia = sample(c(0,1), 100, replace = TRUE),
  lymphoma = sample(c(0,1), 100, replace = TRUE),
  mode.sev.liver = sample(c(0,1), 100, replace = TRUE),
  metast.tumor = sample(c(0,1), 100, replace = TRUE),
  aids = sample(c(0,1), 100, replace = TRUE)
)


#### Values for adherence
adher <- adherence(sample(1:100, 100),
          sample(1:100, 100),
          sample(1:100, 100),
          sample(1:100, 100),
          rep(NA, 10),
          sample(1:100, 100),
          sample(1:100, 100),
          sample(1:100, 100))
coc <- CoC(sample(1:100, 100),
           sample(1:100, 100),
           NA,
           sample(1:100, 100),
           sample(1:100, 100),
           sample(1:100, 100))
