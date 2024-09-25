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
