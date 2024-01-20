MR <- function(n.obs, pop.ref,
               by = 100000,
               decimal = 1) {

  res <- round(epi.conf(cbind(nb_ref , pop_tot), ctype = "inc.rate", method = "exact") * 100000, decimal)

  return(list(MR = res[, 1],
              CI = res[, -1]))
}

# pop_tot <- c(32648555, 32587453)
# nb_ref <- c(61904, 59910)
#
# MR(nb_ref, pop_tot)

