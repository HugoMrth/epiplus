IR <- function(n.obs, pop.ref,
               by = 100000,
               decimal = 1) {

  if (any(pop.ref < n.obs)) stop("Number of observations cannot be greater than the total population")

  res <- round(epi.conf(cbind(n.obs , pop.ref), ctype = "inc.rate", method = "exact") * 100000, decimal)

  return(list(IR = res[, 1],
              CI = res[, -1]))
}

