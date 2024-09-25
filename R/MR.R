MR <- function(n.death, pop.ref,
               by = 100000,
               decimal = 1) {

  if (any(pop.ref < n.death)) stop("Number of observations cannot be greater than the total population")

  res <- round(epi.conf(cbind(n.death , pop.ref), ctype = "inc.rate", method = "exact") * 100000, decimal)

  return(list(MR = res[, 1],
              CI = res[, -1]))
}

