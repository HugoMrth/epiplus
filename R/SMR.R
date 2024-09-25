SMR <- function(n.obs = NULL, n.exp = NULL,
               n.ref = NULL, pop.obs = NULL, pop.ref = NULL,
               MR.ref = NULL,
               by = 100000, decimal = 1) {

  if(is.null(n.obs)) stop("n.obs is missing")
  if (is.null(n.exp) & is.null(pop.obs)) stop("If n.exp is missing, pop.obs must be provided")
  if (!is.null(MR.ref)) {
    if (any(MR.ref < 0)) stop ("A mortality rate cannot be negative")
  }
  if (!is.null(pop.ref) & !is.null(n.ref)) {
    if (any(pop.ref < n.ref)) stop("Number of observations cannot be greater than the total population")
  }
  if (!is.null(pop.obs) & !is.null(n.obs)) {
    if (any(pop.obs < n.obs)) stop("Number of observations cannot be greater than the total population")
  }

  if (!is.null(n.obs) & !is.null(n.exp)) {
    SMR <- n.obs/n.exp
    return(round(SMR, decimal))
  }

  if (!is.null(n.obs) & !is.null(n.ref) & !is.null(pop.obs) & !is.null(pop.ref)) {
    SMR <- n.obs / (n.ref * pop.obs / pop.ref)
    return(round(SMR, decimal))
  }

  if (!is.null(n.obs) & !is.null(pop.obs) & !is.null(MR.ref)) {
    SMR <- n.obs / (MR.ref * pop.obs / by)
    return(round(SMR, decimal))
  }

  stop("The provided parameters do not allow SMR calculation. Please use ?SMR")
}
