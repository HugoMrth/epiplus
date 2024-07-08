SMR <- function(n.obs = NULL, n.exp = NULL,
               n.ref = NULL, pop.obs = NULL, pop.ref = NULL,
               MR.ref = NULL,
               by = 100000, decimal = 1) {

  if(is.null(n.obs)){
    stop("n.obs is missing")
  }

  if (is.null(n.exp) & is.null(pop.obs)) {
    stop("If n.exp is missing, pop.obs must be provided")
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


# pop_tot <- c(32648555, 32587453)
# nb_ref <- c(61904, 59910)
# MR_ref <- c(190, 183)
# pop_obs <- c(300340, 301352)
# nb_exp <- c(570, 551)
# nb_obs <- c(200, 213)
#
# SMR(nb_obs, nb_exp, decimal = 3)
# SMR(n.obs = nb_obs, n.ref = nb_ref, pop.obs = pop_obs, pop.ref = pop_tot, decimal = 3)
# SMR(n.obs = nb_obs, pop.obs = pop_obs, MR.ref = MR_ref, decimal = 3)
