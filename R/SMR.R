SMR <- function(n.obs = NULL, n.exp = NULL,
               n.ref = NULL, pop.obs = NULL, pop.ref = NULL,
               MR.ref = NULL,
               by = 100000, decimal = 1) {

  if(is.null(n.obs)){
    stop("Vous devez obligatoirement renseigner le nombre de cas observes.")
  }

  if (is.null(n.exp) & is.null(pop.obs)) {
    stop("Si le nombre de cas attendu n'est pas renseigne, la taille de la population d'etude doit obligatoirement l'etre.")
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

  stop("Les parametres renseignes ne permettent pas de calculer le SMR. cf. doc pour les trois possibilites d'utilisation")
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
