\title{
  (Standardized) Mortality Rate
}
\name{SMR}
\alias{SMR}
\alias{MR}
\alias{TI}
\description{
  Formules de calcul des taux de mortalite (MR) et taux de mortalite standardise (SMR). La calcul du SMR peut s'effetcue de plusieurs facons selon les informations disponibles. Les trois usages possibles sont renseignes ci-dessous.
}\usage{
  # Calcul du taux de mortalite/d'incidence
  # Les fonctiones MR() et TI() sont equivalentes.
  MR(n.obs, pop.ref,
     by = 100000,
     decimal = 1)

  #Calcul du taux de mortalite standardise
  SMR(n.obs = NULL, n.exp = NULL,
      n.ref = NULL, pop.obs = NULL, pop.ref = NULL,
      MR.ref = NULL,
      by = 100000, decimal = 1)

  # Combinaisons de parametres possibles
  # Pour le calcul du SMR
  SMR(n.obs, n.exp)
  SMR(n.obs, n.ref, pop.obs, pop.ref)
  SMR(n.obs, pop.obs, MR.ref)
}\arguments{
  \item{n.obs}{
    Le nombre de cas observe dans la population d'etude.
}
  \item{n.exp}{
    Le nombre de cas attendus.
}
  \item{n.ref}{
    Le nombre de cas observe dans la population totale/de reference.
}
  \item{pop.obs}{
    La taille de la population d'etude.
}
  \item{pop.ref}{
    La taille de la population totale/de reference.
}
  \item{MR.ref}{
    Le taux de mortalite observe dans la population totale/de reference.
}
  \item{by}{
    La tranche de population sur laquelle appliquer le taux. Par defaut 100.000.
}
  \item{decimal}{
    Le nombre de chiffre apres la virgule desires pour les calculs. Par defaut 1.
}
}\value{
  Un numérique ou un vecteur de numériques
}\author{
  Hugo Marthinet
}\examples{
# Tests du MR

pop_tot <- c(32648555, 32587453)
nb_ref <- c(61904, 59910)

MR(nb_ref, pop_tot)



# Tests du SMR

pop_tot <- c(32648555, 32587453)
nb_ref <- c(61904, 59910)
MR_ref <- c(190, 183)
pop_obs <- c(300340, 301352)
nb_exp <- c(570, 551)
nb_obs <- c(200, 213)

SMR(nb_obs, nb_exp, decimal = 3)
SMR(n.obs = nb_obs, n.ref = nb_ref, pop.obs = pop_obs, pop.ref = pop_tot, decimal = 3)
SMR(n.obs = nb_obs, pop.obs = pop_obs, MR.ref = MR_ref, decimal = 3)
}
