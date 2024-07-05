\title{
  (Standardized) Mortality Rate
}
\name{SMR}
\alias{SMR}
\alias{MR}
\alias{IR}
\description{
Formulas for Mortality Rate (MR) and Standardized Mortality Rate (SMR) calculation.
SMR calculation can be done in different ways depending on the available informations (all three possibilities are available here).
}\usage{
  # Mortality/Incidence Rate
  # MR and IR are equivalent 
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
    Number of observed cases in the studied population
}
  \item{n.exp}{
    Number of expected cases
}
  \item{n.ref}{
    Number of observed cases in the reference population
}
  \item{pop.obs}{
    Size of the studied population
}
  \item{pop.ref}{
Size of the reference population
}
  \item{MR.ref}{
Mortality rate in the reference population
}
  \item{by}{
Denominator to use for the rate calculation. Default is 100 000.
}
  \item{decimal}{
Number of decimals to compute. Default is 1.
}
}\value{
A numerical value.
}\author{
  Hugo Marthinet
}\examples{
# MR
pop_tot <- c(32648555, 32587453)
nb_ref <- c(61904, 59910)

MR(nb_ref, pop_tot)



# SMR 

# Setting the values
pop_tot <- c(32648555, 32587453)
nb_ref <- c(61904, 59910)
MR_ref <- c(190, 183)
pop_obs <- c(300340, 301352)
nb_exp <- c(570, 551)
nb_obs <- c(200, 213)
# All three diffrent way to use the function
SMR(nb_obs, nb_exp, decimal = 3)
SMR(n.obs = nb_obs, n.ref = nb_ref, pop.obs = pop_obs, pop.ref = pop_tot, decimal = 3)
SMR(n.obs = nb_obs, pop.obs = pop_obs, MR.ref = MR_ref, decimal = 3)
}
