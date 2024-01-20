\title{
  Standardized Infection Ratio (SIR)
}\name{SIR}\alias{SIR}\description{
  Calcul le SIR d'une population
}\usage{
  SIR(data, value, ref.value, loc, ref.loc)
}\arguments{
  \item{data}{
    Le jeu de données
}
  \item{value}{
    Nom (ou numéro) de la colonne de la variable sur laquelle calculer le SIR.
}
  \item{ref.value}{
    Valeur de référence du vecteur de valeur. Par défaut, le SIR est calculé pour toutes les valeurs
    et la sortie de la fonction est un vecteur
}
  \item{loc}{
    Nom (ou numéro) de la colonne de la variable qui correspond au lieu.
}
  \item{ref.loc}{
    Valeur de référence du vecteur de lieux. Par défaut, le SIR est calculé pour toutes les valeurs
    et la sortie de la fonction est un vecteur
}
}\value{
  Un numérique ou un vecteur de numériques
}\author{
  Hugo Marthinet
}\examples{
# Chargement du dataset
library(tab)
head(tabdata)

#Calculs des SIR
#Pour une population donnée
SIR(tabdata, value = "Sex", ref.value = "Female", loc = "Race", ref.loc = "White")

#Pour toutes les combinaisons de populations
SIR(tabdata, value = "Sex", loc = "Race")
}
