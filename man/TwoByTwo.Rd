\title{
  Metrics of a confusion matrix
}
\name{confusionMetrics}
\alias{confusionMetrics}
\description{
  Compile tous les indicateurs issus d'une matrice de confusion 2x2 : la prevalence,
  les risques de premiere et deuxieme espece, les sensibilite et specificite et les
  valeurs predictive positive et negative.
}\usage{
  TwoByTwo(tab = NULL,
                x.vrai = NULL,
                x.test = NULL,
                n.decimal = 3)
}\arguments{
  \item{tab}{
    La matrice de confusion 2x2. La première cellule doit être les vrais positifs.
}
  \item{x.vrai}{
    Le vecteur de valeurs vraies. Le vecteur doit être en 0/1 numerique, ou TRUE/FALSE.
    Cet argument n'est utilise que si \code{tab} n'est pas renseigne.
}
  \item{x.test}{
    Meme chose pour les valeurs issues du test.
}
  \item{n.decimal}{
    Nombre de decimales dans les indicateurs, par defaut 3.
}
}\value{
  Une liste de valeurs
}\author{
  Hugo Marthinet
}\examples{
#Test Matrice
(tab <- matrix(c(67, 2, 4, 7),
              nrow = 2, byrow = TRUE))
unlist(TwoByTwo(tab = tab))

#Test vecteurs
x.vrai <- c(rep(1, 71), rep(0, 9))
x.test <- c(rep(1, 67), rep(0, 11), rep(1, 2))
unlist(TwoByTwo(x.vrai = x.vrai,
               x.test = x.test))
}
