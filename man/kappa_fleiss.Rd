\name{kappa_fleiss}
\alias{kappa_fleiss}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Calcul le Kappa de Fleiss.
}
\description{
  Compile le Kappa de Fleiss - indicateur d'accord entre juges - d'un tableau de notation de juges.
}
\usage{
  kappa_cohen(table)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{table}{
    Un tableau tableau d'accord entre juges avec en lignes les sujets notés et en colonnes, les catégories.
    Les cellules correspondent au nombre de juges ayant attribué une catégorie données à un sujets.
    La somme en ligne est donc toujours égale au nombre de juges.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
    Un indice compris entre -1 et 1 traduisant l'accord inter-juges.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
  Hugo Marthinet
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{kappa_cohen}}
}
\examples{

}
