\name{kappa_cohen}
\alias{kappa_cohen}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Calcul le Kappa de Cohen d'un tableau de contingence 2x2.
}
\description{
  Compile le Kappa de Cohen - indicateur d'accord entre juges - d'un tableau de contingence de deux vecteurs binaires.
}
\usage{
  kappa_cohen(table)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{table}{
    Un tableau de contingence 2x2. Correspondant à un tableau d'accord entre juges.
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
  \code{\link{kappa_fleiss}}

}
\examples{

}
