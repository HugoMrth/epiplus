\title{
  Mortality-Related Morbidity Index (MRMI) and Expenditure-Related Morbidity Index (ERMI)
}
\name{morbidity}
\alias{morbidity}
\description{
  Calculation of the Two morbidity indices as described in Constantinou (2018).
}\usage{
  SIR(data, value, ref.value, loc, ref.loc)
}\arguments{
  \item{data}{
    A data frame containing the data.
}
  \item{value}{
Name or index of the variable on which to compute the SIR.
}
  \item{ref.value}{
Reference value to use. By default, the SIR is calculated on each vaue individualy, and the output is a vector.
}
  \item{loc}{
Name or index of the location variable.
}
  \item{ref.loc}{
Reference value for the location vector. By default, the SIR is calculated on each vaue individualy, and the output is a vector.
}
}\value{
  Either a numerical value, or a vector, depending on the parameters.
}\author{
  Hugo Marthinet
}\examples{
# Data
library(tab)
head(tabdata)

# SIR for a given population
SIR(tabdata, value = "Sex", ref.value = "Female", loc = "Race", ref.loc = "White")

# SIR for every possible combinaison
SIR(tabdata, value = "Sex", loc = "Race")
}
