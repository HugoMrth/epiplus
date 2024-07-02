\title{
  Standardized Difference
}\name{standardizedDifference}\alias{standardizedDifference}\description{
  Standardize difference to assess imbalance in a covariates between treated and control subjects
}\usage{
  # Continuous variable
  standardizedDifference(mean.treatment, mean.control, sd.treatment, sd.control)
  # Dichotomous variable
  standardizedDifference(prop.treatment, prop.control)
}\arguments{
  \item{mean.treatment}{
    Sample mean of X in treated subject. 
}
  \item{mean.control}{
    Sample mean of X in control subject.
}
    \item{sd.treatment}{
    Sample standard deviation of X in treated subject.
}
    \item{sd.control}{
        Sample standard deviation of X in control subject.
}
    \item{prop.treatment}{
        Sample prevalence of the variable deviation of X in treated subject.
}
    \item{prop.control}{
    Sample prevalence of the variable deviation of X in treated subject.
}
}\details{
  One must choose to calculate the standardize difference of either a continuous or dichotomous variable. 
  For continuous variable, one must fill in the parameters :\code{mean.treatment}, \code{mean.control}, \code{sd.treatment} and \code{sd.control}. 
  For dichotomous variable, on must fill in the parameters : \code{prop.treatment} and \code{prop.control}. 
  You should not mix both sets of parametersand only use the one you want to work with.
}\value{
  A list containing the standardize difference value and a diagnosis.
}\author{
  Hugo Marthinet
}\references{
Austin, P. C., and Stuart, E. A. (2015) Moving towards best practice when using inverse probability of treatment weighting (IPTW) using the propensity score to estimate causal treatment effects in observational studies. Statist. Med., 34: 3661–3679. doi: 10.1002/sim.6607. 
}\examples{
standardizedDifference(mean.treatment = 64, sd.treatment = 2.3, mean.control = 61.7, sd.control = 3.7)
standardizedDifference(mean.treatment = 64, sd.treatment = 7.1, mean.control = 63.7, sd.control = 9.2)
standardizedDifference(prop.treatment = 0.80, prop.control = 0.82)
}
