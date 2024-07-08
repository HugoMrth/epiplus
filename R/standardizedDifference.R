standardizedDifference <- function(
    mean.treatment = NULL, mean.control = NULL,
    sd.treatment = NULL, sd.control = NULL,
    prop.treatment = NULL, prop.control = NULL) {
  
  if ((length(c(mean.treatment, mean.control, sd.treatment, sd.control)) > 0 & 
       length(c(prop.treatment, prop.control)) == 2) |
      (length(c(mean.treatment, mean.control, sd.treatment, sd.control)) == 4 & 
       length(c(prop.treatment, prop.control)) > 0)) {
    stop("You must provide EITHER the means and standard deviation for a quantitative variable,
         or the proportion for a dichotomous variable")
  }
  if (length(c(mean.treatment, mean.control, sd.treatment, sd.control)) < 4 & 
      length(c(prop.treatment, prop.control)) < 2) {
        stop("You must provide the ALL OF the means and standard deviation for a quantitative variable,
         or the proportion for a dichotomous variable")
  }
  
  if (all(!is.null(c(prop.treatment, prop.control)))) {
    standardDifference <- 100 * (prop.treatment - prop.control) / sqrt((prop.treatment * (1 -prop.treatment) + prop.control * (1 -prop.control))/2)
  }
  if (all(!is.null(c(mean.treatment, mean.control, sd.treatment, sd.control)))) {
    standardDifference <- 100 * (mean.treatment - mean.control) / sqrt((sd.treatment^2 + sd.control^2)/2)
  }
  
  diag <- ifelse(abs(standardDifference) < 10, 
                 "NOT indicative of meaningful imbalance in a covariates between treated and control subjects",
                 "Indicative of meaningful imbalance in a covariates between treated and control subjects")
  
  return(list(d = standardDifference,
              diagnostic = diag))
}

# ## ERRORS
# standardizedDifference(mean.treatment = 2)
# standardizedDifference(mean.treatment = 2, sd.treatment = 0.5)
# standardizedDifference(prop.treatment = 2)
# standardizedDifference(mean.treatment = 2, prop.treatment = 0.5)
# 
# # OK
# standardizedDifference(mean.treatment = 64, sd.treatment = 2.3, mean.control = 61.7, sd.control = 3.7)
# standardizedDifference(mean.treatment = 64, sd.treatment = 7.1, mean.control = 63.7, sd.control = 9.2)
# standardizedDifference(prop.treatment = 0.80, prop.control = 0.82)
