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
