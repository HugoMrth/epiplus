confusionMetrics <- function(tab = NULL,
                           x.true = NULL,
                           x.test = NULL,
                           n.decimal = 3) {

  #### Check Params ####

  if(is.null(tab) & (is.null(x.true) | is.null(x.test))) stop("You must either provide tab, or both x.true and x.test.")
  if(!is.null(tab) & all(dim(tab) != c(2, 2))) stop("Confusion matrix must be a two by two table.")

  if(is.null(tab)) {
    if (length(x.true) != length(x.test)) stop("x.true and x.test must have the same length.")
    if (length(table(x.true)) > 2) stop("x.true has more than two different values")
    if (length(table(x.test)) > 2) stop("x.test has more than two different values")
    if (!(all(x.true %in% c(TRUE, FALSE))) | !(all(x.true %in% c(0, 1)))) stop("all x.true values must be either (TRUE, FLASE) or (0, 1)")
    if (!(all(x.test %in% c(TRUE, FALSE))) | !(all(x.test %in% c(0, 1)))) stop("all x.true values must be either (TRUE, FLASE) or (0, 1)")

    inv_tab <- table(x.test, x.true)
    tab <- matrix(c(inv_tab[2, 2], inv_tab[2, 1], inv_tab[1, 2], inv_tab[1, 1]),
                  nrow = 2, byrow = TRUE)
  }

  #### Code Fonction ####

  prevalence <- sum(tab[,1])/(sum(tab))

  risque_1er <- tab[2, 1]/(sum(tab))
  risque_2em  <- tab[1, 2]/(sum(tab))

  specificite <- tab[2,2]/(sum(tab[, 2]))
  sensibilite <- tab[1,1]/(sum(tab[, 1]))

  valeur_predictive_pos <- (sensibilite * prevalence) / ((sensibilite * prevalence) + (1 - specificite) * (1 - prevalence))
  valeur_predictive_neg <- (specificite * (1 - prevalence)) / (((1 - sensibilite) * prevalence) + specificite * (1 - prevalence))

  res <- list(val = data.frame(prevalence = round(prevalence, n.decimal),
                           alpha = round(risque_1er, n.decimal),
                           beta = round(risque_2em, n.decimal),
                           sensi = round(sensibilite, n.decimal),
                           speci = round(specificite, n.decimal),
                           VPP = round(valeur_predictive_pos, n.decimal),
                           VPN = round(valeur_predictive_neg, n.decimal)),
              OR = data.frame(OR = round(fisher.test(tab)$estimate, n.decimal),
                          lower = round(fisher.test(tab)$conf.int[1], n.decimal),
                          upper = round(fisher.test(tab)$conf.int[2], n.decimal),
                          pval = fisher.test(tab)$p.value),
              RR = data.frame(OR = round(epitools::riskratio.wald(tab)$measure[2, 1], n.decimal),
                          lower = round(epitools::riskratio.wald(tab)$measure[2, 2], n.decimal),
                          upper = round(epitools::riskratio.wald(tab)$measure[2, 3], n.decimal),
                          pval = epitools::riskratio.wald(tab)$p.value[2,2])
  )


  return(res)
}
