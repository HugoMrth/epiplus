kappa_fleiss <- function(table) {
  if (!(is.matrix(table))) {
    table <- as.matrix(table)
  }

  p0 <- 1
  for (i in 2:nrow(table)) {
    p0 <- p0 + (1/(sum(table[i-1, ])*(sum(table[i-1, ])-1))) *
      (sum(table[i, ]^2) - sum(table[i-1, ]))
  }
  p0 <- p0/nrow(table)

  p1 <- sum((colSums(table)/sum(table))^2)

  return((p0 - p1)/(1 - p1))
}
