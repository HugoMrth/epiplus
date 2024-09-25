kappa_fleiss <- function(table) {
  if (!(is.matrix(table))) {
    table <- as.matrix(table)
  }
  if (any(diff(rowSums(table)) != 0)) {
    stop("row sums must all be equal (same number of trials per rater)")
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
