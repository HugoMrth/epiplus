kappa_cohen <- function(table) {

  #### Check Params ####
  if(is.null(table)){
    stop("table missing")
  }
  if(dim(table) == c(2, 2)){
    stop("table must be a 2 by 2 table")
  }

  # Conversion en matrice
  if (!(is.matrix(table))) {
    table <- as.matrix(table)
  }

  #### Code Fonction ####

  # Calcul de la proportion d'accord
  p0 <- (table[1,1] + table[2,2]) / (sum(table))
  # Calcul de l'accord maximal
  p1 <- (colSums(table)[1] * rowSums(table)[1] / sum(table) +
           colSums(table)[2] * rowSums(table)[2] / sum(table)) / sum(table)
  # Kappa de Cohen
  return((p0 - p1)/(1 - p1))
}
