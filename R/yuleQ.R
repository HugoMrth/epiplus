yuleQ <- function(table) {
  #### Check Params ####
  if(is.null(table)) stop("table missing")
  if(any(dim(table) != 2)) stop("table must be a 2 by 2 table")
  if (any(table < 0)) stop("frequencies cannot be negative")
  # Conversion en matrice
  if (!(is.matrix(table)))  table <- as.matrix(table)

  #### Code Fonction ####
  return(((table[1,1]+table[2,2])-(table[1,2]+table[2,1]))/sum(table))
}
