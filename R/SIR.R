SIR <- function(data, value, ref.value = NULL,
         loc, ref.loc = NULL) {

  #### Check Params ####
  if(is.null(data)) stop("data manquant")
  if(is.null(value)) stop("value manquant")

  if (is.null(ref.value)) ref.value <- names(table(data[,value]))
  if (is.null(ref.loc)) ref.loc <- names(table(data[,loc]))

  #### Code Fonction ####
  res <- c()
  res_names <- c()

  for (i in 1:length(ref.value)) {
    for (j in 1:length(ref.loc)) {
      SIR <- (sum(data[data[,loc] == ref.loc[j], value] == ref.value[i], na.rm = TRUE) /
                table(data$Race)[names(table(data$Race)) == ref.loc[j]]) /
        (sum(data[, value] == ref.value[i], na.rm = TRUE) /
           sum(table(data$Race)))
      res <- c(res, SIR)
      res_names <- c(res_names, paste0(ref.value[i], "/", ref.loc[j]))
    }
  }

  names(res) <- res_names
  return(res)
}
