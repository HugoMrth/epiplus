CoC <- function(w = NA, n = NA,
                vi = NA, vc = NA, vu = NA, vs = NA) {

  if (any(!is.na(w) & (!is.numeric(w) | w < 0))) stop("w must be a positive integer")
  if (any(!is.na(n) & (!is.numeric(n) | n < 0))) stop("n must be a positive integer")
  if (any(!is.na(vc) & (!is.numeric(vc) | vc < 0))) stop("vc must be a positive integer")
  if (any(!is.na(vu) & (!is.numeric(vu) | vu < 0))) stop("vu must be a positive integer")
  if (any(!is.na(vs) & (!is.numeric(vs) | vs < 0))) stop("vs must be a positive integer")

  return(
    list(
      CPC = vc/w,
      UPC = vu/w,
      HCC = ifelse(vu/w >= 0.5, 1, 0),
      HSC = ifelse(vs/w >= 0.5, 1, 0),
      HI = sum((vi/w)^2),
      CI = ifelse(ifelse(vu/w > 0.5, 1, 0) == 0 & ifelse(vs/w > 0.5, 1, 0) == 0,
                  0,
                  ifelse(ifelse(vu/w > 0.5, 1, 0) == 1, 1, 0.5)),
      COC = (sum((vi/w)^2)-1/w)/(1-(1/w)),
      EK = (w-n)/(w-1),
      MCI = 1-n/(w + 0.1),
      PPC = ifelse(1-n/(w + 0.1) >= 0.66, 1, 0),
      MMCI= (1-n/(w + 0.1)) / (1-1/(w + 0.1)),
      INOP = 1/n
    )
  )
}
