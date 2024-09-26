adherence <- function(
    n.days.supplied = NA,
    n.days.gaps = NA,
    n.days.period = NA,
    n.days.last.supply = NA,
    surplus = NA,
    diff.first.last.supply = NA,
    quantity.supplied = NA,
    quantity.per.day = NA) {

  if (any(!is.na(n.days.supplied) & (!is.numeric(n.days.supplied) | n.days.supplied < 0))) stop("n.days.supplied must be a positive integer")
  if (any(!is.na(n.days.gaps) & (!is.numeric(n.days.gaps) | n.days.gaps < 0))) stop("n.days.gaps must be a positive integer")
  if (any(!is.na(n.days.period) & (!is.numeric(n.days.period) | n.days.period < 0))) stop("n.days.period must be a positive integer")
  if (any(!is.na(n.days.last.supply) & (!is.numeric(n.days.last.supply) | n.days.last.supply < 0))) stop("n.days.supplied must be a positive integer")
  if (any(!is.na(surplus) & (!is.numeric(surplus) | surplus < 0))) stop("surplus must be a positive integer")
  if (any(!is.na(diff.first.last.supply) & (!is.numeric(diff.first.last.supply) | diff.first.last.supply < 0))) stop("diff.first.last.supply must be a positive integer")
  if (any(!is.na(quantity.supplied) & (!is.numeric(quantity.supplied) | quantity.supplied < 0))) stop("quantity.supplied must be a positive integer")
  if (any(!is.na(quantity.per.day) & (!is.numeric(quantity.per.day) | quantity.per.day < 0))) stop("quantity.per.day must be a positive integer")

  CMA <- n.days.supplied / n.days.period
  CMG <- n.days.gaps / n.days.period
  CMOS <- ifelse(is.na(surplus), n.days.gaps, -surplus)
  CMOS <- CMOS / n.days.period
  CR <- (n.days.supplied - n.days.last.supply) / diff.first.last.supply * 100
  CSA <- n.days.supplied / n.days.period
  DBR <- 1 - ((diff.first.last.supply - n.days.supplied)/diff.first.last.supply) * 100
  MPR <- n.days.supplied / n.days.period
  MPRm <- (n.days.supplied / (diff.first.last.supply + n.days.last.supply)) * 100
  MRA <- n.days.supplied / n.days.period * 100
  PDC <- pmax(n.days.supplied / n.days.period * 100, 1)
  RCR <- ((quantity.supplied / quantity.per.day) * 100) / diff.first.last.supply

  return(list(
    CMA = CMA,
    CMG = CMG,
    CMOS = CMOS,
    CR = CR,
    CSA = CSA,
    DBR = DBR,
    MPR = MPR,
    MPRm = MPRm,
    MRA = MRA,
    PDC = PDC,
    RCR = RCR
  ))
}
