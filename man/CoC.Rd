\title{
  Continuity of Care
}
\name{CoC}
\alias{CoC}
\description{
Compute various metrics of a continuity of care. This includes both measures of concentration (or density) and dispersion.
  Some other measures, not included here like the SECON index, also take into account the time factor. The one presented here only rely on
  the numbers of visits to healthcare professionals.
}\usage{
CoC(w, n,
    vi, vc, vu, vs)
}\arguments{
  \item{w}{
  Total number of visits.
}
  \item{n}{
Total number of different healthcare professionnal consulted.
}
  \item{vi}{
Number of visits per healcare professionnal. This is a vector unless there is only one professional.
}
  \item{vc}{
Number of visit for the last professionnal consulted.
}
  \item{vu}{
Number of visits for the most consulted professionnal.
}
  \item{vs}{
Number of sites visited.
}
}\value{
  A named vector containing various concentration and dispersion measures.
  Values are missing if any necessary information in missing in the function call.
  Output element are as listed :
  \itemize{
  \item CPC - Current Provider of Care (Smebdy 1986) : Total number of visits to the last seen
  healthcare professionnal divide by the total number of visits.
  \item UPC - Usual Provider of Care (Breslau et Reeb 1975) : Rate of visits to the most consulted
  healthcare professionnal.
  \item HCC - Clinician Index (Mainous et Gill 1998) : Equals to 1 if UPC is greater than .5, 0 otherwise.
  \item HSC - Site index (Mainous et Gill 1998) : Equals to 1 if the rate of visits to the
  usual site is greater than .5, 0 otherwise.
  \item HI - Herfindahl Index (Eriksson et Mattsson 1983) : Sum of the squared fractions of visits
  per healthcare professionnal.
  \item CI - Continuity Index (Mainous et Gill 1998) : Base on both the clinicians and site indexes.
  Equals to 1 if HCC=1 and CI=1, 0 if HCC=0 and CI=0, .5 otherwise.
  \item CoC - Bice-Boxerman continuity of care (Bice et Boxerman 1977) : Frequency to which the patients
  consulted the same healthcare professionnal over the time period. CoC=[(∑(v(i)/w)²-(1/w)]/[1-(1/w)]
  \item EK - Ejlertsson’s K Index (Ejlertsson 1984) : Using only the total number of visits and professionnals
    consulted : EK=(w-n)/(w-1).
  \item MCI - Modified Continuity Index (Sturnberg 2002) : MCI=1-n/(w+0,1)
  \item PPC - Personal Provider Continuity (Sturnber 2002) : Dichotomised version of the MCI. Equals to 1 if
   MCI greater than .66, 0 otherwise.
  \item MMCI - Modified, Modified Continuity Index (Magill et Senf 1987) : MMCI=[1-n/(w+0,1)] / [1-1/(w+0,1)]
  \item INOP - Inverse Number of Providers (Eriksson et Mattsson 1983) : Inverse of the total number of visits.
    }
}\author{
  Hugo Marthinet
}\examples{
  # Example for a patient with a care sequence like : ABABBCBD
  # A, B and C being diffrent practionners
  # Order from most to leat recent (A is the most recent visit)

  seq <- "ABABBCBD"
  pract <- c("A", "B", "C", "D")
  vi <- rep(NA, 4)
  for (i in 1:4) {
    vi[i] <- length(lapply(gregexpr(pract[i], seq), attributes)[[1]]$match.length)
  }
  vi

  CoC(w = 8,
      n = 4, # A, B, C and D
      vi = vi,
      vc = 2, # 2 times A
      vu = 4, # 4 times B
      vs = 4) # Assuming every sites are different

  CoC(8, 4, vc = 2)
}
