\title{
  Mortality-Related Morbidity Index (MRMI) and Expenditure-Related Morbidity Index (ERMI)
}
\name{morbidity}
\alias{morbidity}
\description{
  Calculation of the Two morbidity indices as described in Constantinou (2018).
The outcome-specific Mortality-Related Morbidity
Index (MRMI) and Expenditure-Related Morbidity
Index (ERMI), predictive of mortality and of
health care expenditure, respectively, were developed
and validated in a nationwide population of
people aged 65 years and older, with better overall
performance and better calibration than comparable
Charlson indices. \cr
Every argument expect gender and age correspond to wether or not the patients
present the given comorbidity. All of those must be binary (either a unique value or a vector).
Supported binary format are of the types \code{c(0, 1)} and \code{c(TRUE, FALSE)}.
}\usage{
  morbidity <- function(gender = NULL,
                      age = NULL,
                      isch.heart = NULL,
                      cerebro.vasc = NULL,
                      heart.fail = NULL,
                      periph.vasc = NULL,
                      diabetes = NULL,
                      cancer = NULL,
                      cancer.hist = NULL,
                      schizo = NULL,
                      depression = NULL,
                      subst.abuse = NULL,
                      dementia = NULL,
                      parkinson = NULL,
                      mult.sclero = NULL,
                      epilespy = NULL,
                      chornic.resp = NULL,
                      infl.bowel = NULL,
                      rheum.arthr = NULL,
                      HIV = NULL,
                      es.renal = NULL,
                      liver.pancr = NULL)
}\arguments{
  \item{gender}{
    Gender as \code{"M"} for male or \code{"F"} for female.
}
  \item{age}{
    Age in years as a numeric.
}
  \item{isch.heart}{
    Ischemic heart disease
}
  \item{cerebro.vasc}{
    Cerebrovascular disease
}
  \item{heart.fail}{
    Heart failure or arrhythmias or valve
diseases
}
  \item{periph.vasc}{
    Peripheral vascular disease 0.5442
}
  \item{diabetes}{
    Diabetes
}
  \item{cancer}{
    Cancer
}
  \item{cancer.hist}{
    History of cancer
}
  \item{schizo}{
    Schizophrenia and delusional diseases
}
  \item{depression}{
    Depression and mood disorders
}
  \item{subst.abuse}{
    Substance abuse disorders
}
  \item{dementia}{
    Dementia (including Alzheimer’s disease)
}
  \item{parkinson}{
    Parkinson disease
}
  \item{mult.sclero}{
    Multiple sclerosis or Paraplegia or tetraplegia
}
  \item{epilespy}{
    Epilepsy
}
  \item{chornic.resp}{
    Chronic respiratory diseases (including asthma and COPD)
}
  \item{infl.bowel}{
    Inflammatory bowel diseases
}
  \item{rheum.arthr}{
    Rheumatoid arthritis or systemic and connective tissue diseases
}
  \item{HIV}{
    HIV infection or AIDS
}
  \item{es.renal}{
    End-stage renal disease
}
  \item{liver.pancr}{
    Liver and pancreas diseases (including chronic and acute failures)
}
}\value{
  A list with two elements : the MRMI and ERMI.
}\author{
  Hugo Marthinet
}
\references{
Panayotis Constantinou, Philippe Tuppin, Anne Fagot-Campagna, Christelle Gastaldi-Ménager, François G. Schellevis, Nathalie Pelletier-Fleury,
Two morbidity indices developed in a nationwide population permitted performant outcome-specific severity adjustment,
Journal of Clinical Epidemiology,
Volume 103,
2018,
Pages 60-70,
ISSN 0895-4356,
https://doi.org/10.1016/j.jclinepi.2018.07.003.
}
\examples{
morbidity(gender = "M",
                      age = 75,
                      isch.heart = 0,
                      cerebro.vasc = 0,
                      heart.fail = 0,
                      periph.vasc = 0,
                      diabetes = 1,
                      cancer = 0,
                      cancer.hist = 0,
                      schizo = 0,
                      depression = 0,
                      subst.abuse = 0,
                      dementia = 0,
                      parkinson = 0,
                      mult.sclero = 0,
                      epilespy = 0,
                      chornic.resp = 1,
                      infl.bowel = 0,
                      rheum.arthr = 0,
                      HIV = 0,
                      es.renal = 0,
                      liver.pancr = 0)


morbidity(gender = "M",
          age = sample(50:90, 100, replace = TRUE),
          isch.heart = sample(c(0,1), 100, replace = TRUE),
          cerebro.vasc = sample(c(0,1), 100, replace = TRUE),
          heart.fail = sample(c(0,1), 100, replace = TRUE),
          periph.vasc = sample(c(0,1), 100, replace = TRUE),
          diabetes = sample(c(0,1), 100, replace = TRUE),
          cancer = sample(c(0,1), 100, replace = TRUE),
          cancer.hist = sample(c(0,1), 100, replace = TRUE),
          schizo = sample(c(0,1), 100, replace = TRUE),
          depression = sample(c(0,1), 100, replace = TRUE),
          subst.abuse = sample(c(0,1), 100, replace = TRUE),
          dementia = sample(c(0,1), 100, replace = TRUE),
          parkinson = sample(c(0,1), 100, replace = TRUE),
          mult.sclero = sample(c(0,1), 100, replace = TRUE),
          epilespy = sample(c(0,1), 100, replace = TRUE),
          chornic.resp = sample(c(0,1), 100, replace = TRUE),
          infl.bowel = sample(c(0,1), 100, replace = TRUE),
          rheum.arthr = sample(c(0,1), 100, replace = TRUE),
          HIV = sample(c(0,1), 100, replace = TRUE),
          es.renal = sample(c(0,1), 100, replace = TRUE),
          liver.pancr = sample(c(0,1), 100, replace = TRUE))
}
