\title{
  Charlson Comorbidity Index
}
\name{comorbidity}
\alias{comorbidity}
\description{
  Calculation of the Charlson Comorbidity Index as described in Charlson (1987).
The method of classifying comorbidity provides a simple,
readily applicable and valid method of estimating risk of death from comorbid disease for use in
longitudinal studies. \cr
Every argument expect age correspond to wether or not the patients
present the given comorbidity. All of those must be binary (either a unique value or a vector).
Supported binary format are of the types \code{c(0, 1)} and \code{c(TRUE, FALSE)}.
}\usage{
  comorbidity(age,
              myocard.infarct,
             heart.fail,
             periph.vasc,
             cerebr,
             dementia,
             chronic.pulm,
             connect.tissue,
             ulcer,
             mild.liver,
             diabetes,
             hemiplegia,
             renal,
             diabetes.org.dmg,
             tumor,
             leukemia,
             lymphoma,
             mode.sev.liver,
             metast.tumor,
             aids)
}\arguments{
  \item{age}{
    Age in years as a numeric.
}
  \item{myocard.infarct}{
Myocardial infarct
}
  \item{heart.fail}{
Congestive heart failure
}
  \item{periph.vasc}{
Peripheral vascular disease
}
  \item{cerebr}{
Cerebrovascular disease
}
  \item{dementia}{
Dementia
}
  \item{chronic.pulm}{
Chronic pulmonary disease
}
  \item{connect.tissue}{
Connective tissue disease
}
  \item{ulcer}{
Ulcer disease
}
  \item{mild.liver}{
Mild liver disease
}
  \item{diabetes}{
Diabetes
}
  \item{hemiplegia}{
Hemiplegia
}
  \item{renal}{
Moderate or severe renal disease
}
  \item{diabetes.org.dmg}{
Diabetes with end organ damage
}
  \item{tumor}{
Any tumor
}
  \item{leukemia}{
Leukemia
}
  \item{lymphoma}{
Lymphoma
}
  \item{mode.sev.liver}{
Moderate or severe liver disease
}
  \item{metast.tumor}{
Metastatic solid tumor
}
  \item{aids}{
AIDS
}
}\value{
  A one element named vector.
}\author{
  Hugo Marthinet
}
\references{
Charlson ME, Pompei P, Ales KL, MacKenzie CR. A new method of classifying prognostic comorbidity in longitudinal studies: development and validation. J Chronic Dis. 1987
}
\examples{
comorbidity(
  age = 75,
  myocard.infarct = 0,
  heart.fail = 0,
  periph.vasc = 0,
  cerebr = TRUE,
  diabetes = 0,
  dementia = 0,
  chronic.pulm = 0,
  connect.tissue = 0,
  ulcer = 0,
  mild.liver = 0,
  hemiplegia = 0,
  renal = TRUE,
  diabetes.org.dmg = 0,
  tumor = 0,
  leukemia = 0,
  lymphoma = 0,
  mode.sev.liver = 0,
  metast.tumor = 0,
  aids = 0
)
}
