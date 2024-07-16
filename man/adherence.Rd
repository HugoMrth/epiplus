\title{
  Measurement of Adherence
}
\name{adherence}
\alias{adherence}
\description{
A variety of measures have been developed to calculate refill adherence from administrative data such as pharmacy
claims databases. These measures have focused on improving the accuracy of adherence measures or clarifying the evaluation
time frame. As a result, there are many measures used to assess adherence that may or may not be comparable or accurate. \cr
Eleven refill adherence measures were identified and compared using data from the LOSE Weight (Long-term
Outcomes of Sibutramine Effectiveness on Weight) study. Measures compared include Continuous Measure of Medication
Acquisition (CMA); Continuous Multiple Interval Measure of Oversupply (CMOS); Medication Possession Ratio (MPR); Medication
Refill Adherence (MRA); Continuous Measure of Medication Gaps (CMG); Continuous, Single Interval Measure of Medication
Aquisition (CSA); Proportion of Days Covered (PDC); Refill Compliance Rate (RCR); Medication Possession Ratio, modified
(MPRm); Dates Between Fills Adherence Rate (DBR); and Compliance Rate (CR).
}\usage{
adherence(
    n.days.supplied,
    n.days.gaps,
    n.days.period,
    n.days.last.supply,
    surplus,
    diff.first.last.supply,
    quantity.supplied,
    quantity.per.day)
}\arguments{
  \item{n.days.supplied}{
Depending on the context, can either represent the cumulative days’ supply of medication obtained, 
the total days supplied or the number of days’ supply obtained at beginning of interval.
}
  \item{n.days.gaps}{
The total days of treatment gaps.
}
  \item{n.days.period}{
Depending on the context, can either represent the total number of days to next fill or end of observation period,
the total number of days in the observation period, the number of days in the studied interval or
the total number of days evaluated.
}
  \item{n.days.last.supply}{
The number of days’ supply obtained at the last dispensation.
}
  \item{surplus}{
 The days’ supply of medication exceeding days of study participation.
}
  \item{diff.first.last.supply}{
The number of days in interval between first and last refill.
}
  \item{quantity.supplied}{
Sum of quantity dispensed over interval.
}
  \item{quantity.per.day}{
Quantity to be taken per day.
}
}\value{
  A named vector containing various adherence measures.
  Values are missing if any necessary information in missing in the function call.
  Output element are as listed :
  \itemize{
  \item CMA (Continuous Measure of Medication Acquisition), the adherence value for cumulative time period :
cumulative days’ supply of medication obtained/total days to next fill or to end of observation period.
  \item CMG (Continuous Measure of Medication Gaps), the nonadherence value for cumulative period winsorized at zero :
total days of treatment gaps/total days to next fill or end of observation period.
  \item CMOS (Continuous Multiple Interval Measure of Oversupply), the nonadherence value for cumulative period, allowing for surplus :
total days of treatment gaps (+) or surplus (–)/total days in observation period.
  \item CR (Compliance Rate), the adherence value for period between fills :
(total days supplied – last days’ supply)/(last claim date – first claim date) × 100.
  \item CSA (Continuous, Single Interval Measure of Medication Aquisition), adherence value for interval of study participation overall adherence percentage :
days’ supply obtained at beginning of interval/days in interval.
  \item DBR (Dates Between Fills Adherence Rate), overall adherence percentage :
1 – {[(last claim date – first claim date) – total days’ supply]/(last claim date – first claim date)} × 100.
  \item MPR (Medication Possession Ratio), ratio of medication available :
days’ supply: days in period.
  \item MPRm (Medication Possession Ratio, modified), adherence percentage, adjusted to include final refill period :
[total days supplied/(last claim date – first claim date + last days’ supply)] × 100.
  \item MRA (Medication Refill Adherence), overall adherence percentage :
(total days’ supply/total number of days evaluated) × 100.
  \item PDC (Proportion of Days Covered), percentage of days with medication available :
(total days supply/total number of days evaluated) × 100%, capped at 1.0.
  \item RCR (Refill Compliance Rate), overall adherence percentage :
[(sum of quantity dispensed over interval/quantity to be taken per day) × 100]/number of days in interval between first and last refill.
    }
}\author{
  Hugo Marthinet
}
\references{
1. Hess LM, Raebel MA, Conner DA, Malone DC. Measurement of Adherence in Pharmacy Administrative Databases: A Proposal for Standard Definitions and Preferred Measures. Annals of Pharmacotherapy. 2006;40(7-8):1280-1288. doi:10.1345/aph.1H018
}
\examples{

}
