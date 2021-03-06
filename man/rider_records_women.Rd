% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{rider_records_women}
\alias{rider_records_women}
\title{Elite women results}
\format{
Column description:
\describe{
  \item{\code{date}}{Result date.}
  \item{\code{result}}{Result (finishing place).\cr
    When a rider is listed as DNF, DNS, OTL etc., their results are encoded
    numerically as follows:
    \tabular{lcr}{
      \strong{Result} \tab \tab \strong{Code}\cr
      DNF \tab \tab 999\cr
      DNS \tab \tab 998\cr
      OTL \tab \tab 997\cr
      DF \tab \tab 996\cr
      NQ \tab \tab 995\cr
      DSQ \tab \tab 994\cr
    }}
  \item{\code{gc_result_on_stage}}{GC placement if applicable, otherwise NA.}
  \item{\code{race}}{Name of the race.}
  \item{\code{distance}}{Covered distance if applicable, otherwise NA.}
  \item{\code{pointspcs}}{Scored points (PCS ranking).}
  \item{\code{pointsuci}}{Scored points (UCI ranking).}
  \item{\code{stage}}{Stage description if applicable, 'One day' in case of classic
    races, eventually classification description (e.g. Point classification).}
  \item{\code{rider}}{Rider name.}
  \item{\code{team}}{Current team.}
}
}
\source{
\url{http://www.procyclingstats.com}
}
\usage{
rider_records_women
}
\description{
A dataset containing the results of elite women category.
}
\keyword{datasets}
