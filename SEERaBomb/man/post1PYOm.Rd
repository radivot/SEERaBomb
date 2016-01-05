\name{post1PYOm}
\alias{post1PYOm}
\title{Get person-years at risk and observed deaths after cancer }
\description{Converts data.frame D into a list of objects containing information regarding person years at risk
of death after cancer and the number of deaths observed, in a defined time since exposure interval.  }
\usage{post1PYOm(D,brks=c(0,2,5),binIndx=1,yearEnd)} 
\arguments{
  \item{D}{Input data.frame with columns: yrdx, agedx, surv, and status (1=dead).}
  \item{brks}{A vector of break points in years used to form time since diagnosis bins. }
  \item{binIndx}{The index of the interval for which py are to be computed by calling this function.}
  \item{yearEnd}{The most recent year of available mortality data.}
}
\value{A list where the first element is a PY matrix (PYM), the second
element is a vector of deaths observed in the intervals, and the third is a scalar, the py-weighted midpoint of the time intervals. }
\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}, \link{getE},\link{post1PYO}} }
\keyword{internal}
