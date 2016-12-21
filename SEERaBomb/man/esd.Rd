\name{esd}
\alias{esd}
\title{Event vs years Since Diagnosis}
\description{Computes relative risks (RR) of second cancers over specified years-since-diagnosis intervals. 
SEER incidence rates are used to compute background/expected numbers of cases E, sex, age, and calendar year
specifically.  RR = O/E where O and E are the numbers of observed and expected cases. }
\usage{esd(d,srfF,srfM,brkst=c(0,2,5),brksy=NULL) } 
\arguments{
  \item{d}{Input data.frame with columns: yrdx, agedx, sex, py at risk (in years), cancer1, and cancer2.
  Cancer1 and cancer2 should use standard SEERaBomb cancer names, see mapCancs. Cases
  not ending in a second cancer should have cancer2 set to "none".}
  \item{srfF}{Female incidence surface. Output D of mk2D for females, for cancers in cancer2 }
  \item{srfM}{Male incidence surface. Output D of mk2D for males, for cancers in cancer2 }
  \item{brkst}{Vector of breaks in years used to form times since diagnosis intervals/bins. }
  \item{brksy}{Vector of breaks of calendar years to show  trends. Leave NULL for all in one. }
}
\value{data.frame with observed and expected cases, RR, and RR CI for each time since diagnosis interval. }
\note{ This function was developed with support from the Cleveland Clinic Foundation.}
\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}, \link{mk2D},\link{msd}} }

\keyword{IO}
