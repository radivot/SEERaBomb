\name{post1PYOc}
\alias{post1PYOc}
\title{Get person-years at risk and observed cases after first cancers }
\description{Converts a canc data.frame into a list of objects containing information regarding person years at risk
for a second cancer after having a first cancer, and the number observed, in defined intervals of time since diagnosis, calendar year, and age at diagnosis.   }
\usage{post1PYOc(canc, brkst=c(0),binIndxt=1,brksy=c(1973),binIndxy=1,brksa=c(0),binIndxa=1,
             Trt="rad",PYLong=FALSE,yearEnd,firstS,secondS)} 
\arguments{
  \item{canc}{Input canc data.frame that is already sex, and possibly race, specific, but not cancer specific, as 
              treatment of any first cancer could potentially cause any second cancer.}
  \item{brkst}{Vector of breaks in years used to form tsd intervals/bins. An upper limit of 100, well beyond 40 years
              of SEER follow up currently available, is assumed/added to brkst internally, and should thus not be in brkst. }
  \item{binIndxt}{The index of the tsd interval for which py are to be computed by calling this function.}
  \item{brksy}{Vector of breaks used to form groups of calendar year at diagnosis intervals/bins.
               An upper limit of yearEnd (last year in SEER; a seerSet field) is assumed/added to brksy internally.}
  \item{binIndxy}{The index of the year interval for which py are to be computed by calling this function.}
  \item{brksa}{Vector of breaks used to form groups of age at diagnosis intervals/bins. An upper limit of 126 is assumed. }
  \item{binIndxa}{The index of the age at DX interval for which py are to be computed by calling this function.}
  \item{Trt}{The treatment for the first cancers. Note that the second cancer treatment is irrelevant here, so 
  the input canc must not be reduced to only certain treatment types. }
  \item{PYLong}{PYLong of tsd.}
  \item{yearEnd}{This is taken from the seerSet object.}
  \item{firstS}{Vector of first cancers of interest as strings.}
  \item{secondS}{Vector of second cancers of interest as strings.}
}

\value{A list where the first element is a list LPYM with as many PY matrices (PYM) as cancers in canc. The second
element is a matrix of cases observed in this interval after this treatment, where row names are first cancers and column names
are second cancers. And after a few other slots, the last element is a trivial scalar, the py-weighted midpoint of the time interval selected. }

\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}, \link{getE},\link{seerSet}} }
\keyword{internal}
