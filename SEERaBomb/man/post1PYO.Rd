\name{post1PYO}
\alias{post1PYO}
\title{Get person-years at risk and observed cases after first cancers }
\description{Converts a canc data.frame into a list of objects containing information regarding person years at risk
for a second cancer after having a first cancer, and the number observed, in a defined time since exposure interval
and after a defined first cancer therapy.  }
\usage{post1PYO(canc, brks=c(0,2,5),binIndx=1,Trt="rad",PYM=FALSE,yearEnd,firstS,secondS)} 
\arguments{
  \item{canc}{Input canc data.frame that is already sex, and possibly race, specific, but not cancer specific, as 
              treatment of any first cancer could potentially cause any second cancer.}
  \item{brks}{A vector of break points in years used to form time since diagnosis bins. }
  \item{binIndx}{The index of the interval for which py are to be computed by calling this function.}
  \item{Trt}{The treatment for the first cancers. Note that the second cancer treatment is irrelevant here, so 
  the input canc must not be reduced to only certain treatment types. }
  \item{PYM}{PYM of tsd.}
  \item{yearEnd}{This is taken from the seerSet object.}
  \item{firstS}{Vector of first cancers of interest as strings.}
  \item{secondS}{Vector of second cancers of interest as strings.}
}

\value{A list where the first element is a list LPYM with as many PY matrices (PYM) as cancers in canc. The second
element is a matrix of cases observed in this interval after this treatment, where row names are first cancers and column names
are second cancers. The third element is a trivial scalar, the py-weighted midpoint of the time interval selected. }

\note{After the SEER data is installed, see the script mkRRtsx.R in the examples folder. }
\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}, \link{getE},\link{seerSet}} }
\keyword{internal}
