\name{post1PYO}
\alias{post1PYO}
\title{Get person-years at risk and observed cases after first cancers }
\description{Converts a canc data.frame into a list of objects containing information regarding person years at risk
for a second cancer after having a first cancer, and the number observed, in a defined time since exposure interval
and after a defined first cancer form of therapy.  }
\usage{post1PYO(canc, brks=c(0,2,5),binIndx=1,Trt="rad",yearEnd)} 
\arguments{
  \item{canc}{Input canc data.frame that is already sex, and possibly race, specific, but not cancer specific, as 
              treatment of any first cancer could potentially cause any second cancer.}
  \item{brks}{A vector of integer break years used to form time since exposure bins. }
  \item{binIndx}{The index of the interval for which py are to be computed by calling this function (i.e. post1PY).}
  \item{Trt}{The treatment for the first cancers. Note that the second cancer treatment is irrelevant here, so 
  the input canc must not be reduced to only cancers of a certain treatment type. }
  \item{yearEnd}{This is taken from the seerSet object.}
}

\value{A list where the first element is a list LPYM with as many PY matrices (PYM) as cancers in canc. The second
element is a matrix of cases observed in this interval after this treatment, where row names are first cancers and column names
are second cancers. The third element is a trivial scalar, the py-weighted midpoint of the time interval selected.
     }

\note{Examples are not possible without data. After the SEER data is installed, see the script mkRRtsx.R in the examples folder. This function was developed with support from the Cleveland Clinic Foundation.}
\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}, \link{getE},\link{seerSet}} }
\keyword{IO}
