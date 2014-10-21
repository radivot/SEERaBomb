\name{getE2}
\alias{getE2}
\title{Get expected numbers of cases, version 2.  }
\description{Converts the LPYM list of matrices produced by post1PYO into a matrix corresponding to the observed cases 
matrix produced by post1PYO. This is seperated out because it is time consuming and alternative approaches should 
be pursued.}
\usage{getE2(LPYM,D)} 
\arguments{
  \item{LPYM}{List of first cancer PY matrices produced by post1PYO.}
  \item{D}{D component of seerSet list produced by mk2D. }
}

\value{A matrix of expected cases with first cancers labeling the rows and second cancers labeling the columns.}

\note{After the SEER data is installed, see the script mkRRtsx.R in the examples folder. This function was developed with support from the Cleveland Clinic Foundation.}
\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}, \link{seerSet}} }
\keyword{IO}
