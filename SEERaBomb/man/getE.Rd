\name{getE}
\alias{getE}
\title{Get expected numbers of cases.  }
\description{Converts the LPYM list of matrices produced by post1PYO into a matrix corresponding to the observed cases 
matrix produced by post1PYO. This is seperated out because it is time consuming and alternative approaches should 
be pursued.}
\usage{getE(LPYM,L2D)} 
\arguments{
  \item{LPYM}{List of matrices produced by post1PYO.}
  \item{L2D}{List of 2D spline fit functions produced by mk2D. }
}

\value{A matrix of expected cases with first cancers labeling the rows and second cancer labeling the columns.}

\note{Examples are not possible without data. After the SEER data is installed, see the script mkRRtsx.R in the examples folder. This function was developed with support from the Cleveland Clinic Foundation.}
\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}, \link{seerSet}} }
\keyword{IO}
