\name{mapTrts}
\alias{mapTrts}
\title{Map treatment codes to factor}
\description{Uses SEER codes in the SEER field radiatn to add a
factor named trt with levels "noRad","rad", and "unk" to a cancer data frame.}
\usage{mapTrts(D)} 
\arguments{
  \item{D}{A SEER cancer data frame that includes the field radiatn as a column.  }
}

\value{The input data frame with an additional trt column added to its end.}

\note{This function is used by mkSEER when it generates merged R binaries. It is exposed to state
the default definition of trt and, by way of example, 
to show how to override it.}
\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}, \link{mapTrts}} }

\examples{
library(SEERaBomb)
mapTrts # exposes default definition of trt
}


\keyword{IO}
