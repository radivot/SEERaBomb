\name{mapTrts}
\alias{mapTrts}
\title{Map SEER treatment codes to a treatment factor}
\description{ This function adds a column called trt to the cancer data.frame or tbl.}
\usage{mapTrts(D)} 
\arguments{
  \item{D}{A cancer dataframe that includes radiatn as treatment column.  }
}

\value{The input dataframe with an additional trt column added to it.}

\note{This function is used internally when generating merged R binaries. It is exposed 
here to show users how they can overwrite this column by writing a similar function.}
\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}, \link{mapTrts}} }
\keyword{IO}
