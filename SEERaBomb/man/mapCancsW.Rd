\name{mapCancsW}
\alias{mapCancsW}
\title{Map SEER Who Sitecodes to cancers}
\description{ Adds a factor cancer with easily recognizable levels to a SEER cancer data.frame.}
\usage{mapCancsW(D)} 
\arguments{
  \item{D}{A data frame that includes who and histo3 as columns.  }
}

\value{The input data frame with an additional cancerW column added on.}

\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}} }

\examples{
library(SEERaBomb)
mapCancsW # shows default definitions
}


\keyword{IO}
