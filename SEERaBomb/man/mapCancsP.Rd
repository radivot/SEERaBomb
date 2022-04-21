\name{mapCancsP}
\alias{mapCancsP}
\title{Map Primary Site and ICD-O3 histology codes to cancers}
\description{ Adds a factor cancer with easily recognizable levels to a SEER cancer data.frame.}
\usage{mapCancsP(D)} 
\arguments{
  \item{D}{A data frame that includes site and histo3 as columns.  }
}

\value{The input data frame with an additional cancer column added on.}

\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}} }

\examples{
library(SEERaBomb)
mapCancsP # shows default definitions
}


\keyword{IO}
