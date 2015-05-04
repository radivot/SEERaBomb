\name{mapCancs}
\alias{mapCancs}
\title{Map ICD9 and ICD-O3 codes to cancers}
\description{ Adds a factor cancer with easily recognizable levels to a SEER cancer data.frame.}
\usage{mapCancs(D)} 
\arguments{
  \item{D}{A data frame that includes ICD9 and histo3 as columns.  }
}

\value{The input data frame with an additional cancer column added on.}

\note{This is used by mkSEER() when it generates R binaries of the SEER data. Otherwise it provides current cancer 
definitions (seen by looking at the function definition).}

\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}, \link{mapRegs}} }

\examples{
library(SEERaBomb)
mapCancs # shows default definitions
}


\keyword{IO}
