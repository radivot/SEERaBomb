\name{mapCODs}
\alias{mapCODs}
\title{Maps cause of death (COD) integer codes to recognizable Strings in a factor CODS}
\description{ Adds a factor CODS with recognizable levels to a SEER cancer data frame. This function
is slow so it is called within mkSEER}
\usage{mapCODs(D)} 
\arguments{
  \item{D}{A data frame that includes COD as a column.  }
}

\value{The input data frame with an additional CODS column added on.}

\note{Typing mapCODs dumps the function definition and thus the mapping used.}

\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}, \link{mapRegs}} }

\examples{
library(SEERaBomb)
mapCODs # shows default definitions
}


\keyword{IO}
