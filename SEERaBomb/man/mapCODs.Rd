\name{mapCODs}
\alias{mapCODs}
\title{Map CODs to strings}
\description{ Maps integer cause of death (COD) codes in COD of a SEER cancer data frame 
to a factor CODS with recognizable levels. This is a bit slow, so it is called within mkSEER.}
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
