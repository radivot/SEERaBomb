\name{mapCOD2}
\alias{mapCOD2}
\title{Map CODs to strings}
\description{ Maps integer cause of death (COD) codes in COD of a SEER cancer data frame 
to a factor CODS with recognizable levels. Adapted to work with SEER release of April 2022 }
\usage{mapCOD2(D)} 
\arguments{
  \item{D}{A data frame that includes COD as a column.  }
}

\value{The input data frame with an additional CODS column added on.}

\note{Typing mapCOD2 dumps the function definition and thus the mapping used.}

\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package} }}

\examples{
library(SEERaBomb)
mapCOD2 # shows default definitions
}


\keyword{IO}
