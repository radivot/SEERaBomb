\name{mapRegs}
\alias{mapRegs}
\title{Map SEER registry codes to SEER registry symbols}
\description{ This function connects codes for registries to symbols and descriptions of what they are.}
\usage{mapRegs(code=NA)} 
\arguments{
  \item{code}{ Full SEER codes as found in SEER Cancer files. Add 1500 to popuation file codes get such cancer file codes. 
  If this argument is missing (the default) a full dataframe of symbols and descriptions is returned. }
}

\value{A dataframe of SEER registry symbols and descriptions with rownames such as "1501" for 
sf (san francisco) and "1520" for dM (detroit Michigan), or just the symbol if the rowname is given.
Note that city characters are in lower case and state characters are in upper case.}

\note{This function was developed with support from the Taussig Cancer Institute of the Cleveland Clinic Foundation.}
\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}, \link{mapCancs}} }
\examples{
library(SEERaBomb)
mapRegs(1501)
mapRegs()
}
\keyword{IO}
