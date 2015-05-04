\name{mapRegs}
\alias{mapRegs}
\title{Map registry codes to acronyms}
\description{ Maps codes for SEER registries to 2-letter acronyms and 
               corresponding descriptions.}
\usage{mapRegs(code=NA)} 
\arguments{
  \item{code}{ Full SEER codes as found in SEER Cancer files. Add 1500 to popuation file codes get such cancer file codes. 
  If this argument is missing (the default) a full dataframe of symbols and descriptions is returned. }
}

\value{A dataframe of SEER registry symbols and descriptions with rownames such as "1501" for 
sf (san francisco) and "1520" for dM (detroit Michigan), or just the symbol if the rowname is given.
Note that city characters are in lower case and state characters are in upper case.}

\note{This function is used by mkSEER when it generates merged R binaries. It is exposed 
to provide quick access to registry acronym definitions.}


\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}, \link{mapCancs}} }
\examples{
library(SEERaBomb)
mapRegs(1501)
mapRegs()
}
\keyword{IO}
