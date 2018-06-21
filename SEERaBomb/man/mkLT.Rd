\name{mkLT}
\alias{mkLT}
\title{Make Life Tables}
\description{Makes life tables from mortality data binaries produced by mkMrt() and places the files in the same folder.}
\usage{ mkLT(mrtHome="~/data/usMort",input="mrt.RData",output="ltb.RData")}

\arguments{
  \item{mrtHome}{ Directory that contains the mortality data binary.  }
  \item{input}{ File that contains the mortality data binary.  }
  \item{output}{ File that will contain the life tables.  }
}

\value{None. This function is called for its side-effect of producing male and female life tables in files.  }
\author{ Tom Radivoyevitch (\email{radivot@ccf.org}) }
\seealso{\code{\link{SEERaBomb-package},\link{mkSEER}} }
\examples{
\dontrun{
library(SEERaBomb)
mkLT()
load("~/data/usMort/ltb.RData")
tail(ltb$Female)
}
}
\keyword{IO}
