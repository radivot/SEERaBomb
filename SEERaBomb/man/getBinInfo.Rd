\name{getBinInfo}
\alias{getBinInfo}
\title{Gets the lower and upper limit and index of a tsd bin}
\description{ Extract time since diagnosis (tsd) interval information 
in strings produced by cut.}
\usage{getBinInfo(binLab, binS)} 
\arguments{
  \item{binLab}{The label of the specific bin of interest.  }
  \item{binS}{The character vector of bin labels in which binLab exists.  }
}

\value{A numeric vector containting the lower limit (LL), upper limit (UL), and position (index) in the parent vector binS.}

\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}} }
\examples{
library(SEERaBomb)
brks=c(0,0.25,1,3,5)  
(binS=levels(cut(brks+0.1,breaks=c(brks,100)))) #make a vector of intervals 
getBinInfo(binS[4],binS) # test getBinInfo
}
\keyword{internal}
