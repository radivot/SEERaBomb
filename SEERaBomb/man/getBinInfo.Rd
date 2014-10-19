\name{getBinInfo}
\alias{getBinInfo}
\title{Gets the lower and upper limit integers and vector position of a cut bin/interval}
\description{ This function may be useful in situations where you want to keep interval information (e.g. produced by cut) in 
strings but you also need to pull numeric attributes out of it at times.}
\usage{getBinInfo()} 
\arguments{
  \item{binLab}{The label of the specific bin of interest.  }
  \item{binS}{The character vector of bin labels in which binLab exists.  }
}

\value{A numeric vector containting the lower limit (LL), upper limit (UL), and position (index) in the parent vector binS.}

\note{This function was developed with support from the Cleveland Clinic Foundation.}
\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}, \link{mkSEER2}} }
\examples{
library(SEERaBomb)
brks=c(0,0.25,1,3,5)  
(binS=levels(cut(brks+0.1,breaks=c(brks,100)))) #make a vector of intervals 
getBinInfo(binS[4],binS) # test getBinInfo
}
\keyword{IO}
