\name{getPY}
\alias{getPY}
\title{Gets PY contributions to a particular time since exposure interval from survivals}
\description{A vector survival times in years is the main input. This function gives their 
person year (PY) contributions to a particular interval.}
\usage{getPY(surv, bin, binS, brks)} 
\arguments{
  \item{surv}{The total survival time of the patient.  }
  \item{bin}{The label of the specific bin of interest.  }
  \item{binS}{The character vector of bin labels in which binLab exists.  }
  \item{brks}{The numeric vector of break points used by cut to create binS. }
}

\value{A vector as long as the survival vector input of PY at risk in a particular interval.}

\note{This function was developed with support from the Cleveland Clinic Foundation.}
\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}, \link{getBinInfo}} }
\examples{
library(SEERaBomb)
brks=c(0,0.25,1,3,6)  
(binS=levels(cut(brks+0.1,breaks=c(brks,100)))) #make a vector of intervals 
survTimes=c(8,16,1.5,3.7)
getPY(survTimes,binS[1],binS,brks)# all contribute 0.25 to first interval 
getPY(survTimes,binS[4],binS,brks)# 3rd and 4th survivals contribute 0 and 0.7 to (3,6]
getPY(survTimes,binS[5],binS,brks)# 1st and 2nd survival contribute 2 and 10 years to (6,100]
}
\keyword{IO}
