\name{riskVsAge}
\alias{riskVsAge}
\title{Second cancer risk vs attained age after first cancer}
\description{Computes absolute risk of 2nd cancers as a function of attained age after first cancer. }
\usage{riskVsAge(canc,firstS=c("NHL","HL","MM"),
                secondS=c("AML","MDS"),brksa=c(0,30,50,60,70,80,126)) }  
\arguments{
  \item{canc}{canc made by mkSEER().}
  \item{firstS}{Character vector of first cancers of interest.}
  \item{secondS}{Character vector of second cancers of interest.}
  \item{brksa}{Vector of breaks in years used to form attained age intervals.}
}
\value{data.frame with incidence vs age. }

\note{ This function was developed with support from the Cleveland Clinic Foundation.}
\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}, \link{mkSEER}} }

\keyword{IO}
