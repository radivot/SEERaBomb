\name{getE}
\alias{getE}
\title{Get expected numbers of cases  }
\description{Converts list of matrices produced by post1PYO into a matrix that corresponds to the matrix of observed cases 
produced by post1PYO. }
\usage{getE(LPYM,D,ageStart,ageEnd,yearEnd,cancerS,secondS)} 
\arguments{
  \item{LPYM}{List of first cancer PY matrices produced by post1PYO.}
  \item{D}{D component of seerSet list produced by mk2D. }
  \item{ageStart}{ageStart component of seerSet. }
  \item{ageEnd}{ageEnd component of seerSet. }
  \item{yearEnd}{The most recent year in the SEER datasets. }
  \item{cancerS}{Character vector of cancers in canc of seerSet. }
  \item{secondS}{Character vector of second cancers of interest; added to seerSet by mk2D. }
}

\value{A matrix of expected cases with first cancers labeling the rows and second cancers labeling the columns.}

\examples{
library(SEERaBomb)
pm=simSeerSet()
pm=mk2D(pm,secondS="B") 
L=post1PYO(pm$canc,brks=c(0,2,5),binIndx=1,Trt="rad",yearEnd=2012 )
names(L)
names(pm)
with(pm,getE(L$LPYM,D,ageStart,ageEnd,yearEnd,cancerS,secondS))
}


\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}, \link{seerSet}} }
\keyword{IO}
