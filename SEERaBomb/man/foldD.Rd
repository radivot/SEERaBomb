\name{foldD}
\alias{foldD}
\title{Fold Output D of Mortality Since Diagnosis function msd}
\description{Collapses output D of msd, computing new Excess Absoluted Risks (EAR) and Relative Risks (RR) as it folds the dataframe to a reduced size.  }
\usage{foldD(D,keep=c("int")) } 
\arguments{
  \item{D}{Output of msd hold time intervals and at least sex as a variable to fold over.}
  \item{keep}{Default is to keep time intervals (int) and fold variables like sex. }
}
\value{Tibble with new summed observed and expected cases, and new EAR and RR with CI for each interval. }
\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}, \link{msd},\link{mkMrtLocal}} }

\examples{
\dontrun{
library(SEERaBomb)
load("~/data/SEER/mrgd/cancDef.RData") #loads in canc
lu=canc\%>\%filter(cancer=="lung")
lu=lu\%>\%mutate(status=as.numeric(COD>0))\%>\%select(yrdx,agedx,sex,surv,status) 
lu=lu\%>\%mutate(surv=round((surv+0.5)/12,3))#convert surv to years
load("~/data/mrt/mrtUSA.RData")  #loads R binary created by mkMortLocal()
brks=c(0,0.5,3,6,10,15,20,25)    
(dlu=msd(lu,mrt,brkst=brks))
foldD(dlu)
}
}

\keyword{IO}
