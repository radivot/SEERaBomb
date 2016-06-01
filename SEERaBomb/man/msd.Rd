\name{msd}
\alias{msd}
\title{Mortality vs years Since Diagnosis}
\description{Computes relative risks (RR) of death over specified years-since-diagnosis intervals. 
US mortality rates obtained via the R package demography are used to compute background death dedepence on 
age and calendar year.  RR is then O/E where O and E are the number of observed and expected cases. }
\usage{msd(canc,mrt,brks=c(0,2,5),ybrks=NULL) } 
\arguments{
  \item{canc}{Input data.frame with columns: yrdx, agedx, sex, surv (in years), and status (1=dead).}
  \item{mrt}{List with male and female fields, each matrices with mortality rates vs year and age. }
  \item{brks}{Vector of breaks in years used to form years-since-diagnosis intervals/bins. }
  \item{ybrks}{Vector of breaks of calendar years to show mortality trends. Leave NULL for all in one. }
}
\value{data.frame with observed and expected cases, RR, and RR CI for each tsd interval. }
\note{ This function was developed with support from the Cleveland Clinic Foundation.}
\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}, \link{mk2D},\link{seerSet}} }

\examples{
\dontrun{
library(SEERaBomb)
load("~/data/SEER/mrgd/cancDef.RData") #loads in canc
lu=canc\%>\%filter(cancer=="lung")
lu=lu\%>\%mutate(status=as.numeric(COD>0))\%>\%select(yrdx,agedx,sex,surv,status) 
lu=lu\%>\%mutate(surv=round((surv+0.5)/12,3))#convert surv to years

# library(demography)
# d=hmd.mx("USA", "username", "password") #make an account and put your info in here
# mrt=d$rate
# save(mrt,file="~/data/usMort/mrt.RData")
load("~/data/usMort/mrt.RData"); object.size(mrt)# 250kb
brks=c(0,0.5,3,6,10,15,20,25)    
(dlu=msd(lu,mrt,brks))
}
}

\keyword{IO}
