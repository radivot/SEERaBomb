\name{simSeerSet}
\alias{simSeerSet}
\title{Simulate SEER cancers and population person years}
\description{Simulates data for two cancers, A and B. }
\usage{simSeerSet(N=2e9,yearEnd=2012,ka=1e-5,kb=0.04,Ab=1e-5,
                 tauA=10,tauB=1,delay=1,period=4)} 
\arguments{
  \item{N}{Number of person years to simulate. Default is roughly that of SEER. }
  \item{yearEnd}{Most recent SEER year to simulate. }
  \item{ka}{Rate at which cancer A incidence increases linearly with age. }
  \item{kb}{Exponential aging rate constant for cancer B incidence. }
  \item{Ab}{Exponential amplitude for cancer B incidence. }
  \item{tauA}{Survival mean in years for cancer A. }
  \item{tauB}{Survival mean in years for cancer B. }
%  \item{Rba}{The risk of B given A. This constant is assumed to be independent of age and year. 
%  Latencies are assumed to follow a beta distribution between 0 and 5 years with a peak at just under 4 years.},
  \item{delay}{Years until the beginning of the excess risk of B. }
  \item{period}{Duration in Years of the excess risk of B. }
%  \item{shape}{Beta distribution shape paramaters are both set to this. Higher implies more peaked. },
}

\value{A simulated seerSet object with popsa filled using US 2000 Std population proportions and canc with 
cancers A and B
where the incidence of A increases linearly with age and B increase exponentially in age.
Survival times are assumed to be exponentially distributed with means of tauA years for A and tauB years for B.
Radiation therapy of A is assumed to increase RR of B to 5 uniformly for \code{period} years after \code{delay} years.}

\note{Supported by the Cleveland Clinic Foundation.}
\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}, \link{seerSet},\link{mk2D}, \link{plot2D}} }
\examples{
\dontrun{
library(SEERaBomb)
n=simSeerSet()
n=mk2D(n,secondS="B") 
mybrks=c(0,0.75,0.9,1.1,1.25,2,2.5,3,3.5,4,4.75,4.9,5.1,5.25,6)
n=tsd(n,brks=mybrks,trts=c("rad","noRad")) 
(lab=paste0("b",paste(mybrks,collapse="_")))
LM=n$L[[lab]]$'rad'
LMn=n$L[[lab]]$'noRad'
mkD<-function(LM) {
  t=LM$mids
  O=sapply(LM$Obs,sum)
  E=sapply(LM$Exp,sum)
  M=data.frame(O,E,t)
  mutate(M,RR=O/E,L=qchisq(.025,2*O)/(2*E),U=qchisq(.975,2*O+2)/(2*E))
}
D=rbind(cbind(mkD(LMn),rad="no"),cbind(mkD(LM),rad="yes"))
library(ggplot2)
theme_update(legend.position = c(.8, .815),
             axis.text=element_text(size=rel(1.2)),
             axis.title=element_text(size=rel(1.3)),
             legend.title=element_text(size=rel(1.2)),
             legend.text=element_text(size=rel(1.2)))
g=qplot(x=t,y=RR,col=rad,data=D,geom=c("line","point"),
        xlab="Years Since First Cancer Diagnosis",ylab="Relative Risk")
g+geom_abline(intercept=1, slope=0)+geom_errorbar(aes(ymin=L,ymax=U,width=.05))
}
}

\keyword{IO}
