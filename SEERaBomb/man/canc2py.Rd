\name{canc2py}
\alias{canc2py}
\title{Converts canc made by mkSEER into a data.frame with py at risk after firsts}
\description{Makes a data frame in the exernal second cancer data format expected by esd.  }
\usage{canc2py(canc,firstS,secondS) } 
\arguments{
  \item{canc}{output of mkSEER}
  \item{firstS}{Charcter vector of first cancers of interest.}
  \item{secondS}{Charcter vector of second cancers of interest.}
}

\details{Use is currently limited to 2nd cancers in SEER since 1973, e.g. AML, since
esd assumes  its py column is in synch with times since diagnosis. This is not the 
case when e.g. MDS or CMML are in secondS. 
 }


\value{data.frame with columns: yrdx, agedx, sex, py at risk (in years), cancer1, and cancer2.
  Cases not ending in a second cancer have cancer2 set to "none". }
\note{This function was developed with support from the Cleveland Clinic Foundation.}
\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}, \link{esd},\link{msd}} }

\examples{
\dontrun{
rm(list=ls()) 
library(SEERaBomb)
load("~/data/SEER/mrgd/cancDef.RData") 
secS=c("AML","MDS")
frstS=c("HL")
canc=canc\%>\%filter(cancer\%in\%union(frstS,secS))
load("~/data/SEER/mrgd/popsae.RData")
popsa=popsae\%>\%group_by(db,race,sex,age,year)\%>\%summarize(py=sum(py)) # sum on regs
pm=seerSet(canc,popsa,Sex="male",ageStart=0,ageEnd=100) 
pf=seerSet(canc,popsa,Sex="female",ageStart=0,ageEnd=100) 
pm=mk2D(pm,secondS=secS) 
pf=mk2D(pf,secondS=secS)
brks=c(0,1,5,10)
pm=csd(pm,brkst=brks,firstS=frstS) 
pf=csd(pf,brkst=brks,firstS=frstS)
S=rbind(pf$DF,pm$DF)\%>\%select(int,t,cancer1,cancer2,O,E) #merge sexes
S=S\%>\%group_by(int,cancer1,cancer2)\%>\%summarize(O=sum(O),E=sum(E),t=mean(t))
S=S\%>\%mutate(RR=O/E,rrL=qchisq(.025,2*O)/(2*E),rrU=qchisq(.975,2*O+2)/(2*E))
S$data="SEER"

ds=canc2py(canc,frstS,secS)
S2=esd(ds,pf$D,pm$D,brks)
S2=S2\%>\%group_by(int,cancer1,cancer2)\%>\%summarize(O=sum(O),E=sum(E),t=mean(t))
S2=S2\%>\%mutate(RR=O/E,rrL=qchisq(.025,2*O)/(2*E),rrU=qchisq(.975,2*O+2)/(2*E))
S2$data="SEER2"
d=rbind(S,S2)
d\%>\%filter(cancer1=="HL",cancer2=="AML")\%>\%arrange(int)
d\%>\%filter(cancer1=="HL",cancer2=="MDS")\%>\%arrange(int) #low because MDS not observable before 2001
yrcut=2001  # try to fix it like this
ds=ds\%>\%filter(yeaR+py>yrcut)
I=ds$yeaR<yrcut
ds$py[I]=(ds$py-(yrcut-ds$yeaR))[I]
S2=esd(ds,pf$D,pm$D,brks)
S2=S2\%>\%group_by(int,cancer1,cancer2)\%>\%summarize(O=sum(O),E=sum(E),t=mean(t))
S2=S2\%>\%mutate(RR=O/E,rrL=qchisq(.025,2*O)/(2*E),rrU=qchisq(.975,2*O+2)/(2*E))
S2$data="SEER2"
d=rbind(S,S2)
d\%>\%filter(cancer1=="HL",cancer2=="MDS")\%>\%arrange(int) 
#the problem is that esd assumes that py is synched with tsx. csd handles the delays correctly.
d\%>\%filter(cancer1=="HL",cancer2=="MDS")\%>\%group_by(data)\%>\%summarize(o=sum(O)) #cases were shifted
}
}


\keyword{internal}
