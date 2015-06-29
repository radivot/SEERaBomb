\name{mkDF}
\alias{mkDF}
\title{Convert seerSet to data.frame for gg-plotting}
\description{Creates a data.frame of observed and expected cases for each first and second cancer and treatment. }
\usage{mkDF(seerSet)} 
\arguments{
  \item{seerSet}{seerSet object produced by tsd(). }
}
   
\value{A data.frame in long format that can be used by ggplot.  }
  

\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}, \link{mk2D}, \link{tsd}} }
\examples{
\dontrun{ 
library(SEERaBomb)
load("~/data/SEER/mrgd/cancDef.RData") #load in canc
load("~/data/SEER/mrgd/popsae.RData") # load in popsae
canc=canc\%>\%select(-reg,-recno,-agerec,-numprims,-COD,
          -age19,-age86,-radiatn,-ICD9,-db,-histo3) 
popsa=popsae\%>\%group_by(db,race,sex,age,year)\%>\%summarize(py=sum(py)) # sum on regs
pm=seerSet(canc,popsa,Sex="male",ageStart=0,ageEnd=100) #pooled (races) male seerSet
pm=mk2D(pm,secondS=c("AML","MDS"))
brks=c(0,1,5)
firstS=c("NHL","MM")
pm=tsd(pm,brks=brks,trts=c("rad","noRad"),firstS=firstS)
mkDF(pm)
}
}

\keyword{IO}
