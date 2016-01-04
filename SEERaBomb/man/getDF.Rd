\name{getDF}
\alias{getDF}
\title{Converts a seerSet$L series to a data.frame}
\description{Creates a data.frame of observed and expected cases for each first and second cancer and treatment. 
csd() calls this internally for the most recent time series, so it may not need to be called directly.}
\usage{getDF(seerSet,srs=NULL)} 
\arguments{
  \item{seerSet}{seerSet object produced by csd(). }
  \item{srs}{Series. The time series of interest. NULL (default) implies the currently active series, which is
     the most recent. A number i implies the ith series. A string identifies the series by name (numeric vectors 
     will be coerced to such a string via paste0("b",paste(brks,collapse="_")) where brks = vector of time breakpoints.}
}
   
\value{A data.frame in long format that can be used by ggplot.  }

\note{I envision getting away from saving multiseries seerSet objects and instead just saving several DF outputs of getDF. 
Besides smaller objects, a reason for this is that two L objects out of csd can now be confounded if they have
the same time since diagnosis series but a different series for age and/or year of diagnosis. }
  

\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}, \link{csd}} }
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
firstS=c("NHL","MM")
pm=csd(pm,brkst=c(0,1,5),trts=c("rad","noRad"),firstS=firstS)
pm$DF 
getDF(pm)
}
}

\keyword{IO}
