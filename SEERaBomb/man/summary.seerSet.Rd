\name{summary.seerSet}
\alias{summary.seerSet}
\title{Summary of seerSet object}
\description{Creates a data.frame of cases and median ages and
survival times for each cancer and treatment type. }
\usage{
\S3method{summary}{seerSet}(object, \dots)
}
%\usage{summary.seerSet(object, ...)} 
\arguments{
  \item{object}{seerSet object produced by seerSet(). }
  \item{...}{Included to match arg list of generic summary. }
}

   
\value{A list that includes: a data.frame of cases, median ages at diagnosis, and
survival times, in years, for each cancer and treatment type; a data.frame of person-years by year; and 
smaller things such as a title, sex, race, and notes. The resulting list is set to class seerSet.summary 
which has a print method. }
  

\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}, \link{mk2D}, \link{plot2D}} }
\examples{
\dontrun{ 
library(SEERaBomb)
load("~/data/SEER/mrgd/cancDef.RData") #load in canc
load("~/data/SEER/mrgd/popsae.RData") # load in popsae
canc=canc\%>\%select(-reg,-recno,-agerec,-numprims,-COD,
          -age19,-age86,-radiatn,-ICD9,-db,-histo3) 
popsa=popsae\%>\%group_by(db,race,sex,age,year)\%>\%summarize(py=sum(py)) # sum on regs
pm=seerSet(canc,popsa,Sex="male",ageStart=0,ageEnd=100) #pooled (races) male seerSet
pm # no print method for seerSet object, so we see the list
(x=summary(pm)) # print renders summary and plot of PY
class(x)<-NULL #if you want to see the list as is, kill its class.
x    # It then goes through the regular print method for lists.
}
}

\keyword{IO}
