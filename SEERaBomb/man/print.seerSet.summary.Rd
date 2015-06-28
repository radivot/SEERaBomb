\name{print.seerSet.summary}
\alias{print.seerSet.summary}
\title{Prints seerSet.summary objects}
\description{Renders data.frame of cases and median ages and
survival times with a title above and notes below it. Also qplots PY versus years.}
\usage{
\S3method{print}{seerSet.summary}(x, \dots)
}
%\usage{print.seerSet.summary(x, ...)} 
\arguments{
  \item{x}{seerSet.summary object produced by summary.seerSet(). }
  \item{...}{Included to match arg list of generic print. }
}

   
\value{None.  }
  

\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}, \link{summary.seerSet}, \link{seerSet}} }
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
summary(pm) # print method for summary renders the summary and a plot of PY
}
}

\keyword{IO}
