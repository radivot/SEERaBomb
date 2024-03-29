\name{seerSet2}
\alias{seerSet2}
\title{Join SEER cancers and PY}
\description{Creates a sex-specific list of cancer and population person year (PY) data frames, possibly specific 
to a race and interval of ages at diagnosis, without month of diagnosis. }
\usage{seerSet2(canc,popsa,Sex, Race="pooled",ageStart=15,ageEnd=85)} 
\arguments{
  \item{canc}{Data frame of cancers that includes agedx, sex, race, yrdx,surv and cancer. }
  \item{popsa}{Data frame of population PY at 1-year age resolution.  }
  \item{Sex}{"Male"  or "Female". }
  \item{Race}{"white", "black", "other",  or "pooled" (default).}
  \item{ageStart,ageEnd}{canc and popsa will be reduced to ages in ageEnd>age>=ageStart.}
}


\details{In the output: 1) 0.5 years is added to ages at diagnosis (agedx) 
  to reverse SEER flooring to integers; 2) 0.5 months is added to survival months (again, to reverse flooring) 
  before dividing by 12 to convert to years; 
3) year of diagnosis integers are converted to reals by adding to them 0.5 
If ageEnd>85, popsae (extended to ages up to 99) should be used as the input for popsa. 
If popsa is used, the age86 column of popsa will be replaced by an age column.
The age86 and yrbrth columns of a canc are not used and will be removed if they happen to be present; 
users should manually remove any other columns not needed to minimize seerSet object sizes.
Sex and race columns in inputs are removed from outputs as they are specified
in other (scalar) seerSet elements. Also removed from canc are cancer factor levels not present for that sex. 
}
   
\value{A list containing sex specific subsets of canc and popsa and information regarding how they were reduced. }
  

\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}, \link{mk2D}, \link{plot2D}} }
\examples{
\dontrun{ 
library(SEERaBomb)
simSeerSet() # without data, a simulated seerSet
#  else, with data ...
load("~/data/SEER/mrgd/cancDef.RData") #load in canc
load("~/data/SEER/mrgd/popsae.RData") # load in popsae
# trim columns
library(dplyr)  
canc=canc\%>\%select(-reg,-recno,-agerec,-numprims,-COD,
          -age19,-age86,-radiatn,-ICD9,-db,-histo3) 
popsae=popsae\%>\%select(-reg,-db)   
seerSet2(canc,popsae,Sex="male",ageStart=0,ageEnd=100) 
}
}

\keyword{IO}
