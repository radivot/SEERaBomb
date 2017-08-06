\name{mkDemographics}
\alias{mkDemographics}
\title{Make Demographics Tables in an Excel file}
\description{Provides median follow up for all cases combined and median ages for all cases and also many subgroups. Median overall survival times are provided on a second sheet. One file is produced per cancer type.}
\usage{mkDemographics(canc,outDir="~/Results/SEERaBomb")} 
\arguments{
  \item{canc}{A dataframe that includes cancer, age at diagnosis (agedx), age (grouped agedx), 
  race, sex, year (grouped), COD, surv, and trt.  }
  \item{outDir}{Folder of the Excel file that will be generated.}
}

\value{ Returned invisibly, a list of data frames corresponding to tables of the Excel file.}

\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}} }

\examples{
\dontrun{
library(SEERaBomb)
rm(list=ls())
load("~/data/SEER/mrgd/cancCRT.RData")
head(canc)
canc=canc\%>\%select(-age86,-radiatn,-db,-casenum,-modx,-seqnum,-yrbrth,-ICD9,-reg,-histo3)
head(canc)
%##canc$trt=as.character(canc$trt)
%##canc=canc\%>\%mutate(trt=ifelse(ct==1,paste0("chemo.",trt),trt))
canc$year=cut(canc$yrdx,c(1973,2003,2009,2014),include.lowest = T)
canc$age=cut(canc$agedx,c(0,40,50,60,70,80,90,126),include.lowest = T)
canc=canc\%>\%filter(surv<9999)\%>\%select(-ct,-yrdx)
canc=canc\%>\%filter(cancer\%in\%c("AML","MDS","MPN"))
}

}


\keyword{IO}
