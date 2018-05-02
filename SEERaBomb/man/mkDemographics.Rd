\name{mkDemographics}
\alias{mkDemographics}
\title{Make Demographics Tables}
\description{Provides, in an Excel file, quartiles of age at diagnoses  in one sheet and median overall survival times on a second. Many tables are placed in each sheet. One Excel file is produced per cancer type.}
\usage{mkDemographics(canc,outDir="~/Results/SEERaBomb")} 
\arguments{
  \item{canc}{A dataframe that includes cancer, age at diagnosis (agedx), age (grouped agedx), 
  race, sex, year (grouped), COD, surv, and trt.  }
  \item{outDir}{Folder of the Excel file(s) that will be generated.}
}

\value{ Returned invisibly is a list of data frames corresponding to tables of the Excel file(s).}

\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}} }

\examples{
\dontrun{
library(SEERaBomb)
rm(list=ls())
load("~/data/SEER/mrgd/cancDef.RData")
canc$year=cut(canc$yrdx,c(1973,2003,2009,2015),include.lowest = T,dig.lab=4)
canc$age=cut(canc$agedx,c(0,40,50,60,70,80,90,126),include.lowest = T)
canc=canc\%>\%filter(surv<9999)
canc=canc\%>\%select(-age86,-radiatn,-chemo,-db,-casenum,-modx,-seqnum,-yrbrth,-ICD9,-reg,-histo3)
canc=canc\%>\%filter(cancer\%in\%c("AML","MDS","MPN"))
head(canc,3)
mkDemographics(canc)
}
}

\keyword{IO}
