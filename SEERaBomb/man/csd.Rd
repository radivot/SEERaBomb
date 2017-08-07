\name{csd}
\alias{csd}
\title{Cancer risk vs years Since Diagnosis of other cancer}
\description{Computes relative risks (RR) of 2nd cancers over specified intervals of 
times since diagnoses of a 1st cancer. 2D spline fits are used to produce 
expected cases E controlling for background risk dedepence on 
age and calendar year.  RR is then O/E where O is the number of observed cases. }
\usage{csd(seerSet,brkst=c(0),brksy=c(1973),brksa=c(0),trts=NULL,
                    PYLong=FALSE,firstS="all",exclUnkSurv=FALSE) } % ,outDir="~/Results", txt=NULL)} 
\arguments{
  \item{seerSet}{A seerSet object produced by mk2D().  }
  \item{brkst}{Vector of breaks in years used to form Time intervals/bins since diagnosis. 
  An upper limit of 100, well beyond 40 years of SEER follow up currently available, 
              is assumed/added to brkst internally, and should thus not be in brkst. }
  \item{brksy}{Vector of breaks used to form groups of calendar Year at diagnosis intervals/bins.
               An upper limit of yearEnd (last year in SEER; a seerSet field) is assumed/added to brksy internally.}
  \item{brksa}{Vector of breaks used to form groups of Age at diagnosis intervals/bins. An upper limit of 126 is assumed. }
  \item{trts}{Character vector of treatments of interest. Default of NULL => all levels in seerSet's canc$trt. }
  \item{PYLong}{Set true if in addition to O and E for each tsd interval you also want PY strips
             for each individual; having these big dataframes slows saving seerSets, so only fetch if needed.}
  \item{firstS}{Character vector of first cancers of interest. Default of "all" sets it to the vector of all cancers
                in the seerSet field cancerS, which is created when the object is first created by seerSet().}
  \item{exclUnkSurv}{Set true if you wish to exclude all cases with unknown survival times as a marker of bad data.}
}

\value{The input with an L component added to it or extended it if it already existed. Each component of
L is a nested list of lists that can yield
second cancer relative risks as a function of time since 1st cancer diagnosis. The most recent
component of L is also provided as a data.frame seerSet$DF produced internally using getDF. 
     }

\note{ This function was developed with support from the Cleveland Clinic Foundation.}
\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}, \link{mk2D},\link{seerSet}} }

\examples{
\dontrun{
library(SEERaBomb)
pm=simSeerSet()
pm=mk2D(pm)
pm$canc
pm=csd(pm,brkst=c(0,5),brksy=c(1973,2000),brksa=c(0,50),trts=c("noRad","rad"))
pm
library(ggplot2)
theme_set(theme_gray(base_size = 16)) 
theme_update(legend.position = "top")
g=qplot(x=t,y=RR,data=subset(pm$DF,cancer1=="A"&cancer2=="B"),col=trt,geom=c("line","point"),
        xlab="Years Since First Cancer Diagnosis",ylab="Relative Risk")
g=g+facet_grid(yearG~ageG,scales="free")+geom_abline(intercept=1, slope=0) 
g+geom_errorbar(aes(ymin=rrL,ymax=rrU,width=.15))
}
}

\keyword{IO}
