\name{tsd}
\alias{tsd}
\title{Compute RR vs tsd}
\description{Computes relative risks (RR) over specified times since diagnoses (tsd) of a 1st cancer. 2D spline fits are used to produce expected cases E controlling for background risk dedepence on 
age and calendar year.  RR is then O/E where O is the number of observed cases. }
\usage{tsd(seerSet,brks,trts=NULL,PYM=FALSE,firstS="all") } % ,outDir="~/Results", txt=NULL)} 
\arguments{
  \item{seerSet}{A seerSet object produced by mk2D().  }
  \item{brks}{Vector of breaks in years used to form tsd intervals/bins. }
  \item{trts}{Character vector of treatments of interest. Default of NULL => all levels in seerSet's canc$trt. }
  \item{PYM}{Set true if in addition to O and E for each tsd interval you also want PY strips
             for each individual; having these big dataframes slows saving seerSets, so only fetch if needed.}
  \item{firstS}{Character vector of first cancers of interest. Default of "all" sets it to the vector of all cancers
                in the seerSet field cancerS, which is created when the object is first created by seerSet().}
}

\value{The input with an L component added to it or extended if it already existed. Each component of
L is a nested list of lists that can yield
second cancer relative risks as a function of time since diagnosis and different first cancers and if they 
were irradiated or not. 
     }

\note{ This function was developed with support from the Cleveland Clinic Foundation.}
\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}, \link{mk2D},\link{seerSet}} }

\examples{
\dontrun{
library(SEERaBomb)
pm=simSeerSet()
pm=mk2D(pm) 
mybrks=c(0,1,5,10)
pm=tsd(pm,brks=mybrks,trts=c("noRad","rad"),PYM=TRUE)
(lab=paste0("b",paste(mybrks,collapse="_")))
LM=pm$L[[lab]]$'rad'
names(LM)
LM$PyM
LM$Obs
LM$Exp
table(LM$PyM$`(0,1]`$cancer2)
table(LM$PyM$`(1,5]`$cancer2)
table(LM$PyM$`(5,10]`$cancer2)
}
}

\keyword{IO}
