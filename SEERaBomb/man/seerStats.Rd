\name{seerStats}
\alias{seerStats}
\title{Summarize SEER data.}
\description{Shows numbers of cases per cancer in each SEER database and PY in each registry. Sexes are pooled. }
\usage{seerStats(canc,popsa)} 
\arguments{
  \item{canc}{Data frame of cancers that includes agedx and cancer columns. }
  \item{popsa}{Data frame of population PY at 1-year age resolution.  }
}
   
\value{A list of 2 data.frames with sexes pooled, one of cases with cancer types as rows and as columns, databases, totals, cases >=100 years old or not, and numbers of first-, second-, third- and higher cancers. The second data.frame holds population PY, and PY-weighted ages, per registry. }
  

\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}, \link{mk2D}, \link{plot2D}} }
\examples{
\dontrun{ 
library(SEERaBomb)
load("~/data/SEER/mrgd/cancDef.RData") #load in canc
load("~/data/SEER/mrgd/popsae.RData") # load in popsae
seerStats(canc,popsae)
}
}

\keyword{IO}
