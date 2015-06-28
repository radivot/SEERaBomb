\name{seerStats}
\alias{seerStats}
\title{Summarize SEER data prior to its entry into a seerSet object.}
\description{Compared to sex specific seerSet summaries using summary(), this renders info on both sexes together. }
\usage{seerStats(canc,popsa)} 
\arguments{
  \item{canc}{Data frame of cancers that includes agedx and cancer columns. }
  \item{popsa}{Data frame of population PY at 1-year age resolution.  }
}
   
\value{A list that includes: a data.frame with sexes pooled, of cases in each database and of cases >99 years old, for each cancer; a PY data.frame with sexes pooled of registries and PY weighted ages. }
  

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
