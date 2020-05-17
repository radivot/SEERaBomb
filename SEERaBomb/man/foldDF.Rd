\name{foldDF}
\alias{foldDF}
\title{Fold DF Outputs of Cancer Since Diagnosis function csd}
\description{Collapses sex of row binded DF list elements of csd output computing new Relative Risks (RR).  }
\usage{foldDF(DF,keep=c("cancer1","cancer2","int","trt")) } 
\arguments{
  \item{DF}{Row bindings of csd DF with e.g. sex to collapse.}
  \item{keep}{Idea is to keep time intervals (int) and fold variables like sex. }
}
\value{Tibble with new summed observed and expected cases and new RR with CI for each interval. }
\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}, \link{csd} } }

\examples{ # nothing for now
}

\keyword{IO}
