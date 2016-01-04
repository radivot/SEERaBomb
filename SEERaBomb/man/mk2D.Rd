\name{mk2D}
\alias{mk2D}
\title{Make 2D-spline fits of incidences}
\description{Produces two dimensional (2D) spline 
fits of cancer incidence versus age and calendar year, with interactions. In conjunction with person years 
(PY) at risk, this 
is used in csd() to produce expected numbers of cases under a null hypothesis 
that prior cancers do not impact subsequent cancer risks.}
\usage{mk2D(seerSet, knots=5, write=FALSE, outDir="~/Results",txt=NULL,secondS=NULL)} 
\arguments{
  \item{seerSet}{Object of class seerSet, i.e. output list of seerSet(). }
  \item{knots}{Base number of knots; overrides are in place for some cancers.  }
  \item{write}{TRUE = write 2D fits to files. The fits can be >300 MB and take >60 seconds to write, 
  so leave FALSE unless you need it.}
  \item{outDir}{Folder that will hold the output files.}
  \item{txt}{Additional text to distinguish files with different cancer lists. This may be 
  useful during spline fit development. }
  \item{secondS}{Charcter vector of second cancers of interest (note: I often capitalize 
  the final S of vectors of Strings).}
}

\value{The input seerSet with an additional data frame D added to this list. 
D holds background/expected incidences over a 1-year resolution age-year grid. }

\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}, \link{plot2D}, \link{seerSet}} }

\examples{
\dontrun{
library(SEERaBomb)
(pm=simSeerSet())
(pm=mk2D(pm))
names(pm)
head(pm$D)
tail(pm$D)
}
}


\keyword{IO}
