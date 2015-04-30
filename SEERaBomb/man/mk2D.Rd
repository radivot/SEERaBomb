\name{mk2D}
\alias{mk2D}
\title{Make two dimensional spline fits of cancer incidences}
\description{This function produces 2D spline fits to incidence versus age and calendar year, with interactions, and is useful for producing expected numbers of cases under a null hypothesis that prior cancers do not impact subsequent cancer risks.}
\usage{mk2D(seerSet, knots=5, write=FALSE, outDir="~/Results",txt=NULL,secondS=NULL)} 
\arguments{
  \item{seerSet}{Object of class seerSet, i.e. the list output of the function seerSet. }
  \item{knots}{The base number of knots to use. Overrides for some cancers will be hard-coded into this function.  }
  \item{write}{Set to TRUE if you want the list of 2D spline fit objects written to files. This list can be over 300 MB and
               take over 60 seconds to write, so leave false unless you need it.}
  \item{outDir}{folder that will hold the output files.}
  \item{txt}{Additional text to distinguish files with different cancer lists, 
  which should only occur during spline fit development. }
  \item{secondS}{Charcter vector of second cancers of interest.}
}

\value{Typically, the input seerSet object with an additional D data.frame added to it. The data.frame D includes expected  incidences on a 1-year resolution age-year grid. 
     }

\note{After the SEER data is installed, see the script mk2Dsplines.R in the examples folder. This function was developed with support from the Cleveland Clinic Foundation.}
\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}, \link{plot2D},\link{seerSet}} }
\keyword{IO}
