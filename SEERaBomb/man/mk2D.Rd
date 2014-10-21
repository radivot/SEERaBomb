\name{mk2D}
\alias{mk2D}
\title{Make two dimensional spline fits of cancer incidences.}
\description{This function produces 2D spline fits to incidence versus age and calendar year, with interactions, that is useful for producing expected numbers of cases under a null hypothesis that prior cancers do not impact subsequent cancer risks.}
\usage{mk2D(seerSet, knots=10, write=FALSE, outDir="~/Results",txt=NULL)} 
\arguments{
  \item{seerSet}{Object of class seerSet, i.e. the list output of the function seerSet. }
  \item{knots}{The base number of knots to use. Overrides for some cancers will be hard-coded into this function.  }
  \item{write}{Set to TRUE if you want the objects produced written to files.}
  \item{outDir}{folder that will hold the output files.}
  \item{txt}{Additional text to distinguish files with different cancer lists, 
  which should only occur during spline fit development. }
}

\value{A large list of objects and the base name of the three files that contain them: 1)a large list of spline fits L2D (this ends in "L.RData"), the input seerSet (with filename ending in "S.RData"), and the returned dataframe D (in a file ending in "D.RData").   
   The base name encodes the race, sex, startAge and endAge. 
     If a descriptor of the cancer picks used also needs to be in the filename, use the txt argument. 
     The data.frame D includes expected  (based on L2D) incidences on a 1-year resolution age-year grid. 
     }

\note{Examples are not possible without data. After the SEER data is installed, see the script mk2Dsplines.R in the examples folder. This function was developed with support from the Cleveland Clinic Foundation.}
\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}, \link{plot2D},\link{seerSet}} }
\keyword{IO}
