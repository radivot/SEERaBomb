\name{plot2D}
\alias{plot2D}
\title{Plot two-dimensional spline fits of cancer incidences.}
\description{This function allows checking of 2D spline fits to incidence versus age and calendar year produced by mk2D. The user
hits any key to advance through the list of cancers}
\usage{plot2D(L, outDir="~/Results/plots")} 
\arguments{
  \item{L}{list of objects produced by mk2D. Using a list input, and with() internally, protects against "no visible binding" notes
  on package checking.  }
  \item{outDir}{folder that will hold a subfolder of cancer pngs. The name of this subfolder is the basename f in L.}
}

\value{None, results go to png files.   }

\note{Examples are not possible without data. After the data is installed, see the script mk2Dsplines.R in the examples folder. This function was developed with support from the Cleveland Clinic Foundation.}
\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}, \link{mk2D},\link{seerSet}} }
\keyword{IO}
