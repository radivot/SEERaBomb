\name{plot2D}
\alias{plot2D}
\title{Plot 2D cancer incidence splines}
\description{Plots splines of incidence versus age and calendar year produced by mk2D. }
\usage{plot2D(seerSet, write=TRUE,outDir="~/Results/plots",col="red")} 
\arguments{
  \item{seerSet}{seerSet object after it is processed by mk2D.   }
  \item{write}{TRUE if you want to write images to a seerSet subfolder. 
  The name of this subfolder is the basefilename (bfn) of the seerSet. }
  \item{outDir}{Parent folder of seerSet subfolders.}
  \item{col}{Color of surface plot.}
}
\details{A plot will be produced for each cancer fitted by mk2D. For the first of these,
RGL will open a new X11 window.  Adjustments of size and angle of this 
first plot will hold for all subsequent plots. After each plot, 
the user hits any key to write the plot to a file and advance through the list of cancers.}

\value{None, results go to the screen and to png files.   }

\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}, \link{mk2D},\link{seerSet}} }

\examples{
\dontrun{
library(SEERaBomb)
n=simSeerSet()
n=mk2D(n) 
plot2D(n)
}
}


\keyword{IO}
