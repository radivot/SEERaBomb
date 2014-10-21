\name{tsx}
\alias{tsx}
\title{Relative risk vs time-since-exposure}
\description{2D spline fitting results are used here to produce expected cases (E) with time (in years) since exposure (tsx), controlling for risk dedepence on 
age and calendar year under a null hypothesis that prior cancers do not impact subsequent cancer risks. 
In conjection with observed cases (O), relative risks (RR) are then formed as RR=O/E. }
\usage{tsx(seerSet,L2D, brks, bfn="",outDir="~/Results")} 
\arguments{
  \item{seerSet}{The data used.  }
  \item{L2D}{A list of 2D spline fits made by mk2D.  }
  \item{brks}{A vector of integer break years used to form time since exposure bins. }
  \item{bfn}{Base of file name: portion of name that preceeds breakpoint encoding. }
  \item{outDir}{folder that will hold the output file.}
}

\value{None. The output file contains a nested list of lists structure that for a given sex and race,
   yields second cancer risks as a function of tsx and different first cancers and first cancer therapies 
   (typically irradiated or not). Within canc, it is critical that users relevel cancer if needed, and 
   define a trt column, before using mk2D.
     }

\note{Examples are not possible without data. After the SEER data is installed, see the script mkRRtsx.R in the examples folder. This function was developed with support from the Cleveland Clinic Foundation.}
\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}, \link{mk2D},\link{seerSet}} }
\keyword{IO}
