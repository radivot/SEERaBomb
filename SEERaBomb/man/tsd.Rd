\name{tsd}
\alias{tsd}
\title{Relative risk vs time-since-diagnosis}
\description{2D spline fitting results are used here to produce expected cases (E) with time (in years) since diagnosis (tsd) of the first cancer, controlling for risk dedepence on 
age and calendar year under a null hypothesis that prior cancers do not impact subsequent cancer risks. 
In conjunction with observed cases (O), relative risks (RR) can then be formed as RR=O/E. }
\usage{tsd(seerSet,brks,trts=NULL) } % ,outDir="~/Results", txt=NULL)} 
\arguments{
  \item{seerSet}{As made by mk2D.  }
  \item{brks}{A vector of integer break years used to form time since diagnosis bins. }
  \item{trts}{A character vector of treatments of interest. Default of NULL => all levels in canc$trt }
%  \item{outDir}{folder that will hold the output file.}
%    \item{txt}{If needed, additional text in file name to make it unique, e.g. for a modification in a
%               a cancer or treatment definition. }
}

\value{The input with an L component added to it, or extended as a longer list if it already existed. Each component of this
list L is a nested list of lists structure that for a given sex and race,
yields second cancer relative risks as a function of time since diagnosis and different first cancers and first cancer therapies 
(e.g. irradiated or not). 
     }

\note{Examples are not possible without data. After the SEER data is installed, see the script mkRRtsd.R in the examples folder. This function was developed with support from the Cleveland Clinic Foundation.}
\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}, \link{mk2D},\link{seerSet}} }
\keyword{IO}
