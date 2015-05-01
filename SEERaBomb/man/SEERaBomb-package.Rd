\name{SEERaBomb-package}
\alias{SEERaBomb-package}
\docType{package}
\title{ SEER and A-Bomb Data Tools and Analysis Examples }
\description{
SEERaBomb provides R functions that convert fixed-width format
SEER ASCII data into R binaries. This is depicted in the graph below which shows that \code{mkSEER}
merges all three of the SEER databases into one data.frame. 
\if{html}{\figure{mkSEERfig.png}}
\if{latex}{\figure{mkSEERfig.png}{options: width=5in}}
SEER data field positions and names change over the years and the original purpose of  
SEERaBomb was to buffer/protect R scripts from such changes. Since then it has grown a forte in 
estimating the kinetics of relative risks of SEER second cancers after diagnoses of first cancers.
}

\details{
\tabular{ll}{
Package: \tab \pkg{SEERaBomb}\cr
Type: \tab Package\cr
Depends: \tab LaF, RSQLite\cr
Suggests: \tab bbmle \cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
LazyData: \tab yes\cr
URL: \tab \url{http://epbi-radivot.cwru.edu/SEERaBomb/SEERaBomb.html}\cr
}
}


\note{The old version of mkSEER() merged cancer files but it didn't merge databases. 
To avoid breaking scripts that expect separate databases, that old version is still available, but it is now called mkSEERold. I thank the Cleveland Clinic Foundation for its support.}

\author{ Tom Radivoyevitch (\email{radivot@ccf.org}) }
\seealso{\code{\link{getFields},\link{pickFields},\link{mkSEER},\link{mkSEERold},\link{mkAbomb} }}


\keyword{package}
