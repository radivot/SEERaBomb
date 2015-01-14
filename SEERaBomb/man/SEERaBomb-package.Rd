\name{SEERaBomb-package}
\alias{SEERaBomb-package}
\docType{package}
\title{ SEER data setup and examples including analyses of A-Bomb Data }
\description{
This package strives to make SEER data more accessible from R. 
It provides functions that can be combined to convert fixed-width format
SEER ASCII text data files into R binaries. It also provides examples of the use of these binaries and examples that use the Japanese A-bomb survivor data. 
This package works only with the latest SEER release, now of April 15, 2014.
SEER data field positions and names can change over the years, and a goal of 
SEERaBomb is to buffer/protect R scripts from such changes.
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


The main function of this package, \code{mkSEER},
merges all three of the SEER databases. 
This is particularly important for studies of MDS because SEER began 
to track it only in 2001, so the 1973 and 2000 SEER databases 
include similar numbers of MDS cases.   
SEERaBomb is currently being developed to have new features 
for assessing the kinetics of risks of second cancers after treatments of first cancers.
 

\if{html}{\figure{mkSEERfig.png}}
\if{latex}{\figure{mkSEERfig.png}{options: width=5in}}

Our version of mkSEER is now mkSEERold; it pooled cancer files but it didn't pool databases.
}


\note{ This package was created with support from the National Cancer Institute and Tufts Integrative 
        Cancer Biology Program under U54CA149233-029689. 
        Its maintenance is being supported by the Cleveland Clinic Foundation.}

\author{ Tom Radivoyevitch (\email{radivot@ccf.org}) }
\seealso{\code{\link{getFields},\link{pickFields},\link{mkSEER},\link{mkSEERold} }}

\keyword{package}
